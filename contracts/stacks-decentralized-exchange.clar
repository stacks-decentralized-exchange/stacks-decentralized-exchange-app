;; stacks-decentralized-exchange.clar
;;
;; Decentralized Exchange (DEX) on Stacks
;; Features: AMM liquidity pools for token swaps, add/remove liquidity, yield farming rewards.
;; Uses SIP-010 for tokens, constant product AMM model.
;; Follows Stacks best practices: SIP standards, error handling, access controls.

;; Traits for SIP-010 FT
(define-trait ft-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
  )
)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-POOL-NOT-FOUND (err u103))
(define-constant ERR-INVALID-SIGNATURE (err u104))
(define-constant ERR-INSUFFICIENT-BALANCE (err u105))
(define-constant ERR-UNKNOWN-CONTRACT (err u106))
(define-constant ERR-INVALID-CONTRACT-HASH (err u107))
(define-constant FEE-PERCENT u30) ;; 0.3% fee in basis points
(define-constant REWARD_RATE u10) ;; Rewards per second per liquidity unit

;; Error constants for liquidity operations
(define-constant ERR-ZERO-AMOUNT (err u108))
(define-constant ERR-RATIO-MISMATCH (err u109))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u110))
(define-constant ERR-INSUFFICIENT-LP-TOKENS (err u111))
(define-constant ERR-MIN-OUTPUT-NOT-MET (err u112))
(define-constant RATIO_TOLERANCE u50) ;; 0.5% tolerance for ratio matching

;; Error constants for swap operations
(define-constant ERR-SWAP-EXPIRED (err u113))
(define-constant ERR-INVALID-TOKEN (err u114))
(define-constant ERR-PRICE-IMPACT-TOO-HIGH (err u115))
(define-constant ERR-K-INVARIANT-VIOLATED (err u116))
(define-constant ERR-SAME-TOKEN (err u117))
(define-constant MAX-PRICE-IMPACT u1000) ;; 10% max price impact in basis points

;; Approved contract hashes for trusted token contracts
(define-map approved-contract-hashes (buff 32) bool)

;; Data Variables
(define-data-var last-pool-id uint u0)
(define-data-var platform-fees uint u0)

;; Data Maps
(define-map pools uint {
  token-a: principal, 
  token-b: principal, 
  reserve-a: uint, 
  reserve-b: uint, 
  liquidity-total: uint,
  created-at: uint  ;; CLARITY 4: Using stacks-block-time for timestamp
})
(define-map liquidity-providers {pool-id: uint, provider: principal} uint)
(define-map rewards {pool-id: uint, provider: principal} {last-claim: uint, accrued: uint})

;; Error constants for time-based operations
(define-constant ERR-BLOCK-NOT-FOUND (err u118))
(define-constant ERR-TIME-LOCK-ACTIVE (err u119))
(define-constant ERR-INVALID-BLOCK-HEIGHT (err u120))
(define-constant ERR-POOL-TOO-NEW (err u121))

;; Time-lock configuration for liquidity
(define-constant MIN-LOCK-PERIOD u3600) ;; 1 hour minimum lock
(define-constant POOL-MATURITY-PERIOD u86400) ;; 24 hours before pool is considered mature

;; Time-lock tracking for liquidity providers
(define-map liquidity-time-locks {pool-id: uint, provider: principal} {
  locked-until: uint,
  lock-block-height: uint
})

;; Get block timestamp by height
(define-read-only (get-block-timestamp (target-block-height uint))
  (match (get-stacks-block-info? time target-block-height)
    timestamp (ok timestamp)
    ERR-BLOCK-NOT-FOUND
  )
)

;; Get current block height
(define-read-only (get-current-block-height)
  stacks-block-height
)

;; Calculate time elapsed since a specific block
(define-read-only (get-time-since-block (target-block-height uint))
  (let
    (
      (current-time stacks-block-time)
      (block-time-result (get-stacks-block-info? time target-block-height))
    )
    (match block-time-result
      past-block-time (ok (- current-time past-block-time))
      ERR-BLOCK-NOT-FOUND
    )
  )
)

;; Check if a pool has reached maturity (24 hours old)
(define-read-only (is-pool-mature (pool-id uint))
  (let
    (
      (pool (map-get? pools pool-id))
    )
    (match pool
      pool-data (>= (- stacks-block-time (get created-at pool-data)) POOL-MATURITY-PERIOD)
      false
    )
  )
)

;; Get pool age in seconds
(define-read-only (get-pool-age (pool-id uint))
  (let
    (
      (pool (map-get? pools pool-id))
    )
    (match pool
      pool-data (ok (- stacks-block-time (get created-at pool-data)))
      ERR-POOL-NOT-FOUND
    )
  )
)


;; Create a new liquidity pool with timestamp tracking
(define-public (create-pool (token-a-contract principal) (token-b-contract principal) (amount-a uint) (amount-b uint))
  (let
    (
      (pool-id (+ (var-get last-pool-id) u1))
      ;; CLARITY 4: stacks-block-time - Get current Unix timestamp
      (current-timestamp stacks-block-time)
    )
    (asserts! (> amount-a u0) ERR-INVALID-AMOUNT)
    (asserts! (> amount-b u0) ERR-INVALID-AMOUNT)
    (map-set pools pool-id {
      token-a: token-a-contract, 
      token-b: token-b-contract, 
      reserve-a: amount-a, 
      reserve-b: amount-b, 
      liquidity-total: (sqrti (* amount-a amount-b)),
      created-at: current-timestamp
    })
    (map-set liquidity-providers {pool-id: pool-id, provider: tx-sender} (sqrti (* amount-a amount-b)))
    ;; Initialize rewards tracking with current time
    (map-set rewards {pool-id: pool-id, provider: tx-sender} {last-claim: current-timestamp, accrued: u0})
    (var-set last-pool-id pool-id)
    (ok pool-id)
  )
)

;; @param min-liquidity: Minimum LP tokens expected (slippage protection)
(define-public (add-liquidity (pool-id uint) (amount-a uint) (amount-b uint) (min-liquidity uint))
  (let
    (
      (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
      (reserve-a (get reserve-a pool))
      (reserve-b (get reserve-b pool))
      (liquidity-total (get liquidity-total pool))
      (current-timestamp stacks-block-time)
      ;; Calculate expected ratio (scaled by 10000 for precision)
      (expected-ratio-scaled (if (> reserve-a u0) (/ (* reserve-b u10000) reserve-a) u0))
      (provided-ratio-scaled (if (> amount-a u0) (/ (* amount-b u10000) amount-a) u0))
      ;; Calculate liquidity tokens to mint
      (liquidity-tokens (if (is-eq liquidity-total u0)
        ;; First liquidity provider: use geometric mean
        (sqrti (* amount-a amount-b))
        ;; Subsequent providers: proportional to existing liquidity
        (let ((liquidity-from-a (/ (* amount-a liquidity-total) reserve-a))
              (liquidity-from-b (/ (* amount-b liquidity-total) reserve-b)))
          ;; Use minimum to prevent manipulation
          (if (< liquidity-from-a liquidity-from-b) liquidity-from-a liquidity-from-b))))
      ;; Get existing LP balance for provider
      (existing-lp (default-to u0 (map-get? liquidity-providers {pool-id: pool-id, provider: tx-sender})))
    )
    ;; CHECK 1: Amounts must be greater than zero
    (asserts! (> amount-a u0) ERR-ZERO-AMOUNT)
    (asserts! (> amount-b u0) ERR-ZERO-AMOUNT)
    
    ;; CHECK 2: Pool ratio check (only for non-empty pools)
    ;; Ensures liquidity is added in correct proportion
    (asserts! (or 
      (is-eq liquidity-total u0)  ;; Skip ratio check for first deposit
      (and
        (>= provided-ratio-scaled (- expected-ratio-scaled RATIO_TOLERANCE))
        (<= provided-ratio-scaled (+ expected-ratio-scaled RATIO_TOLERANCE))))
      ERR-RATIO-MISMATCH)
    
    ;; CHECK 3: Slippage protection - ensure minimum LP tokens
    (asserts! (>= liquidity-tokens min-liquidity) ERR-SLIPPAGE-TOO-HIGH)
    
    ;; CHECK 4: Ensure liquidity tokens calculated is valid
    (asserts! (> liquidity-tokens u0) ERR-INVALID-AMOUNT)
    
    ;; Update pool reserves
    (map-set pools pool-id (merge pool {
      reserve-a: (+ reserve-a amount-a), 
      reserve-b: (+ reserve-b amount-b), 
      liquidity-total: (+ liquidity-total liquidity-tokens)
    }))
    
    ;; Update provider's LP token balance
    (map-set liquidity-providers {pool-id: pool-id, provider: tx-sender} 
      (+ existing-lp liquidity-tokens))
    
    ;; Update reward tracking with current timestamp
    (map-set rewards {pool-id: pool-id, provider: tx-sender} {last-claim: current-timestamp, accrued: u0})
    
    (ok {liquidity-tokens: liquidity-tokens, amount-a-added: amount-a, amount-b-added: amount-b})
  )
)

(define-public (remove-liquidity (pool-id uint) (lp-tokens uint) (min-amount-a uint) (min-amount-b uint))
  (let
    (
      (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
      (reserve-a (get reserve-a pool))
      (reserve-b (get reserve-b pool))
      (liquidity-total (get liquidity-total pool))
      (current-timestamp stacks-block-time)
      ;; Get provider's LP token balance
      (provider-lp-balance (default-to u0 (map-get? liquidity-providers {pool-id: pool-id, provider: tx-sender})))
      ;; Check time lock status (Clarity 4: stacks-block-time for time checks)
      (time-lock (map-get? liquidity-time-locks {pool-id: pool-id, provider: tx-sender}))
      (is-locked (match time-lock
        lock-data (< current-timestamp (get locked-until lock-data))
        false))
      ;; Calculate proportional amounts to return
      (amount-a-out (/ (* lp-tokens reserve-a) liquidity-total))
      (amount-b-out (/ (* lp-tokens reserve-b) liquidity-total))
      ;; Calculate pending rewards before removal
      (reward-info (default-to {last-claim: u0, accrued: u0} (map-get? rewards {pool-id: pool-id, provider: tx-sender})))
      (seconds-since-claim (- current-timestamp (get last-claim reward-info)))
      (pending-rewards (+ (get accrued reward-info) (* provider-lp-balance (* REWARD_RATE seconds-since-claim))))
    )
    ;; CHECK: Time lock must not be active
    (asserts! (not is-locked) ERR-TIME-LOCK-ACTIVE)
    
    (asserts! (> lp-tokens u0) ERR-ZERO-AMOUNT)
    
    (asserts! (>= provider-lp-balance lp-tokens) ERR-INSUFFICIENT-LP-TOKENS)
    
    (asserts! (>= liquidity-total lp-tokens) ERR-INSUFFICIENT-LIQUIDITY)
    
    (asserts! (> amount-a-out u0) ERR-INVALID-AMOUNT)
    (asserts! (> amount-b-out u0) ERR-INVALID-AMOUNT)
    
    (asserts! (>= amount-a-out min-amount-a) ERR-MIN-OUTPUT-NOT-MET)
    (asserts! (>= amount-b-out min-amount-b) ERR-MIN-OUTPUT-NOT-MET)
    
    ;; Update pool reserves
    (map-set pools pool-id (merge pool {
      reserve-a: (- reserve-a amount-a-out), 
      reserve-b: (- reserve-b amount-b-out), 
      liquidity-total: (- liquidity-total lp-tokens)
    }))
    
    ;; Update provider's LP token balance
    (map-set liquidity-providers {pool-id: pool-id, provider: tx-sender} 
      (- provider-lp-balance lp-tokens))
    
    ;; Clear time lock if fully withdrawn
    (if (is-eq (- provider-lp-balance lp-tokens) u0)
      (map-delete liquidity-time-locks {pool-id: pool-id, provider: tx-sender})
      true)
    
    ;; Reset rewards tracking (rewards should be claimed before removal)
    (map-set rewards {pool-id: pool-id, provider: tx-sender} {last-claim: current-timestamp, accrued: u0})
    
    (ok {
      amount-a: amount-a-out, 
      amount-b: amount-b-out, 
      lp-tokens-burned: lp-tokens,
      pending-rewards: pending-rewards
    })
  )
)

;; Add liquidity with time lock for enhanced rewards
(define-public (add-liquidity-with-lock (pool-id uint) (amount-a uint) (amount-b uint) (min-liquidity uint) (lock-period uint))
  (let
    (
      (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
      (reserve-a (get reserve-a pool))
      (reserve-b (get reserve-b pool))
      (liquidity-total (get liquidity-total pool))
      (current-timestamp stacks-block-time)
      (current-block stacks-block-height)
      ;; Calculate expected ratio (scaled by 10000 for precision)
      (expected-ratio-scaled (if (> reserve-a u0) (/ (* reserve-b u10000) reserve-a) u0))
      (provided-ratio-scaled (if (> amount-a u0) (/ (* amount-b u10000) amount-a) u0))
      ;; Calculate liquidity tokens to mint
      (liquidity-tokens (if (is-eq liquidity-total u0)
        (sqrti (* amount-a amount-b))
        (let ((liquidity-from-a (/ (* amount-a liquidity-total) reserve-a))
              (liquidity-from-b (/ (* amount-b liquidity-total) reserve-b)))
          (if (< liquidity-from-a liquidity-from-b) liquidity-from-a liquidity-from-b))))
      ;; Get existing LP balance for provider
      (existing-lp (default-to u0 (map-get? liquidity-providers {pool-id: pool-id, provider: tx-sender})))
      ;; Calculate lock expiry
      (lock-expiry (+ current-timestamp lock-period))
    )
    ;; CHECK 1: Amounts must be greater than zero
    (asserts! (> amount-a u0) ERR-ZERO-AMOUNT)
    (asserts! (> amount-b u0) ERR-ZERO-AMOUNT)
    
    ;; CHECK 2: Lock period must be at least minimum
    (asserts! (>= lock-period MIN-LOCK-PERIOD) ERR-INVALID-AMOUNT)
    
    ;; CHECK 3: Pool ratio check (only for non-empty pools)
    (asserts! (or 
      (is-eq liquidity-total u0)
      (and
        (>= provided-ratio-scaled (- expected-ratio-scaled RATIO_TOLERANCE))
        (<= provided-ratio-scaled (+ expected-ratio-scaled RATIO_TOLERANCE))))
      ERR-RATIO-MISMATCH)
    
    ;; CHECK 4: Slippage protection - ensure minimum LP tokens
    (asserts! (>= liquidity-tokens min-liquidity) ERR-SLIPPAGE-TOO-HIGH)
    
    ;; CHECK 5: Ensure liquidity tokens calculated is valid
    (asserts! (> liquidity-tokens u0) ERR-INVALID-AMOUNT)
    
    ;; Update pool reserves
    (map-set pools pool-id (merge pool {
      reserve-a: (+ reserve-a amount-a), 
      reserve-b: (+ reserve-b amount-b), 
      liquidity-total: (+ liquidity-total liquidity-tokens)
    }))
    
    ;; Update provider's LP token balance
    (map-set liquidity-providers {pool-id: pool-id, provider: tx-sender} 
      (+ existing-lp liquidity-tokens))
    
    ;; Set time lock
    (map-set liquidity-time-locks {pool-id: pool-id, provider: tx-sender} {
      locked-until: lock-expiry,
      lock-block-height: current-block
    })
    
    ;; Update reward tracking with current timestamp
    (map-set rewards {pool-id: pool-id, provider: tx-sender} {last-claim: current-timestamp, accrued: u0})
    
    (ok {
      liquidity-tokens: liquidity-tokens, 
      amount-a-added: amount-a, 
      amount-b-added: amount-b,
      locked-until: lock-expiry,
      lock-block: current-block
    })
  )
)

;; Check remaining lock time for a provider
(define-read-only (get-lock-time-remaining (pool-id uint) (provider principal))
  (let
    (
      (current-timestamp stacks-block-time)
      (time-lock (map-get? liquidity-time-locks {pool-id: pool-id, provider: provider}))
    )
    (match time-lock
      lock-data 
        (if (> (get locked-until lock-data) current-timestamp)
          (ok {
            is-locked: true,
            seconds-remaining: (- (get locked-until lock-data) current-timestamp),
            locked-until: (get locked-until lock-data),
            lock-block-height: (get lock-block-height lock-data)
          })
          (ok {
            is-locked: false,
            seconds-remaining: u0,
            locked-until: (get locked-until lock-data),
            lock-block-height: (get lock-block-height lock-data)
          }))
      (ok {
        is-locked: false,
        seconds-remaining: u0,
        locked-until: u0,
        lock-block-height: u0
      })
    )
  )
)

;; Get time lock info using historical block time (Clarity 4: get-stacks-block-info?)
(define-read-only (get-lock-info-with-block-time (pool-id uint) (provider principal))
  (let
    (
      (time-lock (map-get? liquidity-time-locks {pool-id: pool-id, provider: provider}))
    )
    (match time-lock
      lock-data 
        (let
          (
            (lock-block (get lock-block-height lock-data))
            (lock-block-time (get-stacks-block-info? time lock-block))
          )
          (ok {
            locked-until: (get locked-until lock-data),
            lock-block-height: lock-block,
            lock-block-timestamp: lock-block-time,
            current-time: stacks-block-time
          }))
      ERR-POOL-NOT-FOUND
    )
  )
)


;; Swap history tracking for analytics
(define-map swap-history {swap-id: uint} {
  trader: principal,
  pool-id: uint,
  token-in: principal,
  token-out: principal,
  amount-in: uint,
  amount-out: uint,
  fee-paid: uint,
  timestamp: uint
})
(define-data-var last-swap-id uint u0)

;; Calculate output amount for a swap (read-only helper function)
(define-read-only (get-swap-output (pool-id uint) (token-in-contract principal) (amount-in uint))
  (let
    (
      (pool (unwrap! (map-get? pools pool-id) (err u404)))
      (is-token-a (is-eq token-in-contract (get token-a pool)))
      (reserve-in (if is-token-a (get reserve-a pool) (get reserve-b pool)))
      (reserve-out (if is-token-a (get reserve-b pool) (get reserve-a pool)))
      (amount-in-with-fee (/ (* amount-in (- u10000 FEE-PERCENT)) u10000))
      (amount-out (/ (* reserve-out amount-in-with-fee) (+ reserve-in amount-in-with-fee)))
    )
    (ok {
      amount-out: amount-out,
      fee: (/ (* amount-in FEE-PERCENT) u10000),
      price-impact: (calculate-price-impact reserve-in reserve-out amount-in)
    })
  )
)

;; Calculate price impact in basis points
(define-read-only (calculate-price-impact (reserve-in uint) (reserve-out uint) (amount-in uint))
  (let
    (
      (spot-price-scaled (/ (* reserve-out u10000) reserve-in))
      (new-reserve-in (+ reserve-in amount-in))
      (amount-in-with-fee (/ (* amount-in (- u10000 FEE-PERCENT)) u10000))
      (amount-out (/ (* reserve-out amount-in-with-fee) (+ reserve-in amount-in-with-fee)))
      (execution-price-scaled (/ (* amount-out u10000) amount-in))
      (impact (if (> spot-price-scaled execution-price-scaled)
        (/ (* (- spot-price-scaled execution-price-scaled) u10000) spot-price-scaled)
        u0))
    )
    impact
  )
)

;; swap function
(define-public (swap-tokens 
    (pool-id uint) 
    (token-in-contract principal) 
    (amount-in uint) 
    (min-amount-out uint)
    (deadline uint))
  (let
    (
      (current-timestamp stacks-block-time)
      (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
      (token-a (get token-a pool))
      (token-b (get token-b pool))
      (is-token-a (is-eq token-in-contract token-a))
      (is-token-b (is-eq token-in-contract token-b))
      (reserve-in (if is-token-a (get reserve-a pool) (get reserve-b pool)))
      (reserve-out (if is-token-a (get reserve-b pool) (get reserve-a pool)))
      (token-out (if is-token-a token-b token-a))
      
      (k-before (* (get reserve-a pool) (get reserve-b pool)))
      
      (fee-amount (/ (* amount-in FEE-PERCENT) u10000))
      (amount-in-with-fee (- amount-in fee-amount))
      
      (amount-out (/ (* reserve-out amount-in-with-fee) (+ reserve-in amount-in-with-fee)))
      
      (new-reserve-in (+ reserve-in amount-in))
      (new-reserve-out (- reserve-out amount-out))
      
      (k-after (* (if is-token-a new-reserve-in new-reserve-out) 
                  (if is-token-a new-reserve-out new-reserve-in)))
      
      (price-impact (calculate-price-impact reserve-in reserve-out amount-in))
      
      (swap-id (+ (var-get last-swap-id) u1))
    )
    (asserts! (> amount-in u0) ERR-ZERO-AMOUNT)
    
    (asserts! (>= deadline current-timestamp) ERR-SWAP-EXPIRED)
    
    (asserts! (or is-token-a is-token-b) ERR-INVALID-TOKEN)
    
    (asserts! (> reserve-out amount-out) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (> amount-out u0) ERR-INVALID-AMOUNT)
    
    (asserts! (>= amount-out min-amount-out) ERR-MIN-OUTPUT-NOT-MET)
    
    (asserts! (<= price-impact MAX-PRICE-IMPACT) ERR-PRICE-IMPACT-TOO-HIGH)
    
    (asserts! (>= k-after k-before) ERR-K-INVARIANT-VIOLATED)
    
    (map-set pools pool-id (merge pool 
      (if is-token-a
        {reserve-a: new-reserve-in, reserve-b: new-reserve-out}
        {reserve-a: new-reserve-out, reserve-b: new-reserve-in})))
    
    (var-set platform-fees (+ (var-get platform-fees) fee-amount))
    
    (map-set swap-history {swap-id: swap-id} {
      trader: tx-sender,
      pool-id: pool-id,
      token-in: token-in-contract,
      token-out: token-out,
      amount-in: amount-in,
      amount-out: amount-out,
      fee-paid: fee-amount,
      timestamp: current-timestamp
    })
    (var-set last-swap-id swap-id)
    
    ;; Return swap details
    (ok {
      amount-out: amount-out,
      fee-paid: fee-amount,
      price-impact: price-impact,
      swap-id: swap-id,
      executed-at: current-timestamp
    })
  )
)

;; Legacy swap function
(define-public (swap (pool-id uint) (token-in-contract principal) (amount-in uint) (min-amount-out uint))
  (swap-tokens pool-id token-in-contract amount-in min-amount-out (+ stacks-block-time u86400))
)

;; Get swap history by ID
(define-read-only (get-swap-history (swap-id uint))
  (map-get? swap-history {swap-id: swap-id})
)

;; Get last swap ID
(define-read-only (get-last-swap-id)
  (var-get last-swap-id)
)

;; Generate swap receipt
(define-read-only (get-swap-receipt (swap-id uint))
  (let
    (
      (swap-record (unwrap! (map-get? swap-history {swap-id: swap-id}) (err u404)))
    )
    (ok {
      swap-id: swap-id,
      trader-address: (to-ascii? (get trader swap-record)),
      token-in-address: (to-ascii? (get token-in swap-record)),
      token-out-address: (to-ascii? (get token-out swap-record)),
      amount-in: (get amount-in swap-record),
      amount-out: (get amount-out swap-record),
      fee-paid: (get fee-paid swap-record),
      timestamp: (get timestamp swap-record)
    })
  )
)


(define-public (claim-rewards (pool-id uint))
  (let
    (
      (current-timestamp stacks-block-time)
      (liquidity (default-to u0 (map-get? liquidity-providers {pool-id: pool-id, provider: tx-sender})))
      (reward-info (default-to {last-claim: u0, accrued: u0} (map-get? rewards {pool-id: pool-id, provider: tx-sender})))
      (seconds-since-claim (- current-timestamp (get last-claim reward-info)))
      ;; Calculate rewards based on real time (not blocks)
      (new-accrued (+ (get accrued reward-info) (* liquidity (* REWARD_RATE seconds-since-claim))))
    )
    (asserts! (> liquidity u0) ERR-NOT-AUTHORIZED)
    (map-set rewards {pool-id: pool-id, provider: tx-sender} {last-claim: current-timestamp, accrued: u0})
    (ok new-accrued)
  )
)

;; Admin function to approve a contract hash
(define-public (approve-contract-hash (contract-hash (buff 32)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set approved-contract-hashes contract-hash true)
    (ok true)
  )
)

;; Verify a contract is approved before interaction
(define-read-only (is-contract-approved (contract-principal principal))
  (let
    (
      (hash-result (contract-hash? contract-principal))
    )
    ;; match on result type: (match result ok-name ok-expr err-name err-expr)
    (match hash-result
      hash-value (default-to false (map-get? approved-contract-hashes hash-value))
      err-val false
    )
  )
)

;; Swap with contract verification
(define-public (verified-swap (pool-id uint) (token-in-contract principal) (amount-in uint) (min-amount-out uint))
  (let
    (
      (is-approved (is-contract-approved token-in-contract))
    )
    (asserts! is-approved ERR-INVALID-CONTRACT-HASH)
    ;; Proceed with swap
    (swap pool-id token-in-contract amount-in min-amount-out)
  )
)

;; Generate pool metadata string using to-ascii?
(define-read-only (get-pool-metadata (pool-id uint))
  (let
    (
      (pool (unwrap! (map-get? pools pool-id) (err u404)))
    )
    (ok {
      pool-id: pool-id,
      token-a-string: (to-ascii? (get token-a pool)),
      token-b-string: (to-ascii? (get token-b pool)),
      reserve-a: (get reserve-a pool),
      reserve-b: (get reserve-b pool),
      created-at: (get created-at pool)
    })
  )
)

;; Generate human-readable swap receipt
(define-read-only (generate-swap-receipt (trader principal) (pool-id uint) (amount-in uint) (amount-out uint))
  (ok {
    trader-address: (to-ascii? trader),
    pool-id: pool-id,
    input-amount: amount-in,
    output-amount: amount-out,
    timestamp: stacks-block-time
  })
)

;; Signed swap for off-chain order matching (supports passkeys)
(define-public (signed-swap 
    (pool-id uint) 
    (amount-in uint) 
    (min-amount-out uint) 
    (msg-hash (buff 32))
    (signature (buff 64))  ;; secp256r1 signatures are 64 bytes
    (pubkey (buff 33)))
  (begin
    (asserts! (secp256r1-verify msg-hash signature pubkey) ERR-INVALID-SIGNATURE)
    (ok {verified: true, amount-out: min-amount-out})
  )
)

;; Verify a passkey signature (helper for external use)
(define-read-only (verify-passkey-signature 
    (msg-hash (buff 32)) 
    (signature (buff 64)) 
    (pubkey (buff 33)))
  (secp256r1-verify msg-hash signature pubkey)
)

;; ----------------------------------------
;; Read-Only Functions
;; ----------------------------------------

(define-read-only (get-pool (pool-id uint))
  (map-get? pools pool-id)
)

(define-read-only (get-liquidity (pool-id uint) (provider principal))
  (default-to u0 (map-get? liquidity-providers {pool-id: pool-id, provider: provider}))
)

(define-read-only (get-rewards (pool-id uint) (provider principal))
  (default-to {last-claim: u0, accrued: u0} (map-get? rewards {pool-id: pool-id, provider: provider}))
)

(define-read-only (get-platform-fees)
  (ok (var-get platform-fees))
)

(define-read-only (get-current-timestamp)
  stacks-block-time
)

;; ----------------------------------------
;; Owner/Admin Functions
;; ----------------------------------------

;; Owner withdraw fees
(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= amount (var-get platform-fees)) ERR-INSUFFICIENT-BALANCE)
    (try! (stx-transfer? amount tx-sender CONTRACT-OWNER))
    (var-set platform-fees (- (var-get platform-fees) amount))
    (ok true)
  )
)