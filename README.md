# Stacks Decentralized Exchange

## Overview

**Stacks Decentralized Exchange** is a peer-to-peer trading platform built on the Stacks blockchain, utilizing Automated Market Maker (AMM) models for liquidity pools. It enables seamless crypto swaps, reduces custodial risks by being fully decentralized, and supports yield farming to incentivize liquidity providers. Tapping into the $55B+ monthly DeFi volume, this DEX focuses on secure, efficient trading with low fees.

Inspired by leading DEXes like Uniswap, it uses constant product AMM for fair pricing and incorporates Stacks-specific optimizations. Potential earnings come from 0.3% trading fees (accrued in STX) and liquidity provider rewards. The core logic is implemented in Clarity smart contracts, leveraging Clarity 4 features for advanced functionality like time-based rewards and signed orders.

Key goals:

- Provide a trustless platform for token swaps and liquidity provision.
- Enable yield farming with on-chain rewards to boost participation.
- Ensure security and transparency through immutable blockchain records.

This repository contains the smart contract, tests, deployment scripts, and documentation for Stacks Decentralized Exchange.

## Features

- **AMM Liquidity Pools**: Create pools for any SIP-010 compliant token pairs, with constant product formula for pricing.
- **Add/Remove Liquidity**: Provide liquidity to earn fees and farming rewards; remove with proportional shares.
- **Token Swaps**: Execute swaps with slippage protection (min-amount-out) and 0.3% fees.
- **Yield Farming**: Claim time-based rewards proportional to liquidity provided, using Clarity 4's `get-block-time`.
- **Signed Orders (Clarity 4)**: Advanced swaps with off-chain signatures verified via `secp256r1-verify`.
- **Fee Management**: Platform fees accumulated and withdrawable by owner.
- **SIP-010 Integration**: Compatible with fungible tokens for broad asset support.
- **Access Controls**: Owner-only functions; robust error handling for invalid operations.
- **Testing Suite**: Unit tests in TypeScript covering pool creation, swaps, liquidity ops, signatures, and edges.
- **Deployment Ready**: Scripts for Devnet, Testnet, and Mainnet via Clarinet.
- **Extensibility**: Designed for multi-hop swaps, oracles, and governance add-ons.

## Project Structure

```
stacks-decentralized-exchange-app/
├── Clarinet.toml               # Project configuration
├── contracts/
│   └── stacks-decentralized-exchange.clar  # Main DEX smart contract
├── tests/
│   └── stacks-decentralized-exchange.test.ts  # Unit tests
├── deployments/
│   ├── default.devnet-plan.yaml   # Devnet plan (YAML)
│   ├── default.testnet-plan.yaml  # Testnet plan
│   └── default.mainnet-plan.yaml  # Mainnet plan
├── deploy.sh                   # Deployment script
├── vitest.config.js            # Vitest config
├── package.json                # Node dependencies (@hirosystems/clarinet-sdk)
├── settings/                   # Clarinet network settings
└── README.md                   # This file
```

### Contract Architecture

```
+-------------------+
|                   |
|   Stacks DEX      |
|   (Main Contract) |
|                   |
+-------------------+
          ^
          |
   SIP-010 FT Trait
          |
   Core Functions:
   - Create Pool
   - Add/Remove Liquidity
   - Swap Tokens
   - Claim Rewards
   - Signed Swap
   - Read-Only Queries
   - Fee Withdrawal (Private)
```

- **stacks-decentralized-exchange.clar**: Handles pools, swaps, liquidity, rewards, and fees. Key functions: `create-pool`, `add-liquidity`, `swap`, `claim-rewards`, `signed-swap`.

## Prerequisites

- [Clarinet](https://docs.stacks.co/reference/clarinet/cli-reference) installed: `cargo install --git https://github.com/hirosystems/clarinet.git --locked clarinet`.
- Node.js/npm for tests with Vitest and Clarinet JS SDK.
- Stacks wallet (e.g., Hiro Wallet) for interactions.
- Familiarity with Clarity, Stacks, SIP-010, and AMM concepts.

## Installation

1. Clone the repo:

   ```
   git clone https://github.com/your-org/stacks-decentralized-exchange-app.git
   cd stacks-decentralized-exchange-app
   ```

2. Install dependencies:

   ```
   npm install
   ```

3. Check integrity:
   ```
   clarinet check
   ```

## Usage

### Local Development

1. Launch console:

   ```
   clarinet console
   ```

   Examples:

   - Create pool: `(contract-call? .stacks-decentralized-exchange create-pool .token-a .token-b u1000 u1000)`
   - Add liquidity: `(contract-call? .stacks-decentralized-exchange add-liquidity u1 .token-a u500 u500)`
   - Swap: `(contract-call? .stacks-decentralized-exchange swap u1 .token-in u100 u90)`
   - Claim rewards: `(contract-call? .stacks-decentralized-exchange claim-rewards u1)`
   - Signed swap: `(contract-call? .stacks-decentralized-exchange signed-swap u1 u100 u90 0x... 0x... 0x...)`
   - Get pool: `(contract-call? .stacks-decentralized-exchange get-pool u1)`

2. Test scenarios like liquidity addition and swaps.

### Testing

Run tests:

```
npm run test
```

Or:

```
npx vitest
```

Covers: Pool ops, swaps with fees, reward accrual, signatures, errors.

## Deployment

### Local Devnet

```
clarinet devnet start
```

Deploy via console/plans.

### Testnet/Mainnet

1. Update settings with keys.

2. Generate plan:

   ```
   clarinet deployments generate --testnet
   ```

3. Apply:
   ```
   clarinet deployments apply --testnet
   ```

Automate with `deploy.sh`. Audit prior to Mainnet.

## Security Considerations

- **Access Controls**: Owner withdrawals; asserts prevent unauthorized actions.
- **Error Handling**: Custom errors for liquidity, amounts, pools, signatures.
- **Best Practices**: SIP-010 traits, no reentrancy, fee deductions. Uses Clarity 4 for secure timestamps/signatures.
- **Recommendations**:
  - Audit (Certik/Stacks partners).
  - Hardware wallets.
  - Monitor flash loans/front-running.
  - Impermanent loss warnings for LPs.

## Future Extensions

- Multi-hop routing.
- External oracles (e.g., Chainlink on Stacks).
- Governance for fee adjustments.
- Frontend (React + @stacks/connect).
- Cross-chain bridges.
- Advanced analytics.

## Contributing

Fork, branch, commit, push, PR. Use issues for discussions.

## License

MIT - see [LICENSE](LICENSE).

## Acknowledgments

- Clarinet/Stacks (Hiro Systems).
- Stacks community for SIPs/Clarity 4.
- Inspired by Uniswap AMM models.

Contact us or open an issue. Trade decentralized! 📈
