# Morpho cryptography

Morpho uses cryptography in multiple ways

## OBFT-related

### OBFT Block hashing

Since Morpho uses an OBFT blockchain underneath, we need a hash function for the OBFT blocks that Morpho uses to ensure validity of chains. Note the hash function used for this has no influence outside of the Morpho network.

| :warning: Currently hardcoded to insecure hash |
| --- |

For easy debugging, at the moment the hash function is hardcoded to an **8-char prefix of MD5**. This would be a terrible idea to use in production, so please change this before thinking of doing that!

In order to fix this, change `MorphoMockHash` in the [entrypoint `run` function](../../morpho-checkpoint-node/src/Morpho/Node/Run.hs#L60-L61) to a cryptographically secure hash type from [`cardano-crypto-class` hash suite](https://github.com/input-output-hk/cardano-base/tree/master/cardano-crypto-class/src/Cardano/Crypto/Hash), and follow the compilation errors.

### OBFT signing

In order for OBFT to provide consensus, it relies on cryptographic signatures. As with the OBFT block hash, the signature scheme used has no influence outside of the Morpho federation.

| :warning: Currently hardcoded to insecure signatures |
| --- |

For easy debugging, at the moment the signature algorithm is mocked, meaning it can be trivially forged. This would be a terrible idea to use in production, so please change this before thinking of doing that!

In order to fix this, change `ConsensusMockCrypto` in the [entrypoint `run` function](../../morpho-checkpoint-node/src/Morpho/Node/Run.hs#L60-L61) to a cryptographically secure signature scheme from [`cardano-crypto-class` signature suite](https://github.com/input-output-hk/cardano-base/tree/master/cardano-crypto-class/src/Cardano/Crypto/DSIGN), and follow the compilation errors.

## Vote signing

Entirely separate from OBFT signing is signing of Morpho votes. As each Morpho node in the federation calls the [`checkpointing_getLatestBlock`](../references/rpc.md#method-checkpointing_getlatestblock) RPC method, the received proof-of-work blocks are signed with the node's private key and published as transactions to all other federation nodes. These transactions are then collected by the current slot leader, in order to create checkpoints, an accumulation of signatures for the same proof-of-work block. These checkpoints are then pushed using the [`checkpointing_pushCheckpoint`](../references/rpc.md#method-checkpointing_pushcheckpoint) RPC call. Notably the signature scheme for votes directly influences the RPC interface, in contrast to the internal OBFT cryptography in the above section.

The signature scheme for this is hardcoded to be ECDSA with the secp256k1 elliptic curve, as implemented by the [secp256k1](https://github.com/bitcoin-core/secp256k1) C library, which is also used by Bitcoin.

Using the standard ECDSA algorithm as described in [RFC6979](https://tools.ietf.org/html/rfc6979) gives rise to two 256-bit values, `r` and `s`, comprising the signature. ECDSA allows a neat mechanism called public key recovery, which allows you to _almost_ [recover the public key](https://crypto.stackexchange.com/questions/18105/how-does-recovering-the-public-key-from-an-ecdsa-signature-work) from `r`, `s` and the signed bytes. Only almost because this process actually generates 2 public keys, not just one. So in order to distinguish between the first and the second public key, [a single additional bit](https://bitcoin.stackexchange.com/questions/38351/ecdsa-v-r-s-what-is-v) `v`, either 0 or 1, needs to be produced during signing. [Historically however](https://bitcoin.stackexchange.com/questions/38351/ecdsa-v-r-s-what-is-v#comment45089_38351), Bitcoin has used values 27 and 28 instead, so this is used for Morpho too, in order to be able to reuse the same scheme. The reason signature recovery is done is because it's more efficient for verification, as we don't need to attempt a verify operation with all known public keys, and the public key doesn't take up any bandwidth either.

So this provides us with two cryptographic primitives:
- Signing: `sign(data, privkey) = (r, s, v = 27|28)`, where `data` is a 256-bit value, which just fits for the proof-of-work block hashes. Any differently-sized data would have to be hashed to 256-bits first. `r` and `s` are also 256-bit values.
- Public key recovery: `recover(data, r, s, v) = pubkey`, where `pubkey` matches the `privkey` it was encoded with, and `data` matches the `data` that was signed.

Verifying whether the signature is valid is then only a matter of recovering the public key and checking whether we trust it.

This is implemented in [ECDSASignature.hs](../../morpho-checkpoint-node/src/Morpho/Crypto/ECDSASignature.hs), which just defers to the [secp256k1-haskell](https://github.com/haskoin/secp256k1-haskell) library, which provides bindings to the [secp256k1](https://github.com/bitcoin-core/secp256k1) C library. Note that latest upstream secp256k1-haskell [doesn't support signature recovery anymore](https://github.com/haskoin/secp256k1-haskell/commit/0f862c2198d3ae47ad9028947f7507321079ab98), so we use version 0.2.4 instead.

The trusted public keys are specified in the config file with [`FedPubKeys`](../references/configuration.md#fedpubkeys), while a node's private key is specified using [`NodePrivKeyFile`](../references/configuration.md#nodeprivkeyfile).
