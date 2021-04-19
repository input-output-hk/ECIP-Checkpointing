# Morpho example

In this directory you can find fully working self-contained example configurations for Morpho checkpointing. The topology consists of two local Morpho nodes connected to each other, both of which issue checkpointing RPC calls to a single local Mantis node which mines blocks on its own. The Mantis node is configured to have an average block mining time of 30 seconds.

There are three directories, `morpho-0`, `morpho-1` and `mantis`, each of which contains:
- A configuration file for the node
- A `run` script that runs the node
- After running, a `data` directory for the persistent data of the node

All three nodes need to be run at the same time to issue checkpoints. Run these commands in three different terminals:
```
$ example/morpho-0/run
$ example/morpho-1/run
$ example/mantis/run
```

In order to run the Mantis node, a compatible `mantis` binary needs to be available, which is the case if the user is in the Nix environment, as described in [the main readme](../README.md#development).

Mantis first needs to generate a DAG, which can take a couple minutes. After that it starts mining for blocks. When the 4th block is mined you should see a message as follows in the Mantis logs:
```
2021-04-23 14:39:32,111 [i.i.e.b.sync.regular.RegularSync] - Block mined [number = 4, hash = 45e92d0f497adaa3c2a78a2e54bcd98a7da2d7905cba03a6bb55c83224a218d7]
```

Soon after that, there should be some messages in the Morpho logs indicating that a new checkpoint is being created from votes, these should look like this for the first node to vote on a proof-of-work block:
```
[vario:morpho.rpc:Info:41] [2021-04-23 12:39:32.19 UTC] Starting RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x))
[vario:morpho.rpc:Info:41] [2021-04-23 12:39:32.20 UTC] Successful RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x)) returned: Just(PowBlock(number: 4, hash: 0x45e92d0f))
[vario:morpho.consensus.mempool:Info:41] [2021-04-23 12:39:32.20 UTC] Transaction Transaction(id: 0x393d9a18, tx: Vote(block: PowBlock(number: 4, hash: 0x45e92d0f), byPubkey: 0x290f0ab4)) added to mempool. Number of transactions in mempool goes from 0 to 1. Bytes of mempool goes from 0 to 2000
[vario:morpho.consensus.forge:Info:28] [2021-04-23 12:39:35.00 UTC] Forged block for slot 484315: MorphoBlock(hash: 0x19d8180b, prevHash: 0x007d3f2e, slot: 484315, blockNo: 118, body: MorphoBody(MorphoBlockTx(transaction: Vote(block: PowBlock(number: 4, hash: 0x45e92d0f), byPubkey: 0x290f0ab4), transactionId: 0x393d9a18)))
[vario:morpho.extract-state:Info:39] [2021-04-23 12:39:35.00 UTC] Current Ledger State: MorphoState(morphoTip: Block(slot: 484315, hash: 0x19d8180b), lastCheckpoint: Checkpoint(block: PowBlock(number: 0, hash: 0x), signedByPubkeys: []), checkpointAt: Genesis, currentVotes: [Vote(block: PowBlock(number: 4, hash: 0x45e92d0f), byPubkey: 0x290f0ab4)])
```

And like this for the second node to vote, which creates a checkpoint:
```
[vario:morpho.rpc:Info:41] [2021-04-23 12:39:35.40 UTC] Starting RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x))
[vario:morpho.rpc:Info:41] [2021-04-23 12:39:35.40 UTC] Successful RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x)) returned: Just(PowBlock(number: 4, hash: 0x45e92d0f))
[vario:morpho.consensus.mempool:Info:41] [2021-04-23 12:39:35.40 UTC] Transaction Transaction(id: 0xdc29f2ef, tx: Vote(block: PowBlock(number: 4, hash: 0x45e92d0f), byPubkey: 0x813abe58)) added to mempool. Number of transactions in mempool goes from 0 to 1. Bytes of mempool goes from 0 to 2000
[vario:morpho.consensus.forge:Info:28] [2021-04-23 12:39:40.00 UTC] Forged block for slot 484316: MorphoBlock(hash: 0x6bdfd8fc, prevHash: 0x19d8180b, slot: 484316, blockNo: 119, body: MorphoBody(MorphoBlockTx(transaction: Vote(block: PowBlock(number: 4, hash: 0x45e92d0f), byPubkey: 0x813abe58), transactionId: 0xdc29f2ef)))
[vario:morpho.extract-state:Info:39] [2021-04-23 12:39:40.00 UTC] Current Ledger State: MorphoState(morphoTip: Block(slot: 484316, hash: 0x6bdfd8fc), lastCheckpoint: Checkpoint(block: PowBlock(number: 4, hash: 0x45e92d0f), signedByPubkeys: [0x290f0ab4, 0x813abe58]), checkpointAt: Block(slot: 484316, hash: 0x6bdfd8fc), currentVotes: [])
```

About 30 seconds after the 4th block has been mined you should see the first checkpoint being pushed, which can be seen in the Mantis logs from:
```
2021-04-23 14:40:00,025 [i.i.e.b.sync.regular.RegularSync] - Received new checkpoint for block 45e92d0f497adaa3c2a78a2e54bcd98a7da2d7905cba03a6bb55c83224a218d7
```

And in the Morpho logs from:
```
[vario:morpho.rpc:Info:39] [2021-04-23 12:40:00.00 UTC] Starting RPC call PushCheckpoint( Checkpoint(block: PowBlock(number: 4, hash: 0x45e92d0f), signedByPubkeys: [0x290f0ab4, 0x813abe58]))
[vario:morpho.rpc:Info:39] [2021-04-23 12:40:00.02 UTC] Successful RPC call PushCheckpoint( Checkpoint(block: PowBlock(number: 4, hash: 0x45e92d0f), signedByPubkeys: [0x290f0ab4, 0x813abe58])) returned: True
```

Resetting can be done by deleting all `data` directories:
```
$ rm -rf example/*/data
```

