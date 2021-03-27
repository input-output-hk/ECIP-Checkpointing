# Morpho implementation

This document gives an overview of how Morpho is implemented.

At its core, Morpho runs the OBFT consensus algorithm implemented in [ouroboros-network](https://github.com/input-output-hk/ouroboros-network) among all federation nodes. This library provides the underlying mechanism for storing and synchronizing historical shared state among all nodes. While OBFT achieves this by running a blockchain, Morpho doesn't necessarily rely on it being a blockchain.

## Ledger state data structures

[(source)](../../morpho-checkpoint-node/src/Morpho/Ledger/State.hs#L41-L46)

The ledger state is the state for a given OBFT chain after all OBFT blocks have been applied in sequence according to the [ledger state transitions](#ledger-state-transitions). Its fields are:
- `morphoTip`: Points to the latest OBFT block that was applied to the state, doesn't include transactions in the mempool
- `lastCheckpoint`: Contains the last checkpoint created, which stores the proof-of-work block that was voted for with the signatures of a majority of nodes
- `checkpointAt`: Points to the OBFT block that created the checkpoint in `lastCheckpoint`. This allows Morpho to distinguish between older checkpoints and ones just created, used to determine whether it should be [pushed](#pushing-new-checkpoints) or not
- `currentVotes`: For each federation node, this stores either
  - No vote if they haven't cast a vote since the last checkpoint was created
  - The latest vote they cast since the checkpoint was created

### Checkpoint

[(source)](../../morpho-checkpoint-node/src/Morpho/Ledger/PowTypes.hs#L48-L53)

A checkpoint as stored in the `lastCheckpoint` field of the [ledger state](#ledger-state-data-structures) contains the following fields:
- `checkpointedBlock`: The proof-of-work block that was checkpointed
- `chkpSignatures`: A list of signatures from a majority of nodes (counting [`RequiredMajority`](../references/configuration.md#requiredmajority)). See the [crypto document](../explanations/crypto.md#vote-signing) for the components of these signatures.

### Vote

[(source)](../../morpho-checkpoint-node/src/Morpho/Ledger/PowTypes.hs#L41-L46)

A vote as stored in `currentVotes` field of the [ledger state](#ledger-state-data-structures) contains the following fields:
- `votedPowBlock`: The proof-of-work block that a node voted for as the next block to be checkpointed
- `voteSignature`: A single signature by the node that cast the vote. See the [crypto document](../explanations/crypto.md#vote-signing) for the components of such signatures.

## Ledger state transitions

[(source)](../../morpho-checkpoint-node/src/Morpho/Ledger/Update.hs#L251-L281)

In OBFT, time is divided into slots, whose duration is controlled with [`SlotDuration`](../references/configuration.md#slotduration). At every slot, the node whose [`NodeId`](../references/configuration.md#nodeid) equals the slot number modulo [`NumCoreNodes`](../references/configuration.md#numcorenodes) is the leader. This node collects transactions from all the other nodes into a mempool, then forges an OBFT block from them, which is then published to all the other nodes. In Morpho's case, a transaction is equivalent to a [vote](#vote), and an OBFT block consists of an arbitrary number of transactions/votes.

As new OBFT blocks are forged, the [ledger state](#ledger-state-data-structures) changes as follows:
- `morphoTip` is updated to the new OBFT block's tip. This ensures that when [Pushing a checkpoint to Mantis](#mantis-pushing), the same checkpoint isn't pushed multiple times
- All the blocks transactions/votes are inserted into `currentVotes`, overriding previous votes by the same public key
- If more than or equal to [`RequiredMajority`](../references/configuration.md#requiredmajority) number of `currentVotes` vote for the same `votedPowBlock`:
  - Set `lastCheckpoint` to a new [checkpoint](#checkpoint) containing the `votedPowBlock` along with all the signatures that voted for that block
  - Set `checkpointAt` to the new value of `morphoTip`, the new OBFT block's tip
  - Set `currentVotes` to an empty mapping, clearing all votes.

### Transaction validation

[(source)](../../morpho-checkpoint-node/src/Morpho/Ledger/Update.hs#L283-L318)

Before transactions/votes are included in the OBFT mempool or chain, they are validated with the following criteria:
- The `votedPowBlock` needs to have a higher block number than the `checkpointedBlock` of the `lastCheckpoint` of the latest [ledger state](#ledger-state-data-structures).
- The `votedPowBlock` needs to have a distance to the latest `checkpointedBlock` that is divisible by [`CheckpointInterval`](../references/configuration.md#checkpointinterval).
- The vote needs to not exist in the ledger states `currentVotes` already
- The public key of the `voteSignature` needs to be in [`FedPubKeys`](../references/configuration.md#fedpubkeys)

## Interactions with the outside world

### Pulling for checkpoint candidates

[(source)](../../morpho-checkpoint-node/src/Morpho/Node/Run.hs#L243-L267)

In a regular interval, as specified by [`PoWBlockFetchInterval`](../references/configuration.md#powblockfetchinterval), the [`checkpointing_getLatestBlock`](../references/rpc.md#method-checkpointing_getlatestblock) RPC call is invoked. The first parameter of the call is set to [`CheckpointInterval`](../references/configuration.md#checkpointinterval), while the second parameter is set to the `lastCheckpoint` field of the [ledger state](#ledger-state-data-structures) of current OBFT chain.

If the RPC call returned no result, nothing is done.

If the RPC call returned a result, a [vote](#vote) is created with `votedPowBlock` being the received block and signing its hash to create the `voteSignature`. This vote is then broadcast to all OBFT nodes in the federation, to be included in one of the next OBFT blocks.

### Pushing new checkpoints

[(source)](../../morpho-checkpoint-node/src/Morpho/Node/Run.hs#L269-L303)

Every time that the OBFT chain gets extended with a new block:
- Get the latest _stable_ ledger state by going back [`StableLedgerDepth`](../references/configuration.md#stableledgerdepth) OBFT blocks [(source)](../../morpho-checkpoint-node/src/Morpho/Ledger/SnapshotTimeTravel.hs#L33-L51)
- Determine if the checkpoint for that stable ledger state should be pushed by checking whether `checkpointAt` equals `morphoTip`, and `morphoTip` isn't genesis. If this is the case, it indicates that this is the first OBFT block with this checkpoint and it should be pushed. If that's not the case, nothing is done.
- To push the checkpoint, the [`checkpointing_pushCheckpoint`](../references/rpc.md#method-checkpointing_pushcheckpoint) RPC call is invoked with the stable ledger state's `lastCheckpoint`, with the first parameter set to the `checkpointedBlock`'s hash, and the second parameter set to `chkpSignatures`.


## End-to-end process

In this section we describe the entire end-to-end process, from mining a proof-of-work block to it being checkpointed by a Morpho federation. Our initial state is a popular proof-of-work network that just started mining the first block. In addition we have a new checkpointing federation with parameters
- [`NumCoreNodes`](../references/configuration.md#numcorenodes): 3
- [`NodeId`](../references/configuration.md#nodeid): 0, 1 and 2
- [`CheckpointInterval`](../references/configuration.md#checkpointinterval): 4
- [`RequiredMajority`](../references/configuration.md#requiredmajority): 2
- [`StableLedgerDepth`](../references/configuration.md#stableledgerdepth): 4

The following events take place in order to issue the first checkpoint. Note that most of these steps are continuous and concurrent, so they don't block each other.

1. The proof-of-work nodes mine blocks numbers 1, 2 and 3, propagating them throughout the network.
2. Meanwhile, Morpho nodes are continuously [pulling](#pulling-for-checkpoint-candidates) their associated proof-of-work nodes with the [`checkpointing_getLatestBlock`](../references/rpc.md#method-checkpointing_getlatestblock) RPC call, which returns nothing, since there are no checkpoint candidates yet, as we're only at the 3rd block.
3. The proof-of-work nodes mine block number 4, which is being propagated through the network. Only federation node 0 has received the new block at the moment.
4. Morpho node 0 pulls for a checkpoint candidate again, this time receiving the 4th proof-of-work block, aligning with `CheckpointInterval`.
5. Morpho node 0 signs the hash of the received proof-of-work block with its private key. The signature is then sent to the other Morpho nodes as a [vote](#vote)
6. Morpho node 1 is the current OBFT slot leader. It receives the vote from Morpho node 0 and puts it into its mempool. At the end of the slot it forges an OBFT block consisting of all the votes in the mempool. This OBFT block is then broadcast to the other Morpho nodes.
7. As all Morpho nodes are made aware of the new OBFT block, the [ledger state](#ledger-state-data-structures) for their latest OBFT block [changes accordingly](#ledger-state-transitions). The `morphoTip` is updated to the new OBFT block, and since we only have a single vote, which is less than `RequiredMajority`, it is added to `currentVotes` without creating a checkpoint.
8. The proof-of-work network now also propagates the new block to federation node 1.
9. Morpho node 1 pulls for a checkpoint candidate, receiving the 4th proof-of-work block. It also signs the hash with its private key and sends a vote for it to all the other Morpho nodes.
10. Morpho node 2 is now the OBFT slot leader. It receives the vote from Morpho node 1, puts it into its mempool, and forges an OBFT block at the end of the slot with the new vote in. This new block is then broadcast to the other Morpho nodes.
11. With the addition of a new OBFT block, Morpho's latest ledger state changes again accordingly. This time around `currentVotes` already contains the previous vote by node 0. Adding the new vote by node 1 achieves the minimum required majority of 2 (`RequiredMajority`). A new [checkpoint](#checkpoint) is therefore formed in the latest ledger state: It is assigned to `lastCheckpoint`, `checkpointAt` and `morphoTip` are updated to the new OBFT block, and `currentVotes` is cleared.
12. While the _latest_ ledger state now contains a new checkpoint, it isn't stable yet. Every time the latest ledger state changes, Morpho [looks at the ledger state](#pushing-new-checkpoints) from `StableLedgerDepth` OBFT blocks ago to get the latest _stable_ ledger state.
13. Morpho nodes wait and check for the latest stable ledger state to have `morphoTip` be equal to `checkpointAt`, indicating that a checkpoint was formed exactly `StableLedgerDepth` blocks ago.
14. Once that check succeeds, after 4 more OBFT blocks (`StableLedgerDepth`), Morpho nodes push the checkpoint stored in `lastCheckpoint` of the latest stable ledger state to their associated proof-of-work nodes via the [`checkpointing_pushCheckpoint`](../references/rpc.md#method-checkpointing_pushcheckpoint) RPC call.
15. Using the signatures received in the checkpoint, the proof-of-work nodes then create a new checkpoint block for the proof-of-work chain and send it to their peers
16. All nodes on the proof-of-work network receive the checkpoint block containing the checkpointing federation's signatures, ensuring that proof-of-work blocks 1 through 4 can't be reverted.
