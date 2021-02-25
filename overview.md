# Overview of how morpho works

This overview assumes that the reader is somewhat familiar with OBFT and checkpointing.

## Topology
- Morpho is run by a fixed number (setting `NumCoreNodes`, 5 in mantis-staging network) of nodes, each of which has a public-private key pair, and each node knows the public key of every other node (setting `FedPubKeys`). In practice, these nodes will be run by parties that are trusted by the community.
- Every morpho node has an associated mantis node which it can use to fetch information needed for checkpointing, and push resulting checkpoints using RPC calls (setting `PoWNodeRpcUrl`).
- Every mantis node, not only the ones associated with morpho, also know the public key of each morpho node.

## Mantis polling
- Every fetch interval (setting `PoWBlockFetchInterval`, currently 5) seconds:
  - Request the latest block at the correct interval (setting `CheckpointInterval`, currently 4) from the POW blockchain (`checkpointing_getLatestBlock` RPC call)
  - If the block indeed is at the correct interval and it has not been voted on already, cast the vote into the morpho blockchain by issuing a transaction for it
    - Correct interval determined by checking that the distance between the last checkpoint and the received block is divisible by the checkpointing interval [1], and that the last checkpoint is before the received block
      - The last checkpoint is determined by looking at the `lastCheckpoint` field of the latest ledger state of the morpho chain (see the [ledger state](#ledger-state) section for how this field is determined)
    - Whether it was already voted on is determined by looking at the `currentVotes` field in the latest ledger state of the morpho chain, seeing if this nodes public key is there and it voted for that block

Sources:
- [`Morpho.Node.Run.requestCurrentBlock`](https://github.com/input-output-hk/ECIP-Checkpointing/blob/4de0b3888642945aacf5b5eba874e5b6f1a5f6be/morpho-checkpoint-node/src/Morpho/Node/Run.hs#L264-L295)
- [`Morpho.Ledger.Update.voteBlockRef`](https://github.com/input-output-hk/ECIP-Checkpointing/blob/4de0b3888642945aacf5b5eba874e5b6f1a5f6be/morpho-checkpoint-node/src/Morpho/Ledger/Update.hs#L295-L325)

## Mantis pushing
- Every time the morpho ledger state changes:
  - Compute the latest stable ledger state by going back 6 (setting `StableLedgerDepth`) morpho blocks 
  - Determine if the checkpoint for that ledger state should be pushed or not
    - If checkpointAt == morphoTip (See how [the ledger state updates](#ledger-state) for reasoning) and morphoTip isn't genesis, push
      - Pushing is done with the `checkpointing_pushCheckpoint` RPC call
    - Otherwise don't push

Sources:
- [`Morpho.Node.Run.publishStableCheckpoint`](https://github.com/input-output-hk/ECIP-Checkpointing/blob/4de0b3888642945aacf5b5eba874e5b6f1a5f6be/morpho-checkpoint-node/src/Morpho/Node/Run.hs#L297-L342)

## Morpho blocks
- Every morpho block contains 0 or more transactions, where each transaction is a vote cast by a morpho node
- Every vote contains both a PoW block to vote on, and a signature signing the PoW block by the node that voted for it
- During each slot (every `SlotDuration`), the leader of that slot is forging a new block consisting of all the transactions (votes) received during that time slot
- After forging, the block is sent to all the other federation members

## Ledger state
The ledger state consists of the following fields:
- `morphoTip`: Points to the latest morpho block that was applied to the state
- `lastCheckpoint`: Contains the last checkpoint created, which stores the PoW block that was voted on and all the signatures
- `checkpointAt`: Points to the latest morpho block that created the checkpoint in lastCheckpoint
- `currentVotes`: For each federation member this stores either
  - No vote if they haven't cast a vote since the last checkpoint was created
  - The latest vote they cast since the checkpoint was created (later ones override earlier ones)

When a new morpho block is forged/received, the ledger state is updated by:
- Updating the morphoTip to the new block (ensures that when [Pushing a checkpoint to mantis](#mantis-pushing), the same checkpoint isn't pushed multiple times) [2]
- Insert all the votes of the block into currentVotes, overriding previous votes by the same public key [3]
- Check if more than or equal `RequiredMajority` number of currentVotes vote for the same PoW block
  - If so, create a new checkpoint from the currentVotes and the PoW block
    - Set lastCheckpoint to the created checkpoint
    - Update checkpointAt to the new block
    - Clear currentVotes

Sources:
- [`Morpho.Ledger.State.MorphoState`](https://github.com/input-output-hk/ECIP-Checkpointing/blob/4de0b3888642945aacf5b5eba874e5b6f1a5f6be/morpho-checkpoint-node/src/Morpho/Ledger/State.hs#L42-L48)
- [`Morpho.Ledger.Update.updateMorphoState`](https://github.com/input-output-hk/ECIP-Checkpointing/blob/4de0b3888642945aacf5b5eba874e5b6f1a5f6be/morpho-checkpoint-node/src/Morpho/Ledger/Update.hs#L207-L293)

## Annotated concerns
- [1]: Why not simply check whether the block number is divisible by `CheckpointInterval`?
- [2]:
  This seems very brittle, what if the pushing of the checkpoint failed?
  Shouldn't this be idempotent anyways, so that sending the same checkpoint multiple times isn't a problem?
  What if multiple checkpoints are forged in quick succession, could one of them not be sent?
- [3]:
  Is it really okay to override previous votes?
  What if a checkpoint was about to be produced, but one member already voted for a new one, overriding and clearing its previous vote?
  Maybe an arbitrary number of votes should be allowed for each federation member
