# How to add a new node to the federation

Adding a new node to the checkpointing federation requires every Morpho node to make changes to their configuration and the Morpho network must be restarted. Also every proof-of-work client in the network will have to adjust their configuration. This document explains the process of what needs to be done for this to work.

## Generate a key pair for the new node

We will need to generate a key pair for the new node, which can be done with
```
$ mantis cli generate-key-pairs
05833520f78aeb1a7e29519329fd76a6f51da9875eada2509e35e6439c24014e
290f0ab42c31bf54a66c29bbdef0d0c38c58576bb8a0e151fe95a0f8a324de8f6696e91b1f9fba549680a37c9e8cf5ee6d32a983a767becd2e701d81d8939930
```

The first line is the private key, the second line is the public key.

## Decide on a new NetworkMagic value

Morpho currently can't add a new node to the federation while keeping the same network. While it is possible to delete all the state directories to reset the network, that doesn't work well in a distributed setting. So instead we should decide on a new [`NetworkMagic`](../references/configuration.md#networkmagic) value to use. For this we can either generate a new value with
```
$ shuf -i 0-4294967295 -n 1
1003735316
```

Or we can just increase the previous `NetworkMagic` (say `1774970916`) by one, so the new one can be `1774970917`.

## Change every Morpho node's configuration

When adding a new Morpho node, every node in the federation needs to adjust their configuration as follows:
- [`NumCoreNodes`](../references/configuration.md#numcorenodes): Increase by one
- [`NetworkMagic`](../references/configuration.md#networkmagic): Assign the newly decided value
- [`FedPubKeys`](../references/configuration.md#fedpubkeys): Add the public key of the new node
- [`RequiredMajority`](../references/configuration.md#requiredmajority): Adjust to still be a majority (if necessary)

If needed, this is also a good time to change any other shared parameters such as [`SlotDuration`](../references/configuration.md#slotduration), [`CheckpointInterval`](../references/configuration.md#checkpointinterval) or [`StableLedgerDepth`](../references/configuration.md#stableledgerdepth).

### Configuring the new node

For the node that is about to be added, make sure that:
- The above fields are set to the same values
- [`NodeId`](../references/configuration.md#nodeid) is set to the new unused node id, `NumCoreNodes - 1`

## Adjust the topology file

The topology file needs to be adjusted as follows:
- A new [node setup](../references/topology.md#node-setup) entry needs to be added for the new node. It should include [producers](../references/topology.md#producer) for every previous node
- All previous node setup entries need to have a new producer for the node that was just added

## Mantis node changes

In order for proof-of-work clients such as Mantis to accept checkpoints signed by the new node they will have to trust its public key. This is the hardest part about adding a new node, because this requires configuration changes to every single node in the proof-of-work network.

For Mantis clients this means adding the new node's public key to the `mantis.blockchains.<network>.checkpoint-public-keys` configuration.
