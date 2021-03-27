# Topology file

The topology JSON file tells each Morpho node how to connect to other Morpho nodes to reach consensus on which proof-of-work blocks to checkpoint. The file can be specified either in the config file with [`TopologyFile`](../references/configuration.md#topologyfile----topology-filepath), or with the CLI option `--topology`. See the [topology explanation](../explanations/introduction.md#topology) for an overview of the checkpointing federation topology.

At the topmost level, the file is a list of [node setups](#node-setup).

### Node setup

**Type:** object with fields
- `nodeId` (number, required): The node this setup object describes. Only the setup object for which this value is equal to [`NodeId`](../references/configuration.md#nodeid) will be used by the node. This allows all nodes to use the same topology file, making updates easier during testing.
- `producers` (list of [producers](#producer), required): The Morpho nodes this node should connect to. Should not include the `nodeId` node.

### Producer

**Type:** object with fields
- `addr` (string, required): Either a DNS or an IP address. Using a DNS address allows dynamically changing the host without restarting the node.
- `port` (number, optional): The port to connect to on the given `addr`. A Morpho node whose [`NodePort`](../references/configuration.md#nodeport----port-port) is set to this value needs to run on that host. If no port is specified, an SRV DNS query is performed against the `addr` in order to determine both the `addr` and `port` to connect to. Omitting the port number allows dynamically changing the port without having to restart the node.
- `valency` (number, required): The number of active TCP connections to maintain to the host, usually should be set to 1. If `addr` is a DNS address, this number of distinct IP addresses will be connected to. If `addr` is an IP address, only values 0 and 1 make sense, where 0 disables the host.

## Example

Here is an example of a topology file for a federation consisting of 3 Morpho nodes, where the three nodes are accessible in different ways:
- Node 0 is accessible at the static IP address `172.18.67.46` and is running Morpho on the static port `1234`.
- Node 1's host can change dynamically. The DNS A record for `morpho.example.com` always points to it. It's using a static port of `5678`.
- Node 2's host and port can change dynamically. A DNS SRV record for `_morpho._tcp.example.com` always points to the correct host and port.

The following topology file can be used by all three nodes.

```json
[
  {
    "nodeId": 0,
    "producers": [
      {
        "addr": "morpho.example.com",
        "port": 5678,
        "valency": 1
      },
      {
        "addr": "_morpho._tcp.example.com",
        "valency": 1
      }
    ]
  },
  {
    "nodeId": 1,
    "producers": [
      {
        "addr": "172.18.67.46",
        "port": 1234,
        "valency": 1
      },
      {
        "addr": "_morpho._tcp.example.com",
        "valency": 1
      }
    ]
  },
  {
    "nodeId": 2,
    "producers": [
      {
        "addr": "172.18.67.46",
        "port": 1234,
        "valency": 1
      },
      {
        "addr": "morpho.example.com",
        "port": 5678,
        "valency": 1
      }
    ]
  }
]
```
