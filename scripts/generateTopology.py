#!/usr/bin/env python
import json
import sys
'''
Topology file generator.

WARNING: this is a quickly hacked script, there's no error handling or
         whatsoever.

Provided a JSON node list in the form { "nodeId": "ip" } in STDIN,
this script will generate a topology file in STDOUT.

Usage example:

# cat nodeList.json
 {
    "0": "127.0.0.1",
    "1": "192.168.1.1",
    "2": "192.168.1.2"
 }
# cat nodeList.json | ./scripts/generateTopology.py > topology.json
# cat topology.json
[
  {
    "nodeId": 0,
    "producers": [
      {
        "addr": "192.168.1.1",
        "port": 3001,
        "valency": 1
      },
      {
        "addr": "192.168.1.2",
        "port": 3001,
        "valency": 1
      }
    ]
  },
  (...)
]


'''

defaultPort = 3000

def genPeer(nodeId):
    return {
        'nodeId' : int(nodeId),
        'producers': [
            {
                'addr':peer[1],
                'port':defaultPort,
                'valency':1
            } for peer in addresses.items() if peer[0] != nodeId
        ]
    }

if __name__ == '__main__':
    addresses = json.loads(''.join(sys.stdin.readlines()))
    t = [ genPeer(node[0]) for node in addresses.items() ]
    print(json.dumps(t, sort_keys=True, indent=2))
    exit(0)
