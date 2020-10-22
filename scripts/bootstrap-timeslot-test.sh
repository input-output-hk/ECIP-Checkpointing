#!/usr/bin/env bash

BIN_URL="https://TODO"
WRAPPER_URL="https://TODO"
TOPOLOGY_URL="https://TODO"
CONFIG_URL="https://TODO"
GENESIS_URL="https://TODO"

# Installing deps
sudo apt update
sudo apt -y install zlib1g-dev libgmp-dev libffi-dev tmux

# Getting binaries
mkdir bin
pushd bin
curl ${BIN_URL} > morpho-checkpoint-node
curl ${WRAPPER_URL} > start-node
chmod u+x morpho-checkpoint-node
chmod u+x start-node
popd

# Getting Config
mkdir configuration
pushd configuration
curl ${TOPOLOGY_URL} > morpho-topology.json
curl ${CONFIG_URL} > morpho-configuration.yaml
popd

# Done
echo "Don't forget to set the appropriate NodeID in the configuration file."
echo "Once that done, you can start the node by running ./bin/start-node"
