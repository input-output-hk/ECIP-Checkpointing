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
curl ${BIN_URL} > blockchain-checkpoint-node
curl ${WRAPPER_URL} > start-node
chmod u+x blockchain-checkpoint-node
chmod u+x start-node
popd

# Getting Config
mkdir configuration
pushd configuration
curl ${TOPOLOGY_URL} > blockchain-topology.json
curl ${CONFIG_URL} > blockchain-configuration.yaml
curl ${GENESIS_URL} > shelley-staging-genesis.json
popd

# Done
echo "Don't forget to set the appropriate NodeID in the configuration file."
echo "Once that done, you can start the node by running ./bin/start-node"
