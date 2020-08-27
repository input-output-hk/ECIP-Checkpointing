#!/usr/bin/env bash

exe=dist-newstyle/build/x86_64-osx/ghc-8.6.5/blockchain-checkpoint-node-1.0.0/x/blockchain-checkpoint-node/build/blockchain-checkpoint-node/blockchain-checkpoint-node

${exe} --topology configuration/blockchain-topology.json \
       --database-path ./db/db \
       --genesis-file configuration/shelley-staging-genesis.json \
       --port 3000 \
       --config configuration/blockchain-configuration.yaml \
       --genesis-hash 3 \
       --socket-dir ./socket \
       --trace-chain-sync-block-server \
       --trace-mempool \
       --trace-forge \
       --trace-ledger-state \
       --trace-rpc \
       --tracing-verbosity-maximal
