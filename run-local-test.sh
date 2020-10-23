#!/usr/bin/env bash

exe=dist-newstyle/build/x86_64-osx/ghc-8.6.5/morpho-checkpoint-node-1.0.0/x/morpho-checkpoint-node/build/morpho-checkpoint-node/morpho-checkpoint-node

${exe} --topology configuration/morpho-topology.json \
       --database-path ./db/db \
       --port 3000 \
       --config configuration/morpho-configuration.yaml \
       --socket-dir ./socket \
       --trace-chain-sync-block-server \
       --trace-mempool \
       --trace-forge \
       --trace-ledger-state \
       --trace-morpho-rpc \
       --tracing-verbosity-maximal
