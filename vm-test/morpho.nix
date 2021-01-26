{
  systemd.services.morpho-node = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      RuntimeDirectory = "morpho";
      StateDirectory = "morpho";
    };
    script = let
      port = 6000;
      mantisPort = 6100;
      key = pkgs.runCommandNoCC "morpho-secret-key" {
        nativeBuildInputs = [ mantis ];
      } ''
        eckeygen 1 | sed -r '/^\s*$/d' > keyFile
        mkdir $out
        head -1 keyFile > $out/secret
        tail -1 keyFile > $out/public
      '';
      topology = (pkgs.formats.json {}).generate "topology.json" {

      };
      configFile = (pkgs.formats.yaml {}).generate "morpho-config.yaml" {
        ApplicationName = "morpho-checkpoint";
        ApplicationVersion = 1;
        CheckpointInterval = 4;
        FedPubKeys = [ "NIXOS_MORPHO_PUBLIC_KEY" ];
        LastKnownBlockVersion-Major = 0;
        LastKnownBlockVersion-Minor = 2;
        LastKnownBlockVersion-Alt = 0;
        NetworkMagic = 12345;
        NodeId = 1;
        NodePrivKeyFile = "${key}/secret";
        NumCoreNodes = 3;
        PoWBlockFetchInterval = 5000000;
        PoWNodeRpcUrl = "http://127.0.0.1:${toString mantisRpcPort}";
        #PrometheusPort: {{ env "NOMAD_PORT_morphoPrometheus" }}
        Protocol = "MockedBFT";
        RequiredMajority = nbNodes / 2 + 1;      # 0 -> 1, 1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3
        RequiresNetworkMagic = "RequiresMagic";
        SecurityParam = 2200;
        StableLedgerDepth = 6;
        SlotDuration = 5;
        SnapshotsOnDisk = 60;
        SnapshotInterval = 60;
        SystemStart = "2020-11-17T00:00:00Z";
        TurnOnLogMetrics = false;
        TurnOnLogging = true;
        ViewMode = "SimpleView";
        minSeverity = "Debug";
        TracingVerbosity = "NormalVerbosity";
        setupScribes = [{
          scKind = "StdoutSK";
          scFormat = "ScText";
          scName = "stdout";
        }];
        defaultScribes = [
          [ "StdoutSK" "stdout" ]
        ];
        setupBackends = [ "KatipBK" ];
        defaultBackends = [ "KatipBK" ];
        options.mapBackends = {};
      };
    in ''
      ${morpho}/bin/morpho-checkpoint-node \
        --topology "${topology}" \
        --database-path "$STATE_DIRECTORY"/db \
        --port "${toString port}" \
        --config "$NOMAD_TASK_DIR/morpho-config.yaml" \
        --socket-dir "$RUNTIME_DIRECTORY"/socket
    '';
  };
}
