[![](https://img.shields.io/endpoint?url=https%3A%2F%2Fhydra.mantis.ist%2Fjob%2Fecip-checkpointing%2Fmaster%2Fpackages.checks%2Fshield)](https://hydra.mantis.ist/job/ecip-checkpointing/master/packages.checks)

# Morpho Checkpointing Node

This is the codebase of the checkpointing nodes for [Mantis](https://mantisclient.io/).

For building and development, a recent Nix version supporting Flakes is recommended, see [here](https://nixos.wiki/wiki/Flakes#Installing_flakes) for installation instructions. The readme assumes such a newer version is available, however older Nix versions can still be used with slightly adjusted commands. Not using Nix is also a possibility but not explicitly shown.

For an overview of how morpho is implemented, see [overview.md](./overview.md).

## Binary Cache

Unless if you are looking for a rather expensive lap/desk heater, you are probably off better leveraging the [Mantis Hydra](https://hydra.mantis.ist/project/ecip-checkpointing) binary cache instead of rebuilding the whole world by yourself.

On a NixOS Machine this needs to be done by adding this to your `configuration.nix`:
```nix
{
  nix = {
    binaryCaches = [
      "https://hydra.mantis.ist"
    ];
    binaryCachePublicKeys = [
      "hydra.mantis.ist-1:4LTe7Q+5pm8+HawKxvmn2Hx0E3NbkYjtf1oWv+eAmTo="
    ];
  };
}
```

On non-NixOS machines your `/etc/nix/nix.conf` needs to be changed as follows:

```
substituters = https://hydra.mantis.ist https://cache.nixos.org/
trusted-substituters = https://hydra.mantis.ist https://cache.nixos.org/
trusted-public-keys = hydra.mantis.ist-1:4LTe7Q+5pm8+HawKxvmn2Hx0E3NbkYjtf1oWv+eAmTo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

## Building

To build the main executable:
```
$ nix build
```

## Development

To enter a development environment, [direnv](https://github.com/direnv/direnv) is recommended for convenience, also allowing you to use your preferred shell. Once installed, just run
```
$ direnv allow
```

and you'll have a working development environment for now and the future whenever you enter this directory.

Otherwise you can use `nix develop` to get a bash shell:
```
$ nix develop
```

The following subsections assume to be run in such development shell.

### Building during Development

To build the main executable:
```
$ cabal build exe:morpho-checkpoint-node
```

### Interactive Development

To get an interactive GHCi repl:
```
$ cabal repl lib:morpho-checkpoint-node
```

Alternatively you can use [ghcid](https://github.com/ndmitchell/ghcid) to get instantaneous feedback:
```
$ ghcid lib:morpho-checkpoint-node
```

A hoogle with all the dependencies is also available, which can either be queried directly:
```
$ hoogle fromMaybe
```

Or you can start a hoogle server for exploring the dependencies in your browser at http://localhost:8080/:
```
$ hoogle server --local
```

Also included in the environment is a working [Haskell Language Server](https://github.com/haskell/haskell-language-server) you can integrate with your editor. See [here](https://github.com/haskell/haskell-language-server#configuring-your-editor) for instructions.

## Testing

For every commit and PR, all test suites are run by Hydra. To run them locally:
```
$ cabal test all
```

There are currently three test suites which can also be run independently:

### Unit Tests

These tests are mostly meant to be run interactively when working
on the checkpoint node. They are pretty fast to complete (>5s).

You can run those using:

```
$ cabal test test
```

### State Machine Tests
These tests are acting as integration tests. They'll require to
spin up several morpho nodes and mocked mantis nodes. They are
pretty long to run (~4 minutes), hence not really meant to be run
very frequently.

You can run those using:

```
$ cabal test state-machine-tests
```

### Mantis Integration Tests
Here, we are testing some Morpho node features directly against the
Mantis node itself. These test require to have Mantis and its
associated CLIs (incl. `signatureValidator`) in your `$PATH` when
running them. This is already taken care of by the Nix environment.

You can run those using:

```
$ cabal test mantis-integration-tests
```

### Full integration testing

In addition to the test suites, it's also possible to manually run a full integration test, running a local mining Mantis node and two Morpho nodes for checkpointing its blocks. See [the example directory](./example) for instructions.

## Code Formatting

We are using the [ormolu](https://github.com/tweag/ormolu) formatter for the Haskell files, `cabal format` for the cabal files.

The CI will reject any non-formatted PR, please make sure you properly
format your code before opening one.

You can use the pre-commit hook to automatically run the formatters
before comitting. You can enable the provided pre-commit hook by
running:

```sh
$ cd $root_of_this_git_repo
$ ln -s $PWD/pre-commit .git/hooks/pre-commit
```

## Profiling

To enable profiling, you need to set the `profile` attribute to `true` in `flake.nix` and reload the development environment (done automatically if `direnv` is used). All the dependencies will be rebuilt with profiling enabled (might take a while).

Next, we can add the desired GHC profiling options in `morpho-checkpoint-node/morpho-checkpoint-node.cabal`. For example:

```
ghc-options:
    -- ...other GHC options
    "-with-rtsopts=-T -p -s -h -l -i0.1"
```

The `-with-rtsopts` GHC option bakes in the runtime settings. Explanation of those runtime settings:

```
- `-p` generates a time and allocation profiling report in a `.prof` file
- `-s` outputs a report on garbage collection
- `-l` outputs an eventlog report
- `-h` generates a standard report on memory usage which can then be used to produces a plot in a postscript file using `hp2ps` utility.

- `-i0.1` sets the sampling frequency of memory profiling to every tenth of a second
```

All possible settings are described [here](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html#).

Those GHC options should ideally be set on the `Morpho` executable, but can also be set on the `state-machine-tests` test suite.

After the development environment finished loading, run `cabal clean`, configure cabal with `cabal configure --enable-profiling` and build with `cabal build morpho-checkpoint-node`.

To profile the executable, run the full integration test as described in the section [Full integration testing](#full-integration-testing).

To profile the `state-machine-tests` test suite, run the `state-machine-tests` as described in the section [State Machine Tests](#state-machine-tests).

These should produce reports with extensions `.prof` (e.g. `morpho-checkpoint-node.prof`), `.hp` (e.g. `morpho-checkpoint-node.hp`) and `.eventlog` (e.g. `morpho-checkpoint-node.eventlog`).

Run `hp2ps -e8in -c morpho-checkpoint-node.hp` to convert the report on memory usage into a postscript graph which can be opened with a PDF viewer.

Run `eventlog2html morpho-checkpoint-node.eventlog` to convert the eventlog report into a static webpage in order to visualise the heap profile.
