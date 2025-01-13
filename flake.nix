{
  description = "Nuchain";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
    sbv.url =
      "github:aravindgopall/sbv/5bad74c93936265046b511308a19e5f864094b75";
    sbv.flake = false;
    pact.url = "github:arshad-6093/pact/0ec45d4f0af24fde629743b2dce75c68ee6bbb3b";
    pact-json.url = "github:kadena-io/pact-json";
    pact-json.flake = false;
    thyme.url = "github:kadena-io/thyme";
    thyme.flake = false;
    higher-leveldb.url =
      "github:jeremyjh/higher-leveldb/9ca7197645a9eb8b27da9eba0904be9d5aa047b8";
    higher-leveldb.flake = false;
    libraft.url = "github:aravindgopall/libraft";
    ekg-json.url = "github:aravindgopall/ekg-json";
    hs-rqlite.url = "github:aravindgopall/hs-rqlite";
    crypton.url = "github:arshad-6093/crypton/c06df875824a1f0819668659078cf2c1ac072a76";
    crypton.flake = false;
    web3-solidity.url = "github:arshad-6093/solidity";
    web3-solidity.flake = false;
    masala.url = "github:arshad-6093/masala/da70f531f80400580b51b26b1b0982eabfe800a1";
    masala.flake = false;
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          # The base package set (this value is the default)
          imports = [
            #inputs.libraft.haskellFlakeProjectModules.output
          ];

          basePackages = pkgs.haskell.packages.ghc94;
          # Packages to add on top of `basePackages`
          packages = {
            # Add source or Hackage overrides here
            # (Local packages are added automatically)
            /* aeson.source = "1.5.0.0" # Hackage version
               shower.source = inputs.shower; # Flake input
            */
            higher-leveldb.source = "0.6.0.0";
            pact.source = inputs.pact;
            pact-json.source = inputs.pact-json;
            thyme.source = inputs.thyme;
            sbv.source = inputs.sbv;
            libraft.source = inputs.libraft;
            ekg-json.source = inputs.ekg-json;
            hs-rqlite.source = inputs.hs-rqlite;
            crypton.source = inputs.crypton;
            web3-solidity.source = inputs.web3-solidity;
            masala.source = inputs.masala;
          };

          # Add your package overrides here
          settings = {
            libraft.check = false;
            higher-leveldb.jailbreak = true;
            higher-leveldb.check = false;
            curryer-rpc.jailbreak = true;
            unicode-data.check = false;
            ekg-json.broken = false;
            ekg-json.jailbreak = true;
            ekg.jailbreak = true;
            monad-metrics.broken = false;
            hs-rqlite.broken = false;
            hs-rqlite.jailbreak = true;
            markov-chain-usage-model.broken = false;
            markov-chain-usage-model.check = false;
            quickcheck-state-machine = {
              broken = false;
              check = false;
            };
            sbv = {
              broken = false;
              jailbreak = true;
              check = false;
              libraryProfiling = false;
              haddock = false;
            };
            poly = {
              broken = false;
              check = false;
              libraryProfiling = false;
            };
            cacophony = {
              broken = false;
              check = false;
            };
            thyme = {
              libraryProfiling = false;
              jailbreak = true;
              check = false;
            };
            true-name.jailbreak = true;
            pact = {
              libraryProfiling = false;
              check = false;
              jailbreak = true;
            };
            barbies-th = {
              broken = false;
              jailbreak = true;
            };
            unicode-data.jailbreak = true;
            winery.check = false;
            cryptonite = {
              custom = drv:
                drv.overrideAttrs { NIX_CFLAGS_COMPILE = "-fcommon"; };
            };
            ed25519-donna = {
              custom = drv:
                drv.overrideAttrs { NIX_CFLAGS_COMPILE = "-fcommon"; };
            };
            jsonrpc-tinyclient = {
              broken = false;
              jailbreak = true;
            };
            scale = {
              broken = false;
              jailbreak = true;
            };
            memory-hexstring = {
              broken = false;
              jailbreak = true;
            };
            web3-crypto = {
              broken = false;
              jailbreak = true;
            };
            web3-solidity = {
              broken = false;
              jailbreak = true;
            };
            web3-ethereum = {
              check = false;
              broken = false;
              jailbreak = true;
            };
            web3-provider = {
              broken = false;
              jailbreak = true;
            };
            animalcase = {
              broken = false;
              jailbreak = true;
            };
            web3-bignum = {
              broken = false;
              jailbreak = true;
            };
            web3-polkadot = {
              check = false;
              broken = false;
              jailbreak = true;
            };
            web3 = {
              broken = false;
              jailbreak = true;
            };
            masala = {
              broken = false;
              jailbreak = true;
              check = false;
            };
            http-conduit = {
              broken = false;
              jailbreak = true;
              check = false;
            };
          };

          # Development shell configuration
          #devShell = {
          #hlsCheck.enable = false;
          #};
          devShell = {
            tools = hp: {
              haskell-language-server = null;
              ghcid = null;
            };
            hlsCheck.enable = false;
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.fourmolu = {
            enable = true;
            package = config.fourmolu.wrapper;
          };
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };

        fourmolu.settings = {
          indentation = 2;
          comma-style = "leading";
          record-brace-space = true;
          indent-wheres = true;
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "multi-line";
          newlines-between-decls = 1;
          extensions = [ "ImportQualifiedPost" ];
        };

        # Default package & app.
        packages.default = self'.packages.nuchain;
        apps.default = self'.apps.kadenaserver;

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "nuchain-devshell";
          meta.description = "nuchain development environment";
          # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [ just ];
        };
      };
    };
}
