{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          haskellPractice =
            final.haskell-nix.cabalProject' {
              name = "haskell-practice";
              src = ./.;
              compiler-nix-name = "ghc884";
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.haskellPractice.flake {};
    in flake // {
      # Built by `nix build .`
      #defaultPackage = flake.packages."haskell-practice:exe:hello";

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShell = pkgs.haskellPractice.shellFor {
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
          stylish-haskell = "latest";
        };
        buildInputs = with pkgs; [ protobuf stack haskellPackages.implicit-hie bash git ];
      };

    }
    );
}
