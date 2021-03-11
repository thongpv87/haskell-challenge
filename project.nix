let
  sources = import ./nix/sources.nix {};

  haskell-nix = import sources."haskell.nix" {
  };

  nixpkgs = import haskell-nix.sources.nixpkgs (haskell-nix.nixpkgsArgs // { config = haskell-nix.nixpkgsArgs.config // { allowUnfree = true; }; });

  src = builtins.path {
    name = "haskell-project-src";
    path = ./.;
    filter = path: type:
      let
        basePath = builtins.baseNameOf path;
      in
        basePath != "dist-newstyle"
    ;
  };
in
nixpkgs.haskell-nix.cabalProject {
  inherit src;
  compiler-nix-name = "ghc8102";
}

