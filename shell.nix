let
  sources = import ./nix/sources.nix {};
  nixpkgs = import sources.nixpkgs {};

  hsPkgs = nixpkgs.haskell.packages.ghc8102;

  project = import ./default.nix;
in
nixpkgs.mkShell {
  name = "cabal-shell";
  inputsFrom = [ project.env ];
  buildInputs = with hsPkgs; [ haskell-language-server cabal-install hie-bios hoogle ];
}
