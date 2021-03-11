let
  project = import ./project.nix;
in
project.holmusk-challenge.components.exes.Hello
