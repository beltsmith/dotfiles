{ pkgs, ... }:

{
  packages = with pkgs; [
    # languages
    ruby_2_7
    rubyPackages_2_7.rails
    solargraph
    bundix
    rufo

    arduino
    # arduino-core
    # arduino-cli
    # arduino-ci

    ocaml
    dune

    python3

    openjdk
  ];
}