{ pkgs, ... }:

{
  packages = with pkgs; [
    binutils # native-comp needs 'as', provided by this
    #emacsPgtkGcc # 28 + pgtk + native-comp
    emacsPgtk

    ## Doom dependencies
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls # for TLS connectivity

    ## Optional dependencies
    fd # faster projectile indexing
    imagemagick # for image-dired
    pinentry_emacs # in-emacs gnupg prompts
    zstd # for undo-fu-session/undo-tree compression

    ## Module dependencies
    # :checkers spell
    (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
    # :checkers grammar
    languagetool
    # :tools editorconfig
    editorconfig-core-c # per-project style config
    # :tools lookup & :lang org +roam
    sqlite
    # :lang cc
    ccls
    # :lang javascript
    nodePackages.javascript-typescript-langserver
    # :lang latex & :lang org (latex previews)
    texlive.combined.scheme-full
    # :lang rust
    rustc
    cargo
    rustfmt
    rust-analyzer

    racket

    ocamlPackages.utop
    ocamlPackages.merlin
    ocamlPackages.ocp-indent
    ocamlformat

    terraform

    plantuml

    shellcheck

    afew
  ];
}
