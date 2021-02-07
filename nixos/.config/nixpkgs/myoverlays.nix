{ ... }:

let
  importOverlay = { rev, url }:
    (import builtins.fetchTarball { url = "${url}/archive/${rev}.tar.gz"; });
  nightlyOverlay = importOverlay {
    rev = "master";
    url = "https://github.com/mozilla/nixpkgs-mozilla";
  };
  emacsOverlay = importOverlay {
    rev = "master";
    url = "https://github.com/nix-community/emacs-overlay";
  };
in { overlays = [ nightlyOverlay emacsOverlay ]; }
