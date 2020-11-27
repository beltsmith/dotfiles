{ pkgs, services, ... }:

let
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ~/dotfiles/doom.d;
  };
in {
  nixpkgs.config = import dotfiles/nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = dotfiles/nixpkgs-config.nix;
  nixpkgs.overlays =
    let
      # Change this to a rev sha to pin
      moz-rev = "master";
      moz-url = builtins.fetchTarball { url = "https://github.com/mozilla/nixpkgs-mozilla/archive/${moz-rev}.tar.gz";};
      nightlyOverlay = (import "${moz-url}/firefox-overlay.nix");
    in [
      nightlyOverlay
    ];

  home.packages = with pkgs; [
    doom-emacs
    zplug
    glances
    exa
    gitAndTools.hub
    fasd
    barrier
    fzf
    asdf direnv
    jq

    spotify

    # terminals
    kitty alacritty

    feh

    synergy
    lastpass-cli
    stow

    polybar rofi dunst
    slack discord

    arandr
    blueman flameshot picom redshift

    # fonts
    nerdfonts

    ripgrep ripgrep-all

    google-chrome google-chrome-dev
    latest.firefox-nightly-bin

    xorg.xkill
  ];

  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    userName = "beltsmith";
    userEmail = "me+gh@alexgirdler.com";
    extraConfig = {
      pull = { rebase = true; };
    };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    initExtra = (builtins.readFile zsh/initExtra.zsh);
    shellAliases = {
      l  = "exa -lgh";
      la = "l -a";
      lm = "l -smodified";
      pbcopy = "xsel --clipboard --input";
      pbpaste = "xsel --clipboard --output";
      kc = "kubectl";
      edit = "$EDITOR";
      hm = "home-manager";
      hms = "home-manager switch";
      hmb = "home-manager build";
    };
  };

  programs.zsh.zplug = {
    enable = true;
    plugins = [
      {name = "zsh-users/zsh-history-substring-search";}
      {name = "zsh-users/zsh-completions";}
      {name = "mafredri/zsh-async";}
      {name = "zsh-users/zsh-syntax-highlighting"; tags = ["defer:3"]; }
      {name = "sindresorhus/pure"; tags = [ "use:pure.zsh" "as:theme"];}
      {name = "b4b4r07/enhancd"; tags = ["use:init.sh"];}
      {name = "plugins/heroku"; tags = ["from:oh-my-zsh"];}
      {name = "plugins/kubectl"; tags = ["from:oh-my-zsh"];}
      {name = "plugins/git"; tags = ["from:oh-my-zsh"];}
      {name = "plugins/sudo"; tags = ["from:oh-my-zsh"];}
      {name = "plugins/bundler"; tags = ["from:oh-my-zsh"];}
      {name = "junegunn/fzf-bin"; tags = ["from:gh-r" "as:command" "rename-to:fzf"];}
      {name = "junegunn/fzf"; tags = ["as:command" "use:'bin/fzf-tmux'"];}
    ];
  };

  programs.tmux = {
    enable = true;
    clock24 = true;
    terminal = "screen-256color";
    baseIndex = 0;
    extraConfig = (builtins.readFile ~/dotfiles/tmux/.tmux.conf);
  };

  programs.direnv.enable = true;

  services.flameshot.enable = true;
  # services.blueman.enable = true;
  services.blueman-applet.enable = true;
  services.picom.enable = true;
  services.redshift = {
    enable = true;
    latitude = "37.775";
    longitude = "-122.419";
    tray = true;
  };
  services.polybar = {
    enable = true;
    script = "polybar main &";
    extraConfig = (builtins.readFile ~/dotfiles/polybar/.config/polybar/config);
    config = {
      colors = {
        background = "#222";
        background-alt = "#444";
        foreground = "#dfdfdf";
        foreground-alt = "#555";
        primary = "#ffb52a";
        secondary = "#e60053";
        alert = "#bd2c40";
      };
      "bar/main" =  {
        width = "100%";
        height = "27";
        fixed-center = false;
      };
    };
  };
  services.gpg-agent.enable = true;

  home.file = {
    ".config/kitty/kitty.conf".source = ~/dotfiles/kitty/.config/kitty/kitty.conf;
    ".emacs.d/init.el".text = ''(load "default.el")'';
  };
}
