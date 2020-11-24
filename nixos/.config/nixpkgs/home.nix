{ pkgs, ... }:

let
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ~/dotfiles/doom.d;
  };
in {
  home.packages = [
    doom-emacs
    pkgs.zplug
    pkgs.glances
    pkgs.exa
    pkgs.gitAndTools.hub
    pkgs.fasd
    pkgs.barrier
    pkgs.spotify
    pkgs.ripgrep
    pkgs.fzf
    pkgs.asdf
    pkgs.jq
  ];

  nixpkgs.config.allowUnfree = true;

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

  home.file = {
    ".config/kitty/kitty.conf".source = ~/dotfiles/kitty/.config/kitty/kitty.conf;
    ".emacs.d/init.el".text = ''(load "default.el")'';
  };
}
