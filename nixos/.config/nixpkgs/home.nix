{ pkgs, ... }:

{
  home.packages = [
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

  programs.asdf.enable = true;

  programs.zsh = {
    enable = true;
    autocd = true;
    initExtra = (builtins.readFile zsh/initExtra.zsh);
    shellAliases = {
      l  = "exa -lgh";
      la = "l -a";
      lm = "l -smodified";
      pbcopy="xsel --clipboard --input";
      pbpaste="xsel --clipboard --output";
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
    # " ~/.tmux/plugins/tpm".source = git clone https://github.com/tmux-plugins/tpm
  };
}
