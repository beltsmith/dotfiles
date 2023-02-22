#!/usr/bin/env zsh
  # This is my zshrc
  export MY_ZSH=$HOME/.zsh
  ###############################################
  ################# Variables  ##################
  ###############################################
  export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
  export DEVKITPRO=/opt/devkitpro
  export DEVKITARM=/opt/devkitpro/devkitARM
  export DEVKITPPC=/opt/devkitpro/devkitPPC
  if [[ -n $SSH_CONNECTION ]]; then
      export EDITOR='emacsclient -c'
  else # lol
      export EDITOR='vim'
  fi
    append_path() {
      export PATH="$PATH:$1"
    }
  
    prepend_path() {
      export PATH="$1:$PATH"
    }
  prepend_path "$HOME/.rbenv/bin"
  
  # The next line updates PATH for the Google Cloud SDK.
  if [ -f '/home/belt/dev/wellsaid/gcloud/google-cloud-sdk/path.zsh.inc' ]; then . '/home/belt/dev/wellsaid/gcloud/google-cloud-sdk/path.zsh.inc'; fi
  
  # The next line enables shell command completion for gcloud.
  if [ -f '/home/belt/dev/wellsaid/gcloud/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/belt/dev/wellsaid/gcloud/google-cloud-sdk/completion.zsh.inc'; fi
  prepend_path "$HOME/.cargo/bin"
  export GOPATH="$HOME/go"
  prepend_path "$GOPATH/bin"
  append_path "$HOME/bin"
  append_path "$HOME/scripts"
  append_path "$HOME/dotfiles/scripts"
  append_path "$HOME/dotfiles/scripts/scripts"
  append_path "/usr/local/bin"
  append_path "$HOME/.local/bin"
  append_path "$HOME/.emacs.d/bin"
  export _JAVA_AWT_WM_NONREPARENTING=1
  export EMAIL="me@alexgirlder.com"
  export WORK_EMAIL="alexander@prevail.io"
  export LPASS_HOME=$HOME/.lpass
  export LPASS_AGENT_TIMEOUT=0
  export AWS_VAULT_BACKEND=secret-service
  export AWS_SESSION_TOKEN_TTL=12h
  export TERM=xterm-256color
  ###############################################
  ################  Directories ## ##############
  ###############################################
  hash -d dev=/home/belt/dev
  hash -d steam=/home/belt/.steam/steam/steamapps/common
  hash -d games=/mnt/games/steamapps/common
  ###############################################
  #################  Aliases   ##################
  ###############################################
  alias kbflash='qmk flash -kb handwired/dactyl_promicro -km beltsmith'
  alias gcproject="gcloud config set project"
    alias l="exa -lgh --git"
    alias ls='exa' # for compatibility with fzf
    alias la='l -a'
    alias lm="l -smodified"
  alias nsw="sudo netctl switch-to"
    alias edit=$EDITOR
    # alias vim='nvim'
    # alias emacs='emacs'
    alias spacemacs='emacs'
  alias vimrc='edit ~/.config/nvim/init.vim'
  alias zshrc='edit ~/.zshrc'
  alias zshv='edit ~/.zsh_vars'
    alias paccmd='yay'
    # alias ya='yaourt'
    alias pacsearch='paccmd -Ss'
    alias pacins='paccmd -S'
    alias pacin='paccmd -U'
    alias pacupd='paccmd -Syyu'
    alias pacdb='sudo pacman -Syy'
    alias sync-packages='arch-install.sh'
  
    alias pbcopy='xsel --clipboard --input'
    alias pbpaste='xsel --clipboard --output'
  
    alias xboxc='sudo xboxdrv --mimic-xpad --detach-kernel-driver --silent'
    alias steam_wine='WINEDEBUG=-all wine ~/.wine/drive_c/Program\ Files/Steam/Steam.exe -no-dwrite >/dev/null 2>&1 &'
    alias bt='bluetoothctl'
  alias ..="\cd .."
  alias ...="\cd ../.."
  alias ....="\cd ../../.."
  alias .....="\cd ../../../.."
  alias ......="\cd ../../../../.."
  
  alias kc="kubectl"
  alias tf="terraform"
  alias dcmp="docker-compose"
    # unalias rg # Fuck off rails generate, who the hell uses you
  alias cassandra="docker run --rm --network host cassandra"
  alias cassandrad="docker run --network host --name my-cassandrad -d cassandra"
  alias cqlsh="docker run --rm -it --network host cassandra cqlsh"
  alias nassh="TERM=xterm-256color ssh root@tower"
  alias hosts="hosts --auto-sudo"
  
  fpath=(~/.zsh/completion $fpath)
  ###############################################
  ################# Functions  ##################
  ###############################################
  reload() {
    source ~/.zshrc
  }
  loadit() {
      [[ -a $1 ]] && source $1
  }
  
    gi() {
        gem install $@
        rbenv rehash
    }
  
  slowly() { trickle -u 1024 -d 1024 $@ }
  cdl () { cd "$@" && ls; }
  y2j() {
      if [[ $# -gt 1 ]]; then
          ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load(ARGF))' < $1 > $2
      else
          ruby -ryaml -rjson -e 'puts JSON.pretty_generate(YAML.load(ARGF))' < $1
      fi
  }
  
  mackup() {
    local folder=$1
    local log=$(mktemp /tmp/transfer.log.XXXXX)
    local destination='backup'
    rsync -avzm --stats --safe-links --ignore-existing --dry-run --human-readable "$folder" "$destination" > "$log"
    cat $log | parallel --will-cite -j 5 rsync -avzm --relative --stats --safe-links --ignore-existing --human-readable {} "$destination" > result.log
  }
  mt() {
    echo '' > $1
  }
    rbit() {
      local rbv='.ruby-version'
      [[ -a $rbv ]] && rbenv install $(< $rbv)
    }
    reflect() {
      sudo reflector --protocol https --latest 30 --number 20 --sort rate --save /etc/pacman.d/mirrorlist
    }
  as-circle() {
      CIRCLE_BRANCH=$(git rev-parse --abbrev-ref HEAD) CIRCLE_SHA1=$(git rev-parse --short HEAD) "$@"
  }
  fzmv() {
      local src=$(find "$1" -type f | fzf)
      local dest=$2
      mv "$src" "$dest"
  }
  
  kasa-git-set() {
      git config --local user.name alex-kasa
      git config --local user.email alex.girdler@kasa.com
      git config --local github.user alex-kasa
      repo=$(git remote -v | awk '{print $2}' | head -n 1 | awk -F '/' '{print $NF}' | cut -f 1 -d '.')
      git remote set-url origin "git@kasa.github.com:kasadev/$repo"
  }
  multi_rspec() {
      for i in $(seq $1) ;
        do bundle exec rspec spec ; [[ ! $? = 0 ]] && break ;
      done
  }
  
  do_multi() {
      for i in $(seq $1) ;
        do ${*:2} ; [[ ! $? = 0 ]] && break ;
      done
  }
  multi_ne() {
      for i in $(seq $1) ;
        do ${*:2} ;
      done
  }
  
  multi_async() {
      for i in $(seq $1) ; do JOB=$i ${*:2} & ; done
  }
  
  multi_curl() { for i in $(seq $1) ; do ${*:2} -h >> logs.out & ; done }
    function mount_build() {
      local build_number="$1"
    }
  function hsql() {
      psql `heroku config -a $1 | grep 'MASTER_DATA' | cut -f 2-100 -d ':'`
  }
  
  function _hsql() {
  
  }
  function _herokuapps() {
      local cache_file = /tmp/heroku_apps
      [[ ( ! -r $cache_file) ]] && heroku
      _arguments -C <(cache_file)
  }
  function hconf() {
      heroku config -a $1
  }
  ###############################################
  ################# Completion ##################
  ###############################################
  # autoload -Uz compinit
  # compinit
  # autoload -Uz compinit && compinit -i
  ###############################################
  #################   Tools    ##################
  ###############################################
    . /opt/asdf-vm/asdf.sh
    # . $HOME/.asdf/asdf.sh
    # . $HOME/.asdf/completions/asdf.bash
  export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
  ###############################################
  #################    SSH     ##################
  ###############################################
  export SSH_ENV="$HOME/.ssh/environment"
  
  start_agent() {
      echo "Initialising new SSH agent..."
      /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
      echo succeeded
      chmod 600 "${SSH_ENV}"
      . "${SSH_ENV}" > /dev/null
      /usr/bin/ssh-add;
  }
  
  # Source SSH settings, if applicable
  
  if [ -f "${SSH_ENV}" ]; then
      . "${SSH_ENV}" > /dev/null
      #ps ${SSH_AGENT_PID} doesn't work under cywgin
      ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
          start_agent;
      }
  else
      start_agent;
  fi
  ###############################################
  #################   Plugins  ##################
  ###############################################
  # export ZPLUG=/usr/share/zsh/scripts/zplug
  # source $ZPLUG/init.zsh
  source $HOME/dotfiles/packages.sh
  [[ -f ~/gits/zsh-snap/znap.zsh ]] ||
      git clone --depth 1 -- \
          https://github.com/marlonrichert/zsh-snap.git ~/gits/zsh-snap
  source ~/gits/zsh-snap/znap.zsh
  
  znap source marlonrichert/zsh-autocomplete
  znap source zsh-users/zsh-autosuggestions
  znap source zsh-users/zsh-syntax-highlighting
  znap source marlonrichert/zcolors
  
  znap source b4b4r07/enhancd
  # znap source junegunn/fzf #, as:command, use:"bin/fzf-tmux"
  # znap source junegunn/fzf-bin #, from:gh-r, as:command, rename-to:fzf
  
  znap prompt sindresorhus/pure
    # zplug "zsh-users/zsh-history-substring-search"
    # zplug "zsh-users/zsh-completions" # do-everything argument completions
    # zplug "marlonrichert/zsh-autocomplete"
    # zplug 'zsh-users/zsh-syntax-highlighting', defer:3
    # zplug "mafredri/zsh-async"
    # zplug "sindresorhus/pure", use:pure.zsh, as:theme
    # zplug "dracula/zsh", as:theme
  
    # zplug "b4b4r07/enhancd", use:init.sh
    # zplug "junegunn/fzf", as:command, use:"bin/fzf-tmux"
    # zplug "junegunn/fzf-bin", from:gh-r, as:command, rename-to:fzf
    # zplug "plugins/heroku", from:oh-my-zsh
  # zplug "plugins/kubectl", from:oh-my-zsh
  # zplug "plugins/gcloud", from:oh-my-zsh
  # zplug "plugins/git", from:oh-my-zsh
    # zplug "plugins/sudo", from:oh-my-zsh
    # zplug "plugins/bundler", from:oh-my-zsh
    # zplug "plugins/rake-fast", from:oh-my-zsh
    # zplug "plugins/rbenv", from:oh-my-zsh
    # zplug "plugins/colorize", from:oh-my-zsh # Plugin for highlighting file content
    # zplug "lib/clipboard", from:oh-my-zsh, if:"[[ $OSTYPE == *darwin* ]]"
    # zplug "hchbaw/auto-fu.zsh"
  
    # zplug "plugins/rails", from:oh-my-zsh
  
    # zplug "lib/completion", from:oh-my-zsh
  ###############################################
  #################    Evals   ##################
  ###############################################
  # eval "$(hub alias -s)"
  
  eval "$(fasd --init auto)"
  # eval "$(pyenv init -)"
  eval "$(direnv hook zsh)"
  # eval "$(rbenv init -)"
  test -r /home/belt/.opam/opam-init/init.zsh && . /home/belt/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
  ###############################################
  #################    Misc    ##################
  ###############################################
  if [[ $TERM == xterm-termite ]]; then
      . /etc/profile.d/vte.sh
      __vte_osc7
  fi
  bindkey -v
  
  bindkey "\e." insert-last-word
  bindkey -M viins '\e.' insert-last-word
  
  # bindkey "^[[A" history-beginning-search-backward
  # bindkey "^[[B" history-beginning-search-forward
  
  bindkey "^[[1;3C" forward-word
  bindkey "^[[1;3D" backward-word
  bindkey '^e' end-of-line
  bindkey '^a' beginning-of-line
  
  bindkey "\e[3~" delete-char
  
  bindkey '^r' history-incremental-search-backward
  export HISTSIZE=1000000000
  export SAVEHIST=$HISTSIZE
  setopt EXTENDED_HISTORY
  HISTFILE=~/.zsh_history
  setopt autopushd
  setopt share_history
  [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
