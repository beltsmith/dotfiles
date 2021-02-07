{ pkgs, services, ... }:

let
  # doom-emacs = pkgs.callPackage (builtins.fetchTarball {
  #   url = "https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz";
  # }) { doomPrivateDir = ~/dotfiles/doom.d; };
  langs = (import ./langs.nix) { inherit pkgs; };
  myemacs = (import ./emacs.nix) { inherit pkgs; };
  tools = (import ./tools.nix) { inherit pkgs; };
  games = (import ./games.nix) { inherit pkgs; };
  media = (import ./media.nix) { inherit pkgs; };
  virt = (import ./virt.nix) { inherit pkgs; };
  desktop = (import ./desktop.nix) {
    inherit pkgs;
    inherit services;
  };
in {
  nixpkgs.config = import dotfiles/nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = dotfiles/nixpkgs-config.nix;
  nixpkgs.overlays = let
    importOverlay = { rev, url }:
      (import
        (builtins.fetchTarball { url = "${url}/archive/${rev}.tar.gz"; }));
    nightlyOverlay = importOverlay {
      rev = "master";
      url = "https://github.com/mozilla/nixpkgs-mozilla";
    };
    emacsOverlay = importOverlay {
      rev = "master";
      url = "https://github.com/nix-community/emacs-overlay";
    };
  in [ nightlyOverlay emacsOverlay ];

  home.file.runelite = {
    source = ../runelite;
    target = "games/runelite";
    executable = true;
  };

  home.packages = with pkgs;
    langs.packages ++ games.packages ++ tools.packages ++ myemacs.packages
    ++ media.packages ++ virt.packages ++ desktop.packages ++ [
      vscode

      elvish

      etcd
      etcdctl
      docker
      docker-compose
      kubectl

      postgresql

      spotify
      cmus

      nvtop
      vulkan-headers
      vulkan-loader
      vulkan-tools

      # terminals
      kitty
      alacritty

      stow

      # libwacom xf86_input_wacom
      zoom-us

      openjdk
      # jdk12
      # jetbrains.idea-community

      heroku

      nagios
      iftop

      fuse-common
      gcc

      # polybarFull
      slack
      discord
      zulip

      # vagrant
      # virtualbox

      krita

      pandoc
      ranger

      ripgrep
      ripgrep-all

      chromedriver
      chromium
      google-chrome
      google-chrome-dev
      latest.firefox-nightly-bin
    ];

  # fonts.fonts = with pkgs; [ emacs-all-the-icons-fonts ];

  programs.home-manager.enable = true;

  services.lorri.enable = true;

  # programs.gmailieer.enable = true;

  # programs.notmuch = {
  #   enable = true;
  #   # hooks = { preNew = "mbsync --all"; };
  # };

  # accounts.email = {
  #   accounts.gmail = {
  #     address = "alexander.girdler@gmail.com";
  #     # gpg = {
  #     #   key = "F9119EC8FCC56192B5CF53A0BF4F64254BD8C8B5";
  #     #   signByDefault = true;
  #     # };
  #     imap.host = "gmail.com";
  #     # mbsync = {
  #     #   enable = true;
  #     #   create = "maildir";
  #     # };
  #     # msmtp.enable = true;
  #     notmuch.enable = true;
  #     primary = true;
  #     realName = "Alex Girdler";
  #     # signature = {
  #     #   text = ''
  #     #     Mit besten Wünschen
  #     #     Ben Justus Bals
  #     #     https://keybase.io/beb
  #     #   '';
  #     #   showSignature = "append";
  #     # };
  #     passwordCommand = "mail-password";
  #     smtp = { host = "gmail.com"; };
  #     userName = "alexander.girdler@gmail.com";
  #   };
  # };

  programs.git = {
    enable = true;
    userName = "beltsmith";
    userEmail = "me+gh@alexgirdler.com";
    extraConfig = { pull = { rebase = true; }; };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    initExtra = (builtins.readFile zsh/initExtra.zsh);
    shellAliases = {
      l = "exa -lgh";
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
      { name = "zsh-users/zsh-history-substring-search"; }
      { name = "zsh-users/zsh-completions"; }
      { name = "mafredri/zsh-async"; }
      { name = "agkozak/zsh-z"; }
      {
        name = "zsh-users/zsh-syntax-highlighting";
        tags = [ "defer:3" ];
      }
      {
        name = "sindresorhus/pure";
        tags = [ "use:pure.zsh" "as:theme" ];
      }
      {
        name = "b4b4r07/enhancd";
        tags = [ "use:init.sh" ];
      }
      # {name = "plugins/heroku"; tags = ["from:oh-my-zsh"];}
      {
        name = "plugins/kubectl";
        tags = [ "from:oh-my-zsh" ];
      }
      {
        name = "plugins/git";
        tags = [ "from:oh-my-zsh" ];
      }
      {
        name = "plugins/sudo";
        tags = [ "from:oh-my-zsh" ];
      }
      {
        name = "junegunn/fzf-bin";
        tags = [ "from:gh-r" "as:command" "rename-to:fzf" ];
      }
      {
        name = "junegunn/fzf";
        tags = [ "as:command" "use:'bin/fzf-tmux'" ];
      }
    ];
  };

  programs.tmux = {
    enable = true;
    clock24 = true;
    terminal = "screen-256color";
    baseIndex = 0;
    extraConfig = (builtins.readFile ~/dotfiles/tmux/.tmux.conf);
  };

  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
  };

  services.flameshot.enable = true;
  services.blueman-applet.enable = true;

  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    # opacityRules = [
    #   # "100:class_g = 'Firefox'"
    #   # "100:class_g = 'Vivaldi-stable'"
    #   "100:class_g = 'VirtualBox Machine'"
    #   # Art/image programs where we need fidelity
    #   "100:class_g = 'Gimp'"
    #   "100:class_g = 'Inkscape'"
    #   "100:class_g = 'aseprite'"
    #   "100:class_g = 'krita'"
    #   "100:class_g = 'feh'"
    #   "100:class_g = 'mpv'"
    #   "100:class_g = 'Rofi'"
    #   "100:class_g = 'Peek'"
    #   "99:_NET_WM_STATE@:32a = '_NET_WM_STATE_FULLSCREEN'"
    # ];
    shadowExclude = [
      # Put shadows on notifications, the scratch popup and rofi only
      "! name~='(rofi|scratch|Dunst)$'"
    ];
    # settings = {
    #   blur-background-exclude = [
    #     "window_type = 'dock'"
    #     "window_type = 'desktop'"
    #     "class_g = 'Rofi'"
    #     "_GTK_FRAME_EXTENTS@:c"
    #   ];

    #   # Unredirect all windows if a full-screen opaque window is detected, to
    #   # maximize performance for full-screen windows. Known to cause
    #   # flickering when redirecting/unredirecting windows.
    #   unredir-if-possible = true;

    #   # GLX backend: Avoid using stencil buffer, useful if you don't have a
    #   # stencil buffer. Might cause incorrect opacity when rendering
    #   # transparent content (but never practically happened) and may not work
    #   # with blur-background. My tests show a 15% performance boost.
    #   # Recommended.
    #   glx-no-stencil = true;

    #   # Use X Sync fence to sync clients' draw calls, to make sure all draw
    #   # calls are finished before picom starts drawing. Needed on
    #   # nvidia-drivers with GLX backend for some users.
    #   xrender-sync-fence = true;
    # };
  };

  services.redshift = {
    enable = true;
    latitude = "37.775";
    longitude = "-122.419";
    tray = true;
  };

  services.polybar = {
    enable = true;
    script = "polybar main &";
    config = {
      fonts = {
        primary = "FuraMono Nerd Font:style=Medium,Regular:size=10";
        secondary = "fixed:pixelsize=10;1";
        special = "siji:pixelsize=10;1";
      };
      colors = {
        background = "#222";
        background-alt = "#444";
        foreground = "#dfdfdf";
        foreground-alt = "#555";
        primary = "#ffb52a";
        secondary = "#e60053";
        alert = "#bd2c40";
      };
      "bar/main" = {
        override-redirect = true;
        width = "100%";
        height = "27";
        fixed-center = false;
        # background = "#fafafa";
        # foreground = "#dfdfdf";
        background = "\${colors.background}";
        foreground = "\${colors.foreground}";
        line-color = "#f00";
        padding-left = 0;
        padding-right = 2;
        module-margin-left = 1;
        module-margin-right = 2;
        font-0 = "\${fonts.primary}";
        font-1 = "\${fonts.secondary}";
        font-2 = "\${fonts.special}";

        modules-left = "workspaces-xmonad title-xmonad";
        modules-center = "";
        modules-right =
          "filesystem pulseaudio memory cpu eth temperature date powermenu";
        tray-position = "right";
        tray-padding = 2;

        cursor-click = "pointer";
        cursor-scroll = "ns-resize";
      };
      "module/filesystem" = {
        type = "internal/fs";
        interval = 25;

        mount-0 = "/";

        label-mounted = "%{F#0a81f5}%mountpoint%%{F-}: %percentage_used%%";
        label-unmounted = "%mountpoint% ";
        label-unmounted-foreground = "\${colors.foreground-alt}";
      };
      "module/workspaces-xmonad" = {
        type = "custom/script";
        exec = "/run/current-system/sw/bin/tail -F /tmp/.xmonad-workspace-log";
        tail = true;
      };

      "module/title-xmonad" = {
        type = "custom/script";
        exec = "/run/current-system/sw/bin/tail -F /tmp/.xmonad-title-log";
        tail = true;
      };
      "module/cpu" = {
        type = "internal/cpu";
        interval = 2;
        format-prefix = "";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#f90000";
        label = "%percentage:2%%";
      };
      "module/memory" = {
        type = "internal/memory";
        interval = 2;
        format-prefix = "";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#4bffdc";
        label = "%percentage_used%% %mb_total%";
      };
      "module/eth" = {
        type = "internal/network";
        interface = "enp0s31f6";
        interval = "3.0";

        format-connected-underline = "#55aa55";
        format-connected-prefix = "@";
        format-connected-prefix-foreground = "\${colors.foreground-alt}";
        label-connected = "%local_ip%";

        format-disconnected = "";
      };
      "module/date" = {
        type = "internal/date";
        interval = 5;

        date = "%Y-%m-%d";
        date-alt = "%Y-%m-%d";

        time = "%H:%M";
        time-alt = "%H:%M:%S";

        format-prefix = "";
        format-prefix-foreground = "\${colors.foreground-alt}";
        format-underline = "#0a6cf5";

        label = "%date% %time%";
      };
      "module/pulseaudio" = {
        type = "internal/pulseaudio";

        format-volume = "<label-volume> <bar-volume>";
        label-volume = "VOL %percentage%%";
        label-volume-foreground = "\${root.foreground}";

        label-muted = "🔇 muted";
        label-muted-foreground = "#666";

        bar-volume-width = 10;
        bar-volume-foreground-0 = "#55aa55";
        bar-volume-foreground-1 = "#55aa55";
        bar-volume-foreground-2 = "#55aa55";
        bar-volume-foreground-3 = "#55aa55";
        bar-volume-foreground-4 = "#55aa55";
        bar-volume-foreground-5 = "#f5a70a";
        bar-volume-foreground-6 = "#ff5555";
        bar-volume-gradient = false;
        bar-volume-indicator = "|";
        bar-volume-indicator-font = 2;
        bar-volume-fill = "─";
        bar-volume-fill-font = 2;
        bar-volume-empty = "─";
        bar-volume-empty-font = 2;
        bar-volume-empty-foreground = "\${colors.foreground-alt}";
      };
      "settings" = { screenchange-reload = true; };
      "global/wm" = {
        margin-top = 0;
        margin-bottom = 0;
      };
    };
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    maxCacheTtlSsh = 72000;
    maxCacheTtl = 72000;
  };

  programs.kitty = {
    enable = true;
    settings = {
      allow_remote_control = true;
      enable_audio_bell = false;
      scrolback_lines = 1000;
      cursor = "#ffe8c0";
      foreground = "#c5c8c6";
      background = "#1d1f21";
      selection_foreground = "#36363a";
      selection_background = "#ddcfbf";
      color0 = "#282a2e";
      color8 = "#373b41";
      color1 = "#a54242";
      color9 = "#cc6666";
      color2 = "#8c9440";
      color10 = "#b5bd68";
      color3 = "#de935f";
      color11 = "#f0c674";
      color4 = "#5f819d";
      color12 = "#81a2be";
      color5 = "#85678f";
      color13 = "#b294bb";
      color6 = "#5e8d87";
      color14 = "#8abeb7";
      color7 = "#707880";
      color15 = "#c5c8c6";
    };
  };

  services.pasystray.enable = true;

  services.udiskie = {
    enable = true;
    tray = "always";
  };

  services.dunst = {
    enable = true;
    settings = {
      frame = {
        width = 0;
        color = "#212121";
      };

      urgency_low = {
        background = "#1d1f21";
        foreground = "#c5c8c6";
        timeout = 10;
      };

      urgency_normal = {

        background = "#1d1f21";
        foreground = "#c5c8c6";
        timeout = 10;
      };

      urgency_critical = {
        background = "#fbc02d";
        foreground = "#000000";
        timeout = 0;

      };

      global = {

        # The format of the message.  Possible variables:
        #   %a  appname
        #   %s  summary
        #   %b  body
        #   %i  iconname (including its path)
        #   %I  iconname (without its path)
        #   %p  progress value if set ([  0%] to [100%]) or nothing
        format = ''
          %a
          <b>%s</b>
          %b
          %p'';

        # The geometry of the window. Format: [{width}]x{height}[+/-{x}+/-{y}]
        # The height = number of notifications, all other variables are px
        # Omit width, provide height for full-screen width notifications
        # If width is 0, window will fit to longest message
        # Positive x value is measured from the left of the screen, negative x is measured from the right
        # Positive y value is measured from the top of the screen
        geometry = "350-50+75";

        font = "Adobe Source Code Pro 10";
        allow_markup = true;
        plain_text = false;
        # Treat message as plain text
        sort = true;
        # Sort messages by urgency
        indicate_hidden = true;
        # Show how many messages are currently hidden (see geometry)
        alignment = "center";
        # Align text left/center/right
        bounce_freq = 0;
        # Frequency to bounce text back and forth if it is longer than the window width (conflicts with "word_wrap")
        show_age_threshold = 60;
        # Show if message is older than x seconds (-1 to disable)
        word_wrap = true;
        # Split notifications into multiple lines if they don't fit into geometry
        ignore_newline = false;
        # Ignore "\n"
        transparency = 0;
        # The transparency of the window. 0 (opaque) to 100 (transparent) - requires compositing window manager (xcompmgr, compiz, compton, etc)
        shrink = false;
        # Shrink window if it's smaller than the width (ignored if width is 0)
        monitor = 0;
        # Display notifications on the monitor indicated (0 is default)
        follow = "mouse"; # Follow mouse/keyboard/none
        show_indicators =
          false; # Display indicators for URLs (U) and actions (A)
        line_height =
          0; # The spacing between lines (forced to height of font at minimum)
        notification_height =
          0; # The height of the entire notification (forced to height of font height and padding at minimum)
        separator_height = 2; # Space in pixels between two notifications
        padding = 8; # Padding between text and separator
        horizontal_padding = 8; # Horizontal padding
        separator_color =
          "frame"; # Color for separator: auto/foreground/frame/X color
        icon_position = "right"; # Align icons left/right/off

        idle_threshold =
          120; # Don't remove messages if the user is idle (no mouse or keyboard input) for longer than idle_threshold seconds
        sticky_history =
          true; # Make notifications remain until clicked on (yes) or timeout as normal (no) when recalled from history
        history_length = 20; # Maximum amount of notifications kept in history

        icon_folders = "/usr/share/icons/hicolor/16x16";
        startup_notification = false;
        dmenu = "/usr/bin/dmenu -p dunst:";
        browser = "/usr/bin/firefox -new-tab";
        max_icon_size = 128;
      };

      shortcuts = {
        # Available modifiers are "ctrl", "mod1", "mod2", "mod3", and "mod4"
        # Xev might be helpful to find names for keys

        # Close notification
        close = "ctrl+mod4+m";

        # Close all notifications
        close_all = "ctrl+mod4+n";

        # Recall last message(s)
        history = "ctrl+mod4+p";
      };
    };
  };

  home.file = {
    ".xprofile".text = ''
      xrandr --output DVI-D-0 --mode 1920x1080 --pos 3760x0 --rotate normal --output HDMI-0 --off --output HDMI-1 --off --output DP-0 --mode 2560x1440 --pos 3440x1080 --rotate normal --output DP-1 --off --output DP-2 --primary --mode 3440x1440 --pos 0x1080 --rotate normal --output DP-3 --off
    '';
  };
  # home.file = { ".emacs.d/init.el".text = ''(load "default.el")''; };
}
