{ pkgs, services, ... }:

{
  packages = with pkgs; [
    feh

    synergy

    libpulseaudio
    # (polybar.override { pulseSupport = true; })
    polybar
    rofi
    dunst

    arandr
    blueman
    flameshot
    picom
    redshift
    pasystray
    udiskie
    pavucontrol
    nitrogen

    lastpass-cli

    lxappearance
    shades-of-gray-theme
    numix-gtk-theme
    numix-icon-theme
    gnome3.adwaita-icon-theme

    font-awesome-ttf
    material-design-icons

    xorg.xfontsel
    xorg.xkill
    xorg.libxcb
  ];

  config = {
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
          # mount-1 = "/home";

          label-mounted = "%{F#0a81f5}%mountpoint%%{F-}: %percentage_used%%";
          label-unmounted = "%mountpoint% ";
          label-unmounted-foreground = "\${colors.foreground-alt}";
        };
        "module/workspaces-xmonad" = {
          type = "custom/script";
          exec =
            "/run/current-system/sw/bin/tail -F /tmp/.xmonad-workspace-log";
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
    };
  };
}
