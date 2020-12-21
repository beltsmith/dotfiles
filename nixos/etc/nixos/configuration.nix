# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    (import "${
        builtins.fetchTarball
        "https://github.com/nix-community/home-manager/archive/master.tar.gz"
      }/nixos")
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_5_8;

  networking = {
    hostName = "abydos"; # Define your hostname.
    # hostName = "cheyenne-mtn"; # Define your hostname.
    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    # wireless.userControlled.enable = true;
    # wireless.networks.StarGateCommand = {
    #   pskRaw = "35b3c38c28eb73494e89ee73bf52efa9b110c62c604e4d7d66f0230197da23de";
    # };

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.enp0s31f6.useDHCP = true;
    interfaces.wlp2s0.useDHCP = true;

    extraHosts = ''
      127.0.0.1     cheyenne-mtn
      ::1           cheyenne-mtn
      127.0.1.1     cheyenne-mtn.localdomain cheyenne-mtn

      10.0.0.3      tower tower.local
      10.0.0.3      sonarr.tower sonarr.tower.local
      10.0.0.3      radarr.tower radarr.tower.local
      10.0.0.3      nrp.tower nrp.tower.local
      10.0.0.3      nzb.tower nzb.tower.local
      10.0.0.3      torrent.tower torrent.tower.local
      10.0.0.3      plex.tower plex.tower.local
      127.0.0.1     www.emacs.local

      127.0.0.1     www.inhouse.dev www.inhouse.local
      127.0.0.1     inhouse.dev inhouse.local

      127.0.0.1     assets.prevail.dev assets.prevail.local
      127.0.0.1     www.prevail.dev www.prevail.local
      127.0.0.1     prevail.dev prevail.local
      54.147.21.178 prevail-ivr

      127.0.0.1     mustacheriders.dev mustacheriders.local
    '';

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    nameservers = [ "1.1.1.1" "9.9.9.9" ];
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = lib.optionalString (config.nix.package == pkgs.nixFlakes)
      "experimental-features = nix-command flakes";
    # package = pkgs.nixFlakes;
    # extraOptions = lib.optionalString (config.nix.package == pkgs.nixFlakes) ''
    #   keep-outputs = true
    #   keep-derivations = true
    #   experimental-features = nix-command flakes
    # '';
  };

  services.openvpn.servers = {
    piaSV = { config = "config /home/belt/openvpn/us_silicon_valley.ovpn "; };
    #  piaCA = { config = "config /home/belt/openvpn/us_california.ovpn "; };
  };

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the Plasma 5 Desktop Environment.
  services.xserver.enable = true;
  services.xserver.wacom.enable = true;
  services.xserver.displayManager.sddm.enable = true;
  # services.xserver.videoDrivers = [ "nvidiaVulkanBeta" ];

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };

  # Configure keymap in X11
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  };
  hardware.pulseaudio.support32Bit = true;

  # steam
  systemd.extraConfig = "DefaultLimitNOFILE=1048576";
  hardware.steam-hardware.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.belt = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "networkmanager"
      "vboxusers"
      "docker"
    ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
  };

  users.extraGroups.vboxusers.members = [ "belt" ];

  environment.pathsToLink = [ "/share/nix-direnv" ];

  # Enable non-free software
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
  };
  nixpkgs.overlays = let
    # Change this to a rev sha to pin
    moz-rev = "master";
    moz-url = builtins.fetchTarball {
      url =
        "https://github.com/mozilla/nixpkgs-mozilla/archive/${moz-rev}.tar.gz";
    };
    nightlyOverlay = (import "${moz-url}/firefox-overlay.nix");
  in [ nightlyOverlay ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    home-manager
    direnv
    nix-direnv
    wireguard
  ];

  fonts.fonts = with pkgs; [ nerdfonts ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.zsh.enable = true;

  programs.java = {
    enable = true;
    package = pkgs.openjdk14;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  virtualisation.virtualbox.host = {
    enable = true;
    enableExtensionPack = true;
  };
  virtualisation.docker.enable = true;

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    enableTCPIP = true;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all ::1/128 trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE ROLE belt WITH LOGIN PASSWORD 'belt' CREATEDB;
      CREATE DATABASE belt;
      GRANT ALL PRIVILEGES ON DATABASE belt TO belt;
    '';
  };

  services.redis.enable = true;

  # security.acme = {
  #   acceptTerms = true;
  #   email = "me@alexgirdler.com";
  # };

  services.nginx = {
    enable = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

    virtualHosts."prevail.local" = {
      enableACME = true;
      forceSSL = true;
      locations."/" = {
        proxyPass = "http://localhost:3000";
        proxyWebsockets = true; # needed if you need to use WebSocket
        extraConfig =
          # required when the target is also TLS server with multiple hosts
          "proxy_ssl_server_name on;" +
          # required when the server wants to use HTTP Authentication
          "proxy_pass_header Authorization;";
      };
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
