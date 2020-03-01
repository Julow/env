{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModprobeConfig = ''
    options hid_apple swap_opt_cmd=1
  '';
  boot.kernelModules = [ "hid-apple" ];

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  time.timeZone = "Europe/Paris";

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: with pkgs; {
    dunst = dunst.override { dunstify = true; };
  };

  environment.systemPackages = with pkgs; [
    firefox rxvt_unicode
    htop curl gnumake wget vim_configurable git mkpasswd
    dunst htop xclip xorg.xev
    fd ack fzf
    gnupg gitAndTools.gitRemoteGcrypt python3 encfs
    zip file vlc spotifyd playerctl
    opam ocaml
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # Required for bluetooth audio

  hardware.bluetooth.enable = true;

  services.xserver = {
    enable = true;
    libinput.enable = true; # Touchpad support
    videoDrivers = [ "nvidia" ];

    # Xmonad
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false; # Required
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

    # Keyboard
    layout = "us";
    displayManager.sessionCommands =
      let custom_keymap = pkgs.runCommand "keymap.xkb" {} ''
          ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${./keymap.xkb} "$out" 2>/dev/null
        '';
      in
      ''
        ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${custom_keymap} "$DISPLAY"
        ${pkgs.xcape}/bin/xcape -e Shift_R=space
      '';

    # Disable screen going off
    serverFlagsSection = ''
      Option "StandbyTime" "0"
      Option "SuspendTime" "0"
      Option "OffTime"     "0"
      Option "BlankTime"   "0"
    '';
  };

  users.users.juloo = {
    isNormalUser = true;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
