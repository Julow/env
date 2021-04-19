{ main_user, host_name, extra_config }:

{ config, pkgs, ... }:

{
  imports = [
    modules/spacetelescope_wallpaper.nix
    modules/keyboard
    modules/display_manager.nix
    modules/desktop.nix
    modules/screen_off.nix
    modules/games.nix
    modules/autorandr.nix
    extra_config
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.timeout = 2;
  # Quiet boot
  boot.initrd.verbose = false;
  boot.consoleLogLevel = 3;
  boot.kernelParams = [ "quiet" "udev.log_priority=3" ];
  networking.dhcpcd.wait = "background"; # Don't wait for dhcp before starting session

  # Enable sound.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;

    media-session.config.bluez-monitor.properties.bluez5 = {
      msbc-support = true;
      sbc-xq-support = true;
    };
  };

  # Bluetooth
  hardware.bluetooth.enable = true;

  networking.hostName = host_name;
  time.timeZone = "Europe/Paris";

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ./packages) ];

  # The same nixpkgs used to build the system. No channel.
  # Link nixpkgs at an arbitrary path so currently running programs can start
  # using the new version as soon as the system switches.
  # No need to reboot to take $NIX_PATH changes (it doesn't change).
  environment.etc.nixpkgs.source = pkgs.lib.cleanSource <nixpkgs>;
  environment.etc.nixpkgs-overlay.source = pkgs.lib.cleanSource ./packages;

  nix.nixPath = [
    "nixpkgs=/etc/nixpkgs"
    "nixpkgs-overlays=/etc/nixpkgs-overlay"
  ];

  environment.systemPackages = with pkgs; [
    # Base tools
    curl gnumake zip unzip jq
    # Admin
    mkpasswd rsync
    htop acpi
    gnupg gitAndTools.gitRemoteGcrypt encfs
    # Dev
    rlwrap fd ack tree cloc
    vim_configurable git tig
    gcc_multi binutils-unwrapped
    python3 perl
    flamegraph
    # Apps
    firefox slack
    pinta
    # Desktop
    dunst xdotool playerctl dmenu pamixer
    pipewire.pulse pavucontrol mpv xclip
    msmtp
    # Other
    nixos-deploy nix-workspaces
    opam2nix
    imagemagick graphviz
  ];

  # Gpg with Yubikey support
  programs.gnupg.agent = { enable = true; };
  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;

  # Adb, need "adbusers" group
  programs.adb.enable = true;

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  users.users."${main_user}" = {
    isNormalUser = true;
    initialPassword = "test";
    extraGroups = [ "docker" "dialout" "adbusers" "audio" ];
  };

  modules.spacetelescope_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.display_manager = { enable = true; user = main_user; };
  modules.desktop.enable = true;
  modules.screen_off = { enable = true; locked = 15; unlocked = 3000; };
  # modules.games.enable = true;

  # Enable xdg portals
  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    extraPortals = [ pkgs.xdg-desktop-portal ];
  };

  # Flatpak
  services.flatpak.enable = true;

  # Quick service for dunst, until https://github.com/NixOS/nixpkgs/pull/58209 is merged
  systemd.user.services.dunst.serviceConfig.ExecStart = [ "" "${pkgs.dunst}/bin/dunst" ];

  # "multi-user.target" shouldn't wait on "network-online.target"
  systemd.targets.network-online.wantedBy = pkgs.lib.mkForce [];
}
