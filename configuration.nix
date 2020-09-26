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
    extra_config
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.timeout = 2;
  networking.dhcpcd.wait = "background"; # Don't wait for dhcp before starting session

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # Required for bluetooth audio

  # Bluetooth
  hardware.bluetooth.enable = true;

  networking.hostName = host_name;
  time.timeZone = "Europe/Paris";

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = (import ./packages/overlay.nix);

  # The same nixpkgs used to build the system. No channel
  nix.nixPath = [
    "nixpkgs=${<nixpkgs>}"
    "nixpkgs-overlays=${./packages}/overlay.nix"
  ];

  environment.systemPackages = with pkgs; [
    # Base tools
    curl gnumake zip unzip file jq
    rlwrap fd ack fzf tree cloc
    # Admin
    mkpasswd rsync
    htop acpi
    gnupg gitAndTools.gitRemoteGcrypt encfs
    # Dev
    vim_configurable git tig
    gcc_multi binutils-unwrapped
    opam ocaml
    python3 perl
    flamegraph
    # Apps
    firefox thunderbird spotify tdesktop
    # Desktop
    dunst xdotool dmenu playerctl
    pavucontrol mpv
    # Other
    opam2nix nixos-deploy
    imagemagick
    rss_to_mail
  ];

  # Gpg with Yubikey support
  programs.gnupg.agent = { enable = true; };
  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  users.users."${main_user}" = {
    isNormalUser = true;
    initialPassword = "test";
    extraGroups = [ "docker" ];
  };

  modules.spacetelescope_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.display_manager = { enable = true; user = main_user; };
  modules.desktop.enable = true;
  modules.screen_off = { enable = true; locked = 15; unlocked = 3000; };
  modules.games.enable = true;

  # Quick service for dunst, until https://github.com/NixOS/nixpkgs/pull/58209 is merged
  systemd.user.services.dunst.serviceConfig.ExecStart = [ "" "${pkgs.dunst}/bin/dunst" ];
}
