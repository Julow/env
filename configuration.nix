{ main_user, host_name, extra_config }:

{ config, pkgs, ... }:

{
  imports = [
    modules/apod_wallpaper
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
    firefox thunderbird
    spotify
    tdesktop
    gcc_multi binutils-unwrapped
    curl gnumake mkpasswd zip unzip file jq
    vim_configurable git rlwrap tig tree cloc
    gnupg gitAndTools.gitRemoteGcrypt encfs
    htop
    dunst xdotool dmenu imagemagick
    pavucontrol
    fd ack fzf
    python3
    opam ocaml
    flamegraph
    mpv playerctl
    opam2nix
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

  modules.apod_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.display_manager = { enable = true; user = main_user; };
  modules.desktop.enable = true;
  modules.screen_off = { enable = true; locked = 15; unlocked = 3000; };
  modules.games.enable = true;

  system.stateVersion = "19.09";
}
