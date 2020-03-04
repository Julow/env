{ config, pkgs, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    modules/apod_wallpaper
    modules/keyboard
    modules/display_manager.nix
    modules/desktop.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull; # Required for bluetooth audio

  # Bluetooth
  hardware.bluetooth.enable = true;

  # Network
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # networking.hostName = "nixos"; # Define your hostname.
  time.timeZone = "Europe/Paris";

  # Video drivers
  services.xserver.videoDrivers = [ "nvidia" ];

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ./packageOverlay.nix) ];

  environment.systemPackages = with pkgs; [
    firefox
    curl gnumake mkpasswd zip file
    vim_configurable git
    gnupg gitAndTools.gitRemoteGcrypt encfs
    dunst htop
    fd ack fzf
    python3
    opam ocaml
    vlc spotifyd playerctl
  ];

  users.users.juloo = {
    isNormalUser = true;
    initialPassword = "test";
  };

  modules.apod_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.display_manager = { enable = true; user = "juloo"; };
  modules.desktop.enable = true;

  system.stateVersion = "19.09";
}
