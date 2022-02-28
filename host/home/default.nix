{ pkgs, config, ... }:

{
  imports = [
    (import ../../common.nix { main_user = "juloo"; host_name = "jules-pc"; })
    ./hardware-configuration.nix
  ];

  boot.loader.grub.device = "nodev";
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.editor = false;

  # Users
  users.users.juloo.hashedPassword = "$6$mY7jXn2tECcVwABv$YcVZDNh5yYf65D6A44dM6LNxFszu0Ipw9M73VSzMfHVuliJ4t7ZlOoGz3cduPnIwZ8/k.jImkfoy88xO.Poxl1";
  users.users.root.hashedPassword = "$6$q5rAlai/sjItj4hJ$0hCDqYPOe37sarAEvfnXnXUufprIlaXFyFVerQ.Ew5LSGgTKnAjLEvpUsA3YfyRfQa1kvuXxJ3tsFvYDNUdZV0";
  users.mutableUsers = false;

  # Network
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  # Video drivers
  services.xserver.videoDrivers = [ "nvidia" ];
  # https://nixos.wiki/wiki/Nvidia
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;

  # Screens
  services.xserver.dpi = 128;

  # Seems necessary for bluetooth headset mode (call)
  hardware.enableAllFirmware = true;

  sound.extraConfig = ''
    options snd-hda-intel model=generic
  '';

  system.stateVersion = "21.11";
}
