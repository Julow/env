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

  # Screens
  services.xserver.dpi = 128;

  # Power off HDD when it's not in use
  # The HDD contains the boot partition because the mother board can't boot on
  # the SSD.
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="block", KERNEL=="sda", ATTR{queue/rotational}=="1", RUN+="${pkgs.hdparm}/bin/hdparm -S 5 /dev/sda"
  '';

  system.stateVersion = "21.11";
}
