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

  # The HDD only contains the boot partition because the mother board can't
  # boot from the SSD and can be turned off to reduce noise and power.
  # hdparm -S 5 doesn't set the standaby timer to 25s as expected but to 10min
  # on this hard drive. Use a systemd service to turn the disk off immediately
  # after boot and set the timer to 10min if it ever needed to wake up.
  systemd.services.sda_sleep = {
    wantedBy = [ "multi-user.target" ];
    script = ''
      ${pkgs.hdparm}/bin/hdparm -S 5 -y /dev/sda
    '';
  };
  # Set "noatime" to avoid waking up before shutdown when the file system
  # wasn't modified.
  fileSystems."/boot".options = [ "defaults" "noatime" ];

  system.stateVersion = "21.11";
}
