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

  # Screens
  services.xserver.dpi = 128;

  # Specify audio format, USB DAC is not reliable otherwise.
  # https://wiki.archlinux.org/title/PipeWire#High_latency_with_USB_DACs_(e.g._Schiit_DACs)
  services.pipewire.media-session.config.alsa-monitor.rules = [
    {
      # Matches all cards
      matches = [{ "node.name" = "~alsa_output.*"; }];
      actions.update-props = {
        "audio.format" = "S24_3LE";
        "audio.rate" = 96000;
        "audio.channels" = 2;
        "api.alsa.period-size" = 256;
      };
    }
  ];

  system.stateVersion = "21.11";
}
