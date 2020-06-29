(import ./configuration.nix) {

  host_name = "jules-pc";
  main_user = "juloo";

  extra_config = { pkgs, ... }: {
    imports = [
      /etc/nixos/hardware-configuration.nix
    ];

    # Network
    networking.useDHCP = false;
    networking.interfaces.eno1.useDHCP = true;

    # Video drivers
    services.xserver.videoDrivers = [ "nvidia" ];

    # Screens
    services.xserver.dpi = 108;
    services.xserver.xrandrHeads = [
      { output = "HDMI-0"; }
      { output = "DP-1"; primary = true; }
    ];

    # Seems necessary for bluetooth headset mode (call)
    hardware.enableAllFirmware = true;

    sound.extraConfig = ''
      options snd-hda-intel model=generic
    '';

  };
}
