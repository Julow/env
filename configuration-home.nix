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

    # Seems necessary for bluetooth headset mode (call)
    hardware.enableAllFirmware = true;

  };
}
