(import ./configuration.nix) {

  host_name = "jules-work";
  main_user = "jules";

  extra_config = {
    imports = [
      nixos-hardware/lenovo/thinkpad/x1/6th-gen
    ];

    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # Network
    networking.useDHCP = false;
    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp2s0.useDHCP = true;
    networking.interfaces.wlan0.useDHCP = true;
    networking.wireless.iwd.enable = true; # Enable wireless support via iwd

    # Video drivers
    services.xserver.videoDrivers = [ "intel" ];

    # This screen has too much pixels
    services.xserver.dpi = 110;

  };
}
