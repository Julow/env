(import ./configuration.nix) {

  host_name = "jules-pc";
  main_user = "juloo";

  extra_config = {
    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # Network
    networking.useDHCP = false;
    networking.interfaces.eno1.useDHCP = true;

    # Video drivers
    services.xserver.videoDrivers = [ "nvidia" ];

  };
}
