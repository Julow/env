(import ./configuration.nix) {

  host_name = "jules-work";
  main_user = "jules";

  extra_config = {
    imports = [
      # Requires https://github.com/NixOS/nixos-hardware
      # This laptop definitely has quirks
      <nixos-hardware/lenovo/thinkpad/x1/6th-gen>
    ];

    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # Network
    networking.useDHCP = false;
    networking.wireless.enable = true; # Enables wireless support via wpa_supplicant.
    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp2s0.useDHCP = true;

    # Video drivers
    services.xserver.videoDrivers = [ "intel" ];

  };
}
