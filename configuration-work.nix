(import ./configuration.nix) {

  host_name = "jules-work";
  main_user = "jules";

  extra_config = { pkgs, ... }: {
    imports = [
      /etc/nixos/hardware-configuration.nix
      nixos-hardware/lenovo/thinkpad/x1/6th-gen
    ];

    # Extra packages
    environment.systemPackages = with pkgs; [
      brightnessctl slack networkmanager
    ];

    # Network
    networking.useDHCP = false;
    networking.interfaces.enp0s31f6.useDHCP = true;
    networking.interfaces.wlp2s0.useDHCP = true;
    networking.interfaces.wlan0.useDHCP = true;
    networking.wireless.iwd.enable = true; # Enable wireless support via iwd

    networking.networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };

    users.users.jules.extraGroups = [ "networkmanager" ];

    # Video drivers
    services.xserver.videoDrivers = [ "intel" ];

    # This screen has too much pixels
    services.xserver.dpi = 138;

  };
}
