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
      brightnessctl networkmanager
    ];

    # Network
    networking.networkmanager.enable = true;
    users.users.jules.extraGroups = [ "networkmanager" ];

    # Video drivers
    services.xserver.xrandrHeads = [
      { output = "DP-1-1"; primary = true; }
      { output = "DP-1-2"; }
      { output = "eDP1"; }
    ];

    # This screen has too much pixels
    services.xserver.dpi = 138;

    # Other
    services.tlp.enable = true;
    hardware.cpu.intel.updateMicrocode = true;

    system.stateVersion = "19.09";
  };
}
