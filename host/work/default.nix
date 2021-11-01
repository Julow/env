{ pkgs, ... }:

{
  imports = [
    (import ../../common.nix { main_user = "jules"; host_name = "jules-work"; })
    /etc/nixos/hardware-configuration.nix
    ../../nixos-hardware/lenovo/thinkpad/x1/6th-gen
  ];

  # Extra packages
  environment.systemPackages = with pkgs; [
    brightnessctl networkmanager
  ];

  # Network
  networking.networkmanager.enable = true;
  users.users.jules.extraGroups = [ "networkmanager" ];

  # This screen has too much pixels
  services.xserver.dpi = 152;

  services.autorandr = {
    enable = true;
    profiles = {
      enable = true;
      profiles = etc/autorandr;
    };
  };

  modules.battery_monitor.enable = true;

  # Power saving
  services.tlp = {
    enable = true;
    settings = {
      CPU_SCALING_GOVERNOR_ON_AC = "performance";
      CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
    };
  };

  boot.initrd.availableKernelModules = [ "thinkpad_acpi" ];

  # Other
  hardware.cpu.intel.updateMicrocode = true;

  system.stateVersion = "19.09";
}
