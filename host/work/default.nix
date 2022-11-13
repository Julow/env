{ pkgs, nixos-hardware, ... }:

{
  imports = [
    (import ../../common.nix {
      main_user = "jules";
      host_name = "jules-work";
    })
    ./hardware-configuration.nix
    nixos-hardware.nixosModules.lenovo-thinkpad-x1-6th-gen
  ];

  # Boot
  boot.loader.grub.device = "nodev";
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.editor = false;

  # This screen has too much pixels
  services.xserver.dpi = 160;

  services.autorandr = {
    enable = true;
  };

  modules.autorandr_profiles = {
    enable = true;
    profiles = etc/autorandr;
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
