{ pkgs, ... }:

let
  nixos-hardware = builtins.fetchGit {
    url = "https://github.com/NixOS/nixos-hardware";
    rev = "267d8b2d7f049d2cb9b7f4a7f981c123db19a868";
  };

in {
  imports = [
    (import ../../common.nix {
      main_user = "jules";
      host_name = "jules-work";
    })
    ./hardware-configuration.nix
    "${nixos-hardware}/lenovo/thinkpad/x1/6th-gen"
  ];

  # Extra packages
  environment.systemPackages = with pkgs; [ networkmanager ];

  # Network
  networking.networkmanager.enable = true;
  users.users.jules.extraGroups = [ "networkmanager" ];

  # Boot
  boot.loader.grub.device = "nodev";
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.editor = false;

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
