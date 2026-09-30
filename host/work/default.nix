{
  pkgs,
  nixos-hardware,
  lib,
  ...
}:

let
  main_user = "jules";
in
{
  imports = [
    ../../nixos
    ./hardware-configuration.nix
    nixos-hardware.nixosModules.lenovo-thinkpad-x1-6th-gen
  ];

  inherit main_user;
  host_name = "jules-work";

  services.xserver.dpi = 160;
  services.libinput.touchpad.accelSpeed = "0.7";
  services.autorandr.enable = true;

  modules.autorandr_profiles = {
    enable = true;
    profiles = etc/autorandr;
  };

  services.xserver.displayManager.lightdm.extraConfig = ''
    display-setup-script = ${pkgs.autorandr} --default default --change
  '';

  # Auto login because the disk is encrypted
  services.xserver.displayManager.lightdm.greeters.mini.enable = lib.mkForce false;
  services.displayManager.autoLogin = {
    enable = true;
    user = main_user;
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

  # Users
  users.users.jules.hashedPassword = "$y$j9T$6xTJCo5/OPSVZUSVTgoem.$ByzkXeNFR/i6LhytD30zDMGMVwDtOEG0VeOuAoLfi28";
  users.users.root.hashedPassword = "$y$j9T$RF8MiWxUXjB/k53DeUEXA/$np1s/L3SszNn6NqJCATAR2iR1dpWEP5ZdJMf2TPn4Q3";
  users.mutableUsers = false;

  boot.initrd.availableKernelModules = [ "thinkpad_acpi" ];

  # Other
  hardware.cpu.intel.updateMicrocode = true;

  system.stateVersion = "19.09";
}
