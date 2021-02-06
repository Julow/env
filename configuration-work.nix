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

    # This screen has too much pixels
    services.xserver.dpi = 138;

    # Power saving
    # https://discourse.nixos.org/t/thinkpad-t470s-power-management/8141
    services.tlp = {
      enable = true;
      extraConfig = ''
        CPU_SCALING_GOVERNOR_ON_AC=performance
        CPU_SCALING_GOVERNOR_ON_BAT=powersave
      '';
    };

    boot.initrd.availableKernelModules = [ "thinkpad_acpi" ];

    boot.extraModprobeConfig = pkgs.lib.mkMerge [
      "options snd_hda_intel power_save=1" # Enable power_save for the audio card
      "options iwlwifi power_save=1 uapsd_disable=1" # Wifi power save
    ];

    services.udev.extraRules = pkgs.lib.mkMerge [
      # Autosuspend USB and PCI devices
      ''ACTION=="add", SUBSYSTEM=="usb", TEST=="power/control", ATTR{power/control}="auto"''
      ''ACTION=="add", SUBSYSTEM=="pci", TEST=="power/control", ATTR{power/control}="auto"''
    ];

    # Other
    hardware.cpu.intel.updateMicrocode = true;

    system.stateVersion = "19.09";
  };
}
