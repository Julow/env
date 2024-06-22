{ config, pkgs, lib, ... }:

let conf = config.modules.virtualisation;

in {
  options.modules.virtualisation = with lib; {
    enable = mkEnableOption "virtualisation";
    user = mkOption { type = types.str; };
  };

  config = lib.mkIf conf.enable {
    users.users."${conf.user}" = {
      extraGroups = [ "libvirtd" ];
    };

    virtualisation.libvirtd = {
      enable = true;
      onBoot = "ignore";
      qemu.runAsRoot = false;
    };

    # Enable kvm
    boot.extraModprobeConfig = ''
      options kvm_intel nested=1
      options kvm_intel emulate_invalid_guest_state=0
      options kvm ignore_msrs=1
    '';
  };
}
