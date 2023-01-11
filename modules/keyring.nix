{ config, pkgs, lib, ... }:

# Using gnome-keyring without the rest of Gnome.

let conf = config.modules.keyring;

in {
  options.modules.keyring = with lib; { enable = mkEnableOption "keyring"; };

  config = lib.mkIf conf.enable {
    environment.systemPackages = with pkgs; [ gnome3.seahorse ];
    services.gnome.gnome-keyring.enable = true;
    security.pam.services.lightdm.enableGnomeKeyring = true;
    programs.ssh.startAgent = true;
  };
}
