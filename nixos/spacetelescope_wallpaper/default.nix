{ config, lib, pkgs, ... }:

with lib;

let conf = config.modules.spacetelescope_wallpaper; in

{
  options.modules.spacetelescope_wallpaper = {
    enable = mkEnableOption "spacetelescope_wallpaper";
  };

  config = mkIf conf.enable {
    systemd.user.services.spacetelescope_wallpaper = {
      enable = true;
      wants = [ "display-manager.service" "network-online.target" ];
      after = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      path = with pkgs; [ feh curl ];
      serviceConfig = {
        Type = "oneshot";
      };
      script = ''
        ${pkgs.bash}/bin/bash ${./spacetelescope_wallpaper.sh}
      '';
    };
  };
}
