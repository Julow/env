{ config, lib, pkgs, ... }:

# Pick the wallpaper randomly from pictures stored in ~/Pictures/wallpaper.
# Runs twice a week.

with lib;

let
  conf = config.modules.gallery_wallpaper;

  feh = pkgs.feh.override { inherit (pkgs) imlib2; };

  refresh_wallpaper = pkgs.writeShellScript "refresh_wallpaper" ''
    ${feh}/bin/feh --bg-fill --recursive --randomize ~/Pictures/wallpaper
  '';

in {
  options.modules.gallery_wallpaper = {
    enable = mkEnableOption "gallery_wallpaper";
  };

  config = mkIf conf.enable {
    # Set the selected wallpaper on startup. '.fehbg' is a script generated by
    # 'feh' that restore the previous wallpaper. Select a new wallpaper if it
    # doesn't exist.
    services.xserver.displayManager.sessionCommands = ''
      if [[ -e ~/.fehbg ]]; then ~/.fehbg & else ${refresh_wallpaper} & fi
    '';

    # Reduce closure size
    services.xserver.desktopManager.wallpaper.enable = false;

    # Refresh the wallpaper twice a week.
    systemd.user.services.gallery_wallpaper = {
      serviceConfig = {
        Type = "oneshot";
        ExecStart = refresh_wallpaper;
      };
    };

    systemd.user.timers.gallery_wallpaper = {
      wants = [ "display-manager.service" ];
      after = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      timerConfig = {
        OnCalendar = "Mon,Thu *-*-* 8:00:00";
        Persistent = true;
      };
    };
  };
}
