{ config, pkgs, lib, ... }:

let conf = config.modules.desktop; in

{
  options.modules.desktop = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf conf.enable {
    services.xserver = {
      enable = true;
      libinput.enable = true; # Touchpad support

      # Xmonad
      windowManager.default = "xmonad";
      desktopManager.xterm.enable = false; # Required
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };

      # Disable screen going off
      serverFlagsSection = ''
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime"     "0"
        Option "BlankTime"   "0"
      '';
    };

  };
}
