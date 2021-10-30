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
      displayManager.defaultSession = "none+xmonad";
      desktopManager.xterm.enable = false; # Required
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./xmonad.hs;
      };

    };
  };
}
