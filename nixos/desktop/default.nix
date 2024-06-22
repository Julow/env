{ config, pkgs, lib, ... }:

let
  conf = config.modules.desktop;

  tools = pkgs.callPackage ./tools { };

in {
  options.modules.desktop = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf conf.enable {
    # Xmonad
    services.xserver = {
      desktopManager.xterm.enable = false; # Required
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./xmonad.hs;
      };
    };
    services.displayManager.defaultSession = "none+xmonad";

    services.xserver.enable = true;
    services.libinput.enable = true; # Touchpad support
    services.picom.enable = true;

    # TODO: Don't add to global environment
    environment.systemPackages = with pkgs; [
      tools
      xterm
      dunst
      playerctl
      autorandr
      xdotool
      networkmanager_dmenu
    ];
  };
}
