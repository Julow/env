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

    services.picom = { enable = true; };

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
