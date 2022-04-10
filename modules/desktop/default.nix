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

    # TODO: Don't add to global environment
    environment.systemPackages =
      [ tools pkgs.xterm pkgs.dunst pkgs.playerctl pkgs.autorandr ];
  };
}
