{ config, pkgs, lib, ... }:

# Keyboard configuration
# - Custom keymap
# - Run xcape
# - Enable mousekeys

let conf = config.modules.keyboard; in

let xcape_expr = "Shift_R=space;Control_L=Escape;Control_R=Escape;Overlay1_Enable=Tab;Overlay2_Enable=Multi_key"; in

{
  options.modules.keyboard = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = lib.mkIf conf.enable {
    services.xserver = {
      layout = "custom-qwerty";

      extraLayouts.custom-qwerty = {
        description = "Custom qwerty layout";
        languages = [ "en" ];
        symbolsFile = ./keymap.xkb;
      };
    };

    # Use a service so it can be restarted by the user
    # xcape does nothing if setxkbmap has never been called since boot.
    systemd.user.services.keyboard = {
      enable = true;
      wants = [ "display-manager.service" ];
      wantedBy = [ "graphical-session.target" ];
      script = ''
        ${pkgs.xorg.setxkbmap}/bin/setxkbmap
        ${pkgs.xkbset}/bin/xkbset m
        ${pkgs.xkbset}/bin/xkbset exp =m
        exec ${pkgs.xcape}/bin/xcape -f -e "${xcape_expr}"
      '';
    };

    services.xbanish = {
      enable = true;
      arguments = "-m se -i mod1 -i shift";
    };

  };
}
