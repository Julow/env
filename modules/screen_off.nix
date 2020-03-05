{ config, pkgs, lib, ... }:

# Timeout before the screen is turned off

let conf = config.modules.screen_off; in

let set_off_time_cmd =
  off_sec:
    "${pkgs.xorg.xset}/bin/xset dpms 0 0 ${toString off_sec}";
in

{
  options.modules.screen_off = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    locked = mkOption {
      type = types.int;
      default = 15;
    };

    unlocked = mkOption {
      type = types.int;
      default = 3600;
    };
  };

  config = lib.mkIf conf.enable {

    # Display manager starts
    services.xserver.displayManager.setupCommands = ''
      ${set_off_time_cmd conf.locked}
    '';

    # Window manager starts
    services.xserver.displayManager.sessionCommands = ''
      ${set_off_time_cmd conf.unlocked}
    '';

  };
}
