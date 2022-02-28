{ config, pkgs, lib, ... }:

let
  conf = config.modules.display_manager;

in {
  options.modules.display_manager = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    user = mkOption { type = types.str; };
  };

  config = lib.mkIf conf.enable {
    # Make light-locker available to the users
    environment.systemPackages = [ pkgs.lightlocker ];

    services.xserver.displayManager = {
      # After the greeter:
      # Starts light-locker
      sessionCommands = ''
        ${pkgs.lightlocker}/bin/light-locker &
      '';

      # Login prompt, use mini greeter
      lightdm.greeters.mini = {
        enable = true;
        user = conf.user;
        extraConfig = ''
          [greeter]
          show-password-label = false
          password-alignment = left
        '';
      };
    };
  };
}
