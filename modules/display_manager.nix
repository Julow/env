{ config, pkgs, lib, ... }:

let conf = config.modules.display_manager; in

{
  options.modules.display_manager = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    user = mkOption {
      type = types.str;
    };
  };

  config = lib.mkIf conf.enable {
    # Make light-locker available to the users
    environment.systemPackages = [ pkgs.lightlocker ];

    # Enable autorandr
    services.autorandr.enable = true;

    services.xserver.displayManager = {
      # Starts light-locker
      # Run autorandr once
      sessionCommands = ''
        ${pkgs.lightlocker}/bin/light-locker &
        ${pkgs.autorandr}/bin/autorandr --change &
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
