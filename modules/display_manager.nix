{ config, pkgs, lib, ... }:

let
  conf = config.modules.display_manager;

  # not recursively. Suitable input to linkFarm
  read_dir = dir:
    lib.mapAttrsToList (name: _: {
      inherit name;
      path = "${conf.autorandr_config}/${name}";
    }) (builtins.readDir dir);

  autorandr_profiles = if conf.autorandr_config == null then
    [ ]
  else
    read_dir conf.autorandr_config;

  autorandr_hooks = [{
    name = "postswitch.d/notify";
    path = pkgs.writeShellScript "autorandr-notify" ''
      ${pkgs.dunst}/bin/dunstify -a "Autorandr" -u low "$AUTORANDR_CURRENT_PROFILE"
    '';
  }];

in {
  options.modules.display_manager = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    user = mkOption { type = types.str; };

    autorandr_config = mkOption {
      type = types.nullOr types.path;
      default = null;
    };
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

      # Before the greeter:
      # Run autorandr
      setupCommands = ''
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

    # Autorandr
    services.autorandr.enable = true;
    environment.etc."xdg/autorandr".source =
      pkgs.linkFarm "xdg-autorandr" (autorandr_hooks ++ autorandr_profiles);
  };
}
