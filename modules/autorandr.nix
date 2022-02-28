{ config, pkgs, lib, ... }:

# Allow declaring global autorandr configuration.
# Extends 'services.autorandr'

let
  conf = config.services.autorandr.profiles;

  # not recursively. Suitable input to linkFarm
  read_dir = dir:
    lib.mapAttrsToList (name: _: {
      inherit name;
      path = "${conf.profiles}/${name}";
    }) (builtins.readDir dir);

  profiles = read_dir conf.profiles;

  default_profile = if conf.default_profile == null then
    [ ]
  else [{
    name = "default";
    path = conf.default_profile;
  }];

  hooks = if conf.notify_hook.enable then [{
    name = "postswitch.d/notify";
    path = pkgs.writeShellScript "autorandr-notify" conf.notify_hook.cmd;
  }] else
    [ ];

in {
  options.services.autorandr.profiles = with lib; {
    enable = mkEnableOption "";

    profiles = mkOption {
      type = types.path;
      description = "Path to autorandr profiles.";
    };

    notify_hook = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to send a notification when the profile is changed.";
      };

      cmd = mkOption {
        type = types.str;
        default = ''
          ${pkgs.libnotify}/bin/notify-send -a "Autorandr" -u low "$AUTORANDR_CURRENT_PROFILE"
        '';
      };
    };

    default_profile = mkOption {
      type = types.nullOr types.str;
      default = "vertical";
      description = ''
        The profile 'default' will be a symlink to the specified profile.
        The default profile might be used when autorandr is run with the arguments '--default default', as it is commonly the case.
        Can be the name of a built-in profile, of a defined profile, an absolute path or 'null'.
      '';
    };

  };

  config = lib.mkIf (config.services.autorandr.enable && conf.enable) {
    # Before the greeter
    services.xserver.displayManager.setupCommands = ''
      ${pkgs.autorandr}/bin/autorandr --default default --change &
    '';

    # Install configuration files
    environment.etc."xdg/autorandr".source =
      pkgs.linkFarm "xdg-autorandr" (hooks ++ default_profile ++ profiles);

  };
}
