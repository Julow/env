{ config, pkgs, lib, ... }:

# Allow declaring global autorandr configuration.

let
  conf = config.modules.autorandr;

  # not recursively. Suitable input to linkFarm
  read_dir = dir:
    lib.mapAttrsToList (name: _: {
      inherit name;
      path = "${conf.config}/${name}";
    }) (builtins.readDir dir);

  profiles = read_dir conf.config;

  default_profile = if conf.default_profile == null then
    [ ]
  else [{
    name = "default";
    path = conf.default_profile;
  }];

  hooks = if conf.notify_hook then [{
    name = "postswitch.d/notify";
    path = pkgs.writeShellScript "autorandr-notify" conf.notify_cmd;
  }] else
    [ ];

in {
  options.modules.autorandr = with lib; {
    enable = mkEnableOption "modules.autorandr";

    config = mkOption {
      type = types.path;
      description = "Path to autorandr profiles.";
    };

    notify_hook = mkOption {
      type = types.bool;
      default = true;
      description =
        "Whether to send a notification when the profile is changed. See option 'notify_cmd'.";
    };

    notify_cmd = mkOption {
      type = types.str;
      default = ''
        ${pkgs.dunst}/bin/dunstify -a "Autorandr" -u low "$AUTORANDR_CURRENT_PROFILE"
      '';
    };

    default_profile = mkOption {
      type = types.nullOr types.str;
      default = "vertical";
      description = ''
        The profile 'default' will be a symlink to this profile.
        It will be used as the default profile when autorandr is run with the arguments '--default default'.
        Can be the name of a built-in profile, a defined profile, an absolute path or 'null'.
      '';
    };

  };

  config = lib.mkIf conf.enable {
    environment.systemPackages = [ pkgs.autorandr ];

    # Before the greeter
    services.xserver.displayManager.setupCommands = ''
      ${pkgs.autorandr}/bin/autorandr --default default --change &
    '';

    # Autorandr service
    services.autorandr.enable = true;

    # Install configuration files
    environment.etc."xdg/autorandr".source =
      pkgs.linkFarm "xdg-autorandr" (hooks ++ default_profile ++ profiles);

  };
}
