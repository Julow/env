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

  };

  config = lib.mkIf conf.enable {
    environment.systemPackages = [ pkgs.autorandr ];

    # Before the greeter
    services.xserver.displayManager.setupCommands = ''
      ${pkgs.autorandr}/bin/autorandr --change &
    '';

    # Autorandr service
    services.autorandr.enable = true;

    # Install configuration files
    environment.etc."xdg/autorandr".source =
      pkgs.linkFarm "xdg-autorandr" (hooks ++ profiles);

  };
}
