{
  config,
  pkgs,
  lib,
  ...
}:

let
  conf = config.modules.display_manager;

in
{
  options.modules.display_manager = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    user = mkOption { type = types.str; };
  };

  config = lib.mkIf conf.enable {
    services.xserver.displayManager = {
      lightdm.enable = true;

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

    # Screen locking
    programs.xss-lock = {
      enable = true;
      lockerCommand = "${pkgs.writeShellScript "xsecurelock-wrapped" ''
        # Stop picom while xsecurelock is running. The alternative of setting
        # XSECURELOCK_COMPOSITE_OBSCURER would make it less secure.
        ${pkgs.systemd}/bin/systemctl --user stop picom.service
        trap "${pkgs.systemd}/bin/systemctl --user start picom.service" EXIT
        ${pkgs.xsecurelock}/bin/xsecurelock
      ''}";
    };
  };
}
