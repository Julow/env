{ config, pkgs, lib, ... }:

# Keyboard configuration
# - Custom keymap
# - Use xcape to turn space into shift
# - Load the hid-apple kernel module

let conf = config.modules.keyboard; in

# Check errors in the keymap at build time
let custom_keymap =
  pkgs.runCommand "keymap.xkb" {} ''
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${./keymap.xkb} "$out" 2>/dev/null
  '';
in

{
  options.modules.keyboard = with lib; {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

  };

  config = lib.mkIf conf.enable {
    boot.extraModprobeConfig = "options hid_apple swap_opt_cmd=1";
    boot.kernelModules = [ "hid-apple" ];

    services.xserver = {
      layout = "us";

      displayManager.sessionCommands = ''
        ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${custom_keymap} "$DISPLAY"
        ${pkgs.xcape}/bin/xcape -e Shift_R=space &
      '';

    };
  };
}
