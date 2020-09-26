{ config, lib, pkgs, ... }:

with lib;

# Fetches wallpapers from https://www.spacetelescope.org/

let script =
  with pkgs;
  runCommand "spacetelescope_wallpaper" {
    buildInputs = [ makeWrapper ];
  } ''
    makeWrapper ${./spacetelescope_wallpaper.sh} $out/bin/spacetelescope_wallpaper.sh \
      --prefix PATH : ${makeBinPath [ feh curl ]}
  '';
in

let conf = config.modules.spacetelescope_wallpaper; in

{
  options.modules.spacetelescope_wallpaper = {
    enable = mkEnableOption "spacetelescope_wallpaper";
  };

  config = mkIf conf.enable {
    services.xserver.displayManager.sessionCommands = ''
      ${script}/bin/spacetelescope_wallpaper.sh &
    '';
  };
}
