{ config, lib, pkgs, ... }:

with lib;

# Fetches today's Astronomical Picture Of the Day and sets it as wallpaper

let apod_script =
  with pkgs;
  runCommand "apod_wallpaper" {
    buildInputs = [ makeWrapper ];
  } ''
    makeWrapper ${./apod_wallpaper.sh} $out/bin/apod_wallpaper.sh \
      --prefix PATH : ${makeBinPath [ jq feh curl ]}
  '';
in

let conf = config.modules.apod_wallpaper; in

{
  options.modules.apod_wallpaper = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf conf.enable {
    services.xserver.displayManager.sessionCommands = ''
      ${apod_script}/bin/apod_wallpaper.sh &
    '';
  };
}
