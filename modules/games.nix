{ config, pkgs, lib, ... }:

let conf = config.modules.games; in

{
  options.modules.games = with lib; {
    enable = mkEnableOption "modules.games";
  };

  config = lib.mkIf conf.enable {
    environment.systemPackages = with pkgs; [
      steam
    ];

    networking.firewall.allowedUDPPorts = [
      34197 # Factorio
    ];

    # Support for 32bit games
    hardware.opengl.driSupport32Bit = true;
    hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    hardware.pulseaudio.support32Bit = true;
    services.pipewire.alsa.support32Bit = true;
  };
}
