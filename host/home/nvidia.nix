{ pkgs, config, ... }:

# https://nixos.wiki/wiki/Nvidia

# # Support for 32bit games
# hardware.opengl.driSupport32Bit = true;
# hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
# services.pipewire.alsa.support32Bit = true;

{
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  services.xserver.videoDrivers = [ "nvidia" ];

  # Unfortunately, the open drivers are not available for this card.
  hardware.nvidia = {
    nvidiaSettings = false;
    powerManagement.enable = true;
  };
}
