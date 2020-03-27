(import ./configuration.nix) {

  host_name = "jules-pc";
  main_user = "juloo";

  extra_config = { pkgs, ... }: {
    # Use the systemd-boot EFI boot loader.
    boot.loader.systemd-boot.enable = true;
    boot.loader.efi.canTouchEfiVariables = true;

    # Network
    networking.useDHCP = false;
    networking.interfaces.eno1.useDHCP = true;

    # Video drivers
    services.xserver.videoDrivers = [ "nvidia" ];

    # Extra packages
    environment.systemPackages = with pkgs; [ steam ];

    # Support for 32bit games
    hardware.opengl.driSupport32Bit = true;
    hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    hardware.pulseaudio.support32Bit = true;

  };
}
