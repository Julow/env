{ main_user, host_name }:

{ config, pkgs, nixpkgs, home-manager, ... }@inputs:

{
  imports = [
    modules/spacetelescope_wallpaper
    modules/keyboard
    modules/display_manager.nix
    modules/desktop
    modules/screen_off.nix
    modules/autorandr.nix
    modules/battery_monitor.nix
    home-manager.nixosModule
  ];

  # Quiet and fast boot
  boot.initrd.verbose = false;
  boot.consoleLogLevel = 3;
  boot.kernelParams = [ "quiet" "udev.log_priority=3" ];
  boot.loader.timeout = 2;
  boot.loader.grub.configurationLimit = 20; # Don't keep an unlimited number of systems
  networking.dhcpcd.wait = "background"; # Don't wait for dhcp before starting session

  # Enable sound.
  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;

    # Bluetooth
    media-session.config.bluez-monitor.rules = [
      {
        # Matches all cards
        matches = [{ "device.name" = "~bluez_card.*"; }];
        actions = {
          "update-props" = {
            "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
            "bluez5.msbc-support" = true;
            "bluez5.sbc-xq-support" = true;
          };
        };
      }
      {
        # Matches all sources and all outputs
        matches = [
          { "node.name" = "~bluez_input.*"; }
          { "node.name" = "~bluez_output.*"; }
        ];
        actions = { "node.pause-on-idle" = false; };
      }
    ];
  };

  # Bluetooth
  hardware.bluetooth.enable = true;

  networking.hostName = host_name;
  time.timeZone = "Europe/Paris";

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ./packages) ];

  # The same nixpkgs used to build the system. No channel.
  # Link nixpkgs at an arbitrary path so currently running programs can start
  # using the new version as soon as the system switches.
  # No need to reboot to take $NIX_PATH changes (it doesn't change).
  environment.etc.nixpkgs.source = pkgs.lib.cleanSource nixpkgs;
  environment.etc."nixpkgs-overlay/overlays.nix".text = ''
    import ${./packages}
  '';

  nix.nixPath = [
    "nixpkgs=/etc/nixpkgs"
    "nixpkgs-overlays=/etc/nixpkgs-overlay"
  ];

  # Enable flakes
  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';

  environment.systemPackages = with pkgs; [
    # Base tools
    curl gnumake zip unzip jq
    vim_configurable git
    python3
    # Admin
    mkpasswd rsync
    htop acpi
    gnupg gitAndTools.gitRemoteGcrypt
    encfs-gpg
    # Apps
    firefox
    pinta
    thunderbird
    # Desktop
    xdotool dmenu
    pipewire.pulse pavucontrol mpv xclip
    # Other
    nixos-deploy
    graphviz
  ];

  fonts = {
    fonts = with pkgs; [
      fira-code
    ];
  };

  # Adb, need "adbusers" group
  programs.adb.enable = true;

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  # Main user
  users.users."${main_user}" = {
    isNormalUser = true;
    extraGroups = [ "docker" "dialout" "adbusers" "audio" ];
  };
  home-manager.users."${main_user}" = import ./home;
  home-manager.extraSpecialArgs = {
    inherit (inputs) nur_rycee;
  };

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  # Modules
  modules.spacetelescope_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.display_manager = { enable = true; user = main_user; };
  modules.desktop.enable = true;
  modules.screen_off = { enable = true; locked = 15; unlocked = 3000; };

  # Enable xdg portals
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal ];
  };

  # Flatpak
  services.flatpak.enable = true;

  # "multi-user.target" shouldn't wait on "network-online.target"
  systemd.targets.network-online.wantedBy = pkgs.lib.mkForce [];

  # Support for 32bit games
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  services.pipewire.alsa.support32Bit = true;
}
