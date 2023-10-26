{ main_user, host_name }:

{ config, pkgs, nixpkgs, home-manager, nix-gc-env, ... }@inputs:

{
  imports = [
    modules/autorandr.nix
    modules/battery_monitor.nix
    modules/desktop
    modules/display_manager.nix
    modules/gallery_wallpaper.nix
    modules/keyboard
    modules/keyring.nix
    modules/screen_off.nix
    modules/spacetelescope_wallpaper
    modules/virtualisation.nix
    home-manager.nixosModule
    nix-gc-env.nixosModules.default
  ];

  # Quiet and fast boot
  boot.initrd.verbose = false;
  boot.consoleLogLevel = 3;
  boot.kernelParams = [ "quiet" "udev.log_priority=3" ];
  boot.loader.timeout = 2;
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 10;
    editor = false;
  };
  boot.loader.efi.canTouchEfiVariables = true;
  networking.dhcpcd.wait = "background"; # Don't wait for dhcp before starting session

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;

  # Enable sound.
  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };
  # Disable socket activation, which is annoying with bluetooth and break web
  # applications on first launch.
  services.pipewire.socketActivation = false;
  systemd.user.services.pipewire.wantedBy = [ "graphical-session.target" ];
  systemd.user.services.pipewire-pulse.wantedBy = [ "graphical-session.target" ];

  # Locale
  networking.hostName = host_name;
  time.timeZone = "Europe/Paris";
  i18n.supportedLocales = [ "fr_FR.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ./packages) ];

  # The same nixpkgs used to build the system. No channel.
  # Link nixpkgs at an arbitrary path so currently running programs can start
  # using the new version as soon as the system switches.
  # No need to reboot to take $NIX_PATH changes (it doesn't change).
  environment.etc.nixpkgs.source = nixpkgs;
  environment.etc."nixpkgs-overlay/overlays.nix".text = ''
    import ${./packages}
  '';
  # Pin nixpkgs in the flake registry too
  nix.registry.nixpkgs.flake = nixpkgs;

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
    curl gnumake zip unzip jq fd ripgrep git
    python3 sqlite
    # Admin
    mkpasswd rsync
    htop acpi
    gnupg gitAndTools.gitRemoteGcrypt
    rclone git-annex git-annex-remote-rclone
    encfs-gpg
    # Apps
    gimp
    thunderbird
    google-chrome
    fluffychat
    # Desktop
    dmenu
    pavucontrol xclip
    networkmanager
    # Other
    nixos-deploy graphviz yt-dlp
  ];

  programs.vim = {
    defaultEditor = true;
    package = pkgs.vim_configurable;
  };
  environment.variables.VISUAL = "gvim";

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

  modules.virtualisation = {
    enable = false;
    user = main_user;
  };

  # Main user
  users.users."${main_user}" = {
    isNormalUser = true;
    extraGroups = [ "docker" "dialout" "adbusers" "audio" "networkmanager" ];
  };
  home-manager.users."${main_user}" = import ./home;
  home-manager.extraSpecialArgs = {
    inherit (inputs) nur_rycee vim_plugins;
  };

  home-manager.useGlobalPkgs = true;
  home-manager.useUserPackages = true;

  # Modules
  modules.desktop.enable = true;
  modules.display_manager = { enable = true; user = main_user; };
  modules.gallery_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.keyring.enable = true;
  modules.screen_off = { enable = true; locked = 15; unlocked = 3000; };

  # Automatic GC
  nix.gc = {
    automatic = true;
    dates = "weekly";
    delete_generations = "+5";
  };

  # Enable xdg portals
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal ];
  };

  # Flatpak
  services.flatpak.enable = true;

  # "multi-user.target" shouldn't wait on "network-online.target"
  systemd.targets.network-online.wantedBy = pkgs.lib.mkForce [];
}
