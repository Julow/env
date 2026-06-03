{ config, pkgs, lib, nixpkgs, home-manager, nix-gc-env, ... }@inputs:

# NixOS configuration not related to a specific machine. Included from host/*.

let
  # Returns the content of a directory as a list of paths
  readDir_paths = dir:
    lib.mapAttrsToList (n: _: dir + "/${n}") (builtins.readDir dir);

  # Every other files and directories in nixos/
  modules = lib.filter (p: p != ./default.nix) (readDir_paths ./.);

  main_user_opts = { lib, ... }:
    with lib; {
      options.main_user = mkOption { type = types.str; };
      options.host_name = mkOption { type = types.str; };
      config = { };
    };

in {
  imports = modules ++ [
    home-manager.nixosModules.home-manager
    nix-gc-env.nixosModules.default
    main_user_opts
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

  networking.networkmanager.enable = true;
  hardware.bluetooth.enable = true;

  # Enable sound.
  xdg.sounds.enable = false; # Disable bell sounds
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
  };
  # Disable socket activation, which is annoying with bluetooth and break web
  # applications on first launch.
  services.pipewire.socketActivation = false;
  systemd.user.services.pipewire.wantedBy = [ "graphical-session.target" ];
  systemd.user.services.pipewire-pulse.wantedBy = [ "graphical-session.target" ];

  # Locale
  networking.hostName = config.host_name;
  time.timeZone = "Europe/Paris";
  i18n.defaultLocale = "fr_FR.UTF-8";
  i18n.supportedLocales = [ "fr_FR.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];

  # Nixpkgs config and package overrides
  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [ (import ../packages) ];

  # The same nixpkgs used to build the system. No channel.
  # Link nixpkgs at an arbitrary path so currently running programs can start
  # using the new version as soon as the system switches.
  # No need to reboot to take $NIX_PATH changes (it doesn't change).
  environment.etc.nixpkgs.source = nixpkgs;
  environment.etc."nixpkgs-overlay/overlays.nix".text = ''
    import ${../packages}
  '';
  # Pin nixpkgs in the flake registry too
  nix.registry.nixpkgs.flake = nixpkgs;

  nix.nixPath = [
    "nixpkgs=/etc/nixpkgs"
    "nixpkgs-overlays=/etc/nixpkgs-overlay"
  ];

  # Enable flakes
  nix.package = pkgs.nixVersions.stable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
  programs.nix-ld.enable = true; # Needed to use the androidsdk

  environment.systemPackages = with pkgs; [
    # Base tools
    curl gnumake zip unzip jq fd ripgrep git
    python3 sqlite nixfmt
    pkgs.android-tools
    # Admin
    mkpasswd rsync
    htop acpi
    gnupg git-remote-gcrypt
    rclone git-annex git-annex-remote-rclone
    encfs-gpg
    # Apps
    gimp
    google-chrome
    libreoffice
    # Desktop
    dmenu
    pavucontrol xclip
    networkmanager
    pcmanfm
    celluloid yt-dlp
  ];

  programs.vim = {
    enable = true;
    defaultEditor = true;
    package = pkgs.vim-full;
  };
  environment.variables.VISUAL = "gvim";

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  fonts.packages = with pkgs; [
    fira-code
  ];

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  modules.virtualisation = {
    enable = false;
    user = config.main_user;
  };

  services.flatpak.enable = false;

  # Enabled by default for some reasons. Frees 1GB
  services.speechd.enable = lib.mkForce false;

  # Main user
  users.users."${config.main_user}" = {
    isNormalUser = true;
    extraGroups = [
      "docker" "dialout" "adbusers" "audio" "networkmanager" "systemd-journal"
      "scanner" "lp"
    ];
  };
  home-manager.users."${config.main_user}" = import ../home;

  home-manager = {
    extraSpecialArgs = {
      inherit (inputs) nur_rycee vim_plugins;
    };
    backupFileExtension = "hm-backup";
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  # Modules
  modules.desktop.enable = true;
  modules.display_manager = { enable = true; user = config.main_user; };
  modules.gallery_wallpaper.enable = true;
  modules.keyboard.enable = true;
  modules.screen_off = { enable = true; locked = 15; unlocked = 3000; };

  # Power management
  powerManagement.enable = true;
  services.thermald.enable = true;

  # services.printing.enable = true;
  # services.printing.drivers = with pkgs; [ cnijfilter2 ];
  # hardware.printers = {
  #   ensurePrinters = [
  #     {
  #       name = "Canon_MG3600_series";
  #       location = "Home";
  #       deviceUri = "usb://Canon/MG3600%20series?serial=76B321&interface=1";
  #       model = "canonmg3600.ppd";
  #       ppdOptions.PageSize = "A4";
  #     }
  #   ];
  #   ensureDefaultPrinter = "Canon_MG3600_series";
  # };
  # hardware.sane.enable = true; # Scanners

  # Automatic GC
  nix.gc = {
    automatic = true;
    dates = "weekly";
    delete_generations = "+5";
  };

  systemd.network.wait-online.enable = false;
  # "multi-user.target" shouldn't wait on "network-online.target"
  systemd.targets.network-online.wantedBy = pkgs.lib.mkForce [];
}
