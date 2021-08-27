{ main_user, host_name, extra_config }:

{ config, pkgs, ... }:

{
  imports = [
    modules/spacetelescope_wallpaper
    modules/keyboard
    modules/display_manager.nix
    modules/desktop.nix
    modules/screen_off.nix
    modules/autorandr.nix
    modules/battery_monitor.nix
    extra_config
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.editor = false;
  boot.loader.timeout = 2;
  # Quiet boot
  boot.initrd.verbose = false;
  boot.consoleLogLevel = 3;
  boot.kernelParams = [ "quiet" "udev.log_priority=3" ];
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
  environment.etc.nixpkgs.source = pkgs.lib.cleanSource <nixpkgs>;
  environment.etc.nixpkgs-overlay.source = pkgs.lib.cleanSource ./packages;

  nix.nixPath = [
    "nixpkgs=/etc/nixpkgs"
    "nixpkgs-overlays=/etc/nixpkgs-overlay"
  ];

  environment.systemPackages = with pkgs; [
    # Base tools
    curl gnumake zip unzip jq
    vim_configurable git
    python3
    # Admin
    mkpasswd rsync
    htop acpi
    gnupg gitAndTools.gitRemoteGcrypt encfs
    # Apps
    firefox
    pinta
    # Desktop
    dunst xdotool playerctl dmenu pamixer
    pipewire.pulse pavucontrol mpv xclip
    # Other
    nixos-deploy nix-workspaces
    imagemagick graphviz
  ];

  fonts = {
    fonts = with pkgs; [
      fira-code
    ];
  };

  # Gpg with Yubikey support
  programs.gnupg.agent = { enable = true; };
  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.pcscd.enable = true;

  # Adb, need "adbusers" group
  programs.adb.enable = true;

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  users.users."${main_user}" = {
    isNormalUser = true;
    initialPassword = "test";
    extraGroups = [ "docker" "dialout" "adbusers" "audio" ];
  };

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

  # Quick service for dunst, until https://github.com/NixOS/nixpkgs/pull/58209 is merged
  systemd.user.services.dunst.serviceConfig.ExecStart = [ "" "${pkgs.dunst}/bin/dunst" ];

  # "multi-user.target" shouldn't wait on "network-online.target"
  systemd.targets.network-online.wantedBy = pkgs.lib.mkForce [];

  # Support for 32bit games
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  services.pipewire.alsa.support32Bit = true;
}
