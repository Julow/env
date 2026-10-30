{ pkgs, vim_plugins, ... }:

# # nix run -f ./home -c home-manager-generation
# mk_home = conf:
#   (import "${home-manager_src}/home-manager/home-manager.nix" {
#     inherit pkgs;
#     confPath = conf;
#   }).activationPackage;

{
  imports = [
    ./mpv.nix
    ./htop.nix
    ./git
    ./firefox
    ./pcmanfm.nix
    ./thunderbird
    ./less.nix
  ];

  services.dunst = {
    enable = true;
    configFile = ./dunstrc;
  };

  xresources.extraConfig = builtins.readFile ./xresources;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    pinentry.package = pkgs.pinentry-gnome3;
  };

  xdg.userDirs.enable = true;
  xdg.userDirs.setSessionVariables = false;

  home.file.".vim".source = vim_plugins.dot_vim;

  programs.home-manager.enable = false;
  home.stateVersion = "21.11";
}
