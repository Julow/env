{ pkgs, ... }:

# # nix run -f ./home -c home-manager-generation
# mk_home = conf:
#   (import "${home-manager_src}/home-manager/home-manager.nix" {
#     inherit pkgs;
#     confPath = conf;
#   }).activationPackage;

{
  imports = [ ./mpv.nix ./htop.nix ./git.nix ./firefox ./vim.nix ];

  services.dunst = {
    enable = true;
    configFile = ./dunstrc;
  };

  xresources.extraConfig = builtins.readFile ./xresources;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
  };

  programs.home-manager.enable = false;
  home.stateVersion = "21.11";
}
