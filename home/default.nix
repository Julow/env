{ pkgs, vim_plugins, ... }:

# # nix run -f ./home -c home-manager-generation
# mk_home = conf:
#   (import "${home-manager_src}/home-manager/home-manager.nix" {
#     inherit pkgs;
#     confPath = conf;
#   }).activationPackage;

{
  imports =
    [ ./mpv.nix ./htop.nix ./git.nix ./firefox ./gtk.nix ./thunderbird ];

  services.dunst = {
    enable = true;
    configFile = ./dunstrc;
  };

  xresources.extraConfig = builtins.readFile ./xresources;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
  };

  home.file.".vim".source = vim_plugins.dot_vim;

  programs.less.enable = true;
  home.sessionVariables.LESS = "--RAW-CONTROL-CHARS --ignore-case --tabs=4 --use-color -DBr-$Cr-$ --jump-target=5 --mouse --wheel-lines=3";

  programs.home-manager.enable = false;
  home.stateVersion = "21.11";
}
