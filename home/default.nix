{ pkgs ? import <nixpkgs> { } }:

# nix run -f ./home -c home-manager-generation

let
  home-manager_src = pkgs.fetchgit {
    url = "https://github.com/nix-community/home-manager";
    rev = "2860d7e3bb350f18f7477858f3513f9798896831";
    branchName = "release-21.11";
    sha256 = "0zkhdf3d2pqr6lr2mibrngp0fjcpdjk9abc55kqp43mq05caq6f9";
  };

  mk_home = conf:
    (import "${home-manager_src}/home-manager/home-manager.nix" {
      inherit pkgs;
      confPath = conf;
    }).activationPackage;

in mk_home {
  imports = [ ./mpv.nix ./htop.nix ./git.nix ];

  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  services.dunst = {
    enable = true;
    configFile = ./dunstrc;
  };

  xresources.extraConfig = builtins.readFile ./xresources;

  home.file = { ".vim".source = ../vim; };

  programs.home-manager.enable = false;
  home.stateVersion = "21.11";
}
