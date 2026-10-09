{ pkgs, config, lib, ... }:
{
  home.file = {
    ".config/git/config".text = lib.readFile ./gitconfig;
    ".config/git/ignore".text = lib.readFile ./gitignore;
  };
}
