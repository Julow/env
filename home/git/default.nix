{ pkgs, config, ... }: {
  home.file.".config/git/config".source = ./.gitconfig;
  home.file.".config/git/ignore".source = ./.gitignore;
}
