{ pkgs, config, ... }: {
  programs.git = {
    enable = true;
    ignores =
      [ "*.swp" ".vimrc" "_opam" "__pycache__" "*.pyc" "_build" "/result" ];
    extraConfig = builtins.readFile ./gitconfig;
  };
}
