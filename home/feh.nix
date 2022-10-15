{ pkgs, config, ... }: {
  home.file.".config/feh/themes".text = ''
    feh -Z --draw-filename -.
  '';
}
