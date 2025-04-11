{ pkgs, config, ... }: {
  xdg.configFile."gtk-3.0/bookmarks".text = ''
    file:///home/juloo/Downloads
    file:///home/juloo/notes/drive
  '';
}
