{ pkgs, config, ... }: {
  xdg.configFile."gtk-3.0/bookmarks".text = ''
    file:///home/juloo/Downloads
    file:///tmp/screenshot
    file:///home/juloo/notes/stash
    file:///home/juloo/notes/drive
  '';
}
