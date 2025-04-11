{ pkgs, config, ... }:
let home = config.home.homeDirectory;
in {
  xdg.configFile."gtk-3.0/bookmarks".text = ''
    file://${home}/Downloads
    file://${home}/notes/drive
  '';
}
