{ pkgs, config, ... }:
let home = config.home.homeDirectory;
in {
  gtk.gtk3.bookmarks =
    [ "file://${home}/Downloads" "file://${home}/notes/drive" ];
}
