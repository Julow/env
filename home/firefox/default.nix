{ pkgs, config, nur_rycee, ... }:

{
  programs.firefox = {
    enable = true;

    extensions = with (pkgs.callPackage nur_rycee { }).firefox-addons; [
      ublock-origin
      privacy-badger
      vimium
      clearurls
    ];

    profiles.hm = {
      bookmarks = import ./bookmarks.nix;
      settings = import ./prefs.nix;
      userChrome = builtins.readFile ./userChrome.css;
    };
  };
}
