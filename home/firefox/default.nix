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

    package = pkgs.firefox.override {
      extraPolicies."3rdparty".Extensions = {
        "uBlock0@raymondhill.net" = {
          # uBlock settings are written in JSON to be more compatible with the
          # backup format. This checks the syntax.
          adminSettings =
            builtins.fromJSON (builtins.readFile ./ublock-settings.json);
        };
      };
    };
  };
}
