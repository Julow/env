{ pkgs, config, lib, nur_rycee, ... }:
let

  inherit (pkgs.callPackage nur_rycee { }) firefox-addons;

  awesome-rss = firefox-addons.buildFirefoxXpiAddon {
    pname = "awesome-rss";
    version = "1.3.5";
    addonId = "{97d566da-42c5-4ef4-a03b-5a2e5f7cbcb2}";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/1124727/awesome_rss-1.3.5.xpi";
    sha256 = "sha256-/DxiUy1kYrwmn26p+mG7ytyJi9r89m4W4VvsZrsJTZs=";
    meta = { };
  };

  redirector = firefox-addons.buildFirefoxXpiAddon {
    pname = "redirector";
    version = "3.5.3";
    addonId = "redirector@einaregilsson.com";
    url =
      "https://addons.mozilla.org/firefox/downloads/file/3535009/redirector-3.5.3.xpi";
    sha256 = "sha256-7dvT1ZROdI0L1uy22enPDgwC3O1vQtshqrZBkOccD3E=";
    meta = { };
  };

  cascade_theme = pkgs.fetchgit {
    url = "https://github.com/andreasgrafen/cascade";
    rev = "a72936632ca48dcaed983286cee68688fc2f8eb2";
    sha256 = "0v3mhxg7z4mziv71k4q83x47vyly30ja9wl6dsmp3zq6i9z9z6ah";
  };

  userChrome = lib.concatMapStringsSep "\n" builtins.readFile [
    ./userChrome.css
    "${cascade_theme}/chrome/includes/cascade-config-mouse.css"
    "${cascade_theme}/chrome/includes/cascade-colours.css"
    "${cascade_theme}/chrome/includes/cascade-layout.css"
    "${cascade_theme}/chrome/includes/cascade-responsive.css"
    "${cascade_theme}/chrome/includes/cascade-floating-panel.css"
    "${cascade_theme}/chrome/includes/cascade-nav-bar.css"
    "${cascade_theme}/chrome/includes/cascade-tabs.css"
  ];

in {
  programs.firefox = {
    enable = true;

    extensions = with firefox-addons; [
      ublock-origin
      privacy-badger
      vimium
      clearurls
      awesome-rss
      redirector
    ];

    profiles.hm = {
      bookmarks = import ./bookmarks.nix;
      settings = import ./prefs.nix;
      inherit userChrome;
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
