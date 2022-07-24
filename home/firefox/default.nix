{ pkgs, config, nur_rycee, ... }:
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
