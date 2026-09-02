{
  pkgs,
  config,
  lib,
  nur_rycee,
  ...
}:
let

  inherit (pkgs.callPackage nur_rycee { }) firefox-addons;

  awesome-rss = firefox-addons.buildFirefoxXpiAddon {
    pname = "awesome-rss";
    version = "1.3.5";
    addonId = "{97d566da-42c5-4ef4-a03b-5a2e5f7cbcb2}";
    url = "https://addons.mozilla.org/firefox/downloads/file/1124727/awesome_rss-1.3.5.xpi";
    sha256 = "sha256-/DxiUy1kYrwmn26p+mG7ytyJi9r89m4W4VvsZrsJTZs=";
    meta = { };
  };

  userChrome = lib.concatMapStringsSep "\n" builtins.readFile [
    ./userChrome.css
  ];

  mk_profile = id: {
    inherit id;
    # Force remove bookmarks that were previously configured that way
    bookmarks.force = true;
    bookmarks.settings = [ ];
    settings = import ./prefs.nix;
    inherit userChrome;

    search = {
      force = true;
      default = "ddg-lite";
      privateDefault = "ddg-lite";
      engines."ddg-lite" = {
        name = "DuckDuckGo Lite";
        urls = [
          {
            template = "https://lite.duckduckgo.com/lite/";
            params = [
              {
                name = "q";
                value = "{searchTerms}";
              }
            ];
          }
        ];
        iconMapObj."16" = "https://lite.duckduckgo.com/favicon.ico";
      };
    };

    extensions.packages = with firefox-addons; [
      ublock-origin
      privacy-badger
      vimium
      clearurls
      awesome-rss
    ];
  };

in
{
  programs.firefox = {
    enable = true;
    configPath = ".mozilla/firefox";
    languagePacks = [ "fr" ];

    profiles.hm = mk_profile 0;
    profiles.work = mk_profile 1;

    # Policies: https://mozilla.github.io/policy-templates/
    policies = {
      # Clear cookies when the browser exits.
      Cookies.Allow = [
        "https://github.com"
        "https://discuss.ocaml.org"
        "https://www.mediapart.fr"
        "https://boardgamearena.com"
        "https://deezer.com"
        "https://web.whatsapp.com"
        "https://leboncoin.fr"
        "https://slack.com"
        "https://discord.com"
        "https://lichess.org"
      ];
      SanitizeOnShutdown = true; # Clear history on exit

      # Disable unecessary features
      AppAutoUpdate = false;
      BackgroundAppUpdate = false;
      DisableFirefoxStudies = true;
      DisableFirefoxScreenshots = true;
      DisableForgetButton = true;
      DisableMasterPasswordCreation = true;
      DisableProfileImport = true;
      DisableProfileRefresh = true;
      DisableSetDesktopBackground = true;
      DisablePocket = true;
      DisableTelemetry = true;
      DisableFormHistory = true;
      DisablePasswordReveal = true;
      DontCheckDefaultBrowser = true;
      OfferToSaveLogins = false;

      "3rdparty".Extensions."uBlock0@raymondhill.net".adminSettings = builtins.fromJSON (
        builtins.readFile ./ublock-settings.json
      );
    };
  };
}
