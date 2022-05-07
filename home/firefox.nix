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
      bookmarks = {
        Wikipedia = {
          keyword = "w";
          url =
            "https://www.wikipedia.org/w/index.php?title=Special:Search&search=%s";
        };
        "Google maps" = {
          keyword = "m";
          url = "https://www.google.com/maps?q=%s";
        };
        Thesaurus = {
          keyword = "the";
          url = "https://www.thesaurus.com/browse/%s";
        };
        "English dictionary" = {
          keyword = "def";
          url = "https://www.dictionary.com/browse/%s";
        };
        Github = {
          keyword = "gh";
          url = "https://github.com/search?q=%s";
        };
        "OCaml packages" = {
          keyword = "oca";
          url = "https://ocaml.org/packages/search?q=%s";
        };
      };

      settings = import ./firefox-prefs.nix;

      userChrome = builtins.readFile ./firefox-userChrome.css;
    };
  };
}
