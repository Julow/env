let
  # Shorthand for defining a keyword bookmark
  kw = keyword: url: {
    name = keyword;
    inherit keyword url;
  };

in [
  (kw "w"
    "https://www.wikipedia.org/w/index.php?title=Special:Search&search=%s")
  (kw "m" "https://www.google.com/maps?q=%s")
  (kw "gh" "https://github.com/search?q=%s")
  (kw "oca" "https://ocaml.org/packages/search?q=%s")
  (kw "en" "https://www.deepl.com/fr/translator#fr/en/%s")
  (kw "fr" "https://www.deepl.com/fr/translator#en/fr/%s")
  (kw "conj" "https://conjugaison.lemonde.fr/conjugaison/search?verb=%s")
  (kw "android"
    "https://developer.android.com/s/results?q=%s&all_languages=true&text")
]
