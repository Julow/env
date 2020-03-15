self: super:

with super;

{
  dunst = dunst.override { dunstify = true; };

  xterm = xterm.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ "--enable-exec-xterm" ];
  });

  ocamlPackages = self.ocaml-ng.ocamlPackages_4_08;

  opam2nix =
    let src = import (builtins.fetchTarball "https://github.com/timbertson/opam2nix/archive/v1.tar.gz"); in
    self.callPackage src {};

  rss_to_mail = self.callPackage packages/rss_to_mail {};
}
