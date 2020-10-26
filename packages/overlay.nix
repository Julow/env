let overlay = self: super: with super; {

  dunst = dunst.override { dunstify = true; };

  xterm = xterm.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ "--enable-exec-xterm" ];
  });

  ocamlPackages = self.ocaml-ng.ocamlPackages_4_10;

  opam2nix = self.callPackage ./opam2nix.nix {};

  rss_to_mail = self.callPackage ./rss_to_mail.nix {};

  nixos-deploy = self.callPackage ./nixos-deploy.nix {};

};
in

[ overlay ]
