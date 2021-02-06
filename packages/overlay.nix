let overlay = self: super: with super; {

  dunst = dunst.override { dunstify = true; };

  xterm = xterm.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ "--enable-exec-xterm" ];
  });

  ocamlPackages = self.ocaml-ng.ocamlPackages_4_10;

  opam2nix = self.callPackage ./opam2nix.nix {};

  rss_to_mail = self.callPackage ./rss_to_mail.nix {};

  nixos-deploy = self.callPackage ./nixos-deploy.nix {};

  nix-workspaces = self.callPackage (pkgs.fetchgit {
      url = "https://github.com/Julow/nix-workspaces";
      rev = "db356cbaa787c399139e817187e97183104cbb10";
      sha256 = "1lxr0rkg0yxn9km2hjryn0rf4a62jqrnfyjhdrx4jc5r9kgj67bj";
    }) {};

};
in

[ overlay ]
