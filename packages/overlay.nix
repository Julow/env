let overlay = self: super: with super; {

  xterm = xterm.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ "--enable-exec-xterm" ];
  });

  ocamlPackages = self.ocaml-ng.ocamlPackages_4_10;

  opam2nix = self.callPackage ./opam2nix.nix {};

  rss_to_mail = self.callPackage ./rss_to_mail.nix {};

  nixos-deploy = self.callPackage ./nixos-deploy.nix {};

  nix-workspaces = self.callPackage (pkgs.fetchgit {
      url = "https://github.com/Julow/nix-workspaces";
      rev = "0f7070e2dbe6ad5866af353e68ff071c77f4d434";
      sha256 = "0dhcb0pcyj6ylcl5bkyj5h4jvfmkavwnh4m0sa9iq1l60sa5v30x";
    }) {};

};
in

[ overlay ]
