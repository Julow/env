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
      rev = "f29f5646ec92d86360a847be64d192b92192b14e";
      sha256 = "18r0m30h4195cmnga1aqydy5cnkb7wx157k6lk1j5hsywa1acbqp";
    }) {};

};
in

[ overlay ]
