self: super:
with super;

let callGitPackage = args: self.callPackage (pkgs.fetchgit args) { };

in {

  xterm = xterm.overrideAttrs (attrs: {
    configureFlags = attrs.configureFlags ++ [ "--enable-exec-xterm" ];
  });

  ocamlPackages = self.ocaml-ng.ocamlPackages_4_10;

  opam2nix = callGitPackage {
    url = "https://github.com/timbertson/opam2nix";
    rev = "d1d4cb15131bf45c9402aeff7df05dc78a92add7";
    sha256 = "1v65w65hiqjcgcgavjgcc6nbdjw3c1rcnpripp86ni5gmry548p1";
  };

  rss_to_mail = (callGitPackage {
    url = "https://github.com/Julow/rss_to_mail";
    rev = "5abe83bd73c33e9d377140042ddd44b75c852aca";
    sha256 = "02k2rdccsdw61c54hqw07162pkki829a1wys662563g087b3ww2r";
  }).rss_to_mail;

  nixos-deploy = callGitPackage {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "9332940f9051e87d47c6d89148451c259a32b8e5";
    sha256 = "1y7x9n7v309prr7090rrlqwlrs2m6pikl3flyhyv2b9zs2kzm5vx";
  };

  nix-workspaces = callGitPackage {
    url = "https://github.com/Julow/nix-workspaces";
    rev = "f29f5646ec92d86360a847be64d192b92192b14e";
    sha256 = "18r0m30h4195cmnga1aqydy5cnkb7wx157k6lk1j5hsywa1acbqp";
  };

  mpv = super.mpv.override { scripts = with super.mpvScripts; [ mpris ]; };
}
