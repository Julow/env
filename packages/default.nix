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
    rev = "212ea465a3ced46826b3a9a2c200f848f4daad11";
    sha256 = "02ay33x93iivj126yyf7x9almh3gw5a9wxjn2rijai4dl40m8xx9";
  };

  rss_to_mail = (callGitPackage {
    url = "https://github.com/Julow/rss_to_mail";
    rev = "318f464623b301c472ea528f18856ea6e041758a";
    sha256 = "0b85dj7pr4n0shh14apkhgj67xpxp0ffdrqyw25fp52hbzj83bm9";
  }).rss_to_mail;

  nixos-deploy = callGitPackage {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "995ccabc200d27c0d811f950f3deb3083aee798f";
    sha256 = "0qslrnvia6y8zb5zrqjw89vj5zfzkh3wi18r2qpnnxa80qjqqmxd";
  };

  mpv = super.mpv.override {
    scripts = with super.mpvScripts; [
      mpris
      youtube-quality
    ];
  };
}
