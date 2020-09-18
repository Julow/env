{ stdenv, pkgs, fetchgit, ... }:

let
  src = fetchgit {
    url = "https://github.com/Julow/rss_to_mail.git";
    rev = "5abe83bd73c33e9d377140042ddd44b75c852aca";
    sha256 = "02k2rdccsdw61c54hqw07162pkki829a1wys662563g087b3ww2r";
  };
in

(pkgs.callPackage src {}).rss_to_mail
