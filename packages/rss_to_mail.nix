{ stdenv, pkgs, fetchgit, ... }:

let
  src = fetchgit {
    url = "https://github.com/Julow/rss_to_mail.git";
    rev = "4e0f866d192d0ec169c72c0f83b089fb93b9a4a5";
    sha256 = "0l3zl0ik7kwczc6zswfyfxxzif7qk2w0siwb0w1qgiainxs7zhl3";
  };
in

(pkgs.callPackage src {}).rss_to_mail
