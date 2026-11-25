self: super:
with super;

let callGitPackage = args: self.callPackage (pkgs.fetchgit args) { };

in {

  nixos-deploy = callGitPackage {
    url = "https://github.com/Julow/nixos-deploy";
    rev = "55454666374f43fd71e85e4a4984ef09e8ea8839";
    sha256 = "sha256-EtXPrqaX0QYg+sDdEoZnre8ypxvXEG9aC8vk9WzNk5s=";
  };

  mpv = mpv.override {
    scripts = with mpvScripts; [ mpris quality-menu ];
  };

  encfs-gpg = self.callPackage ./encfs-tools.nix { };

  # Build xmonad-contrib from a specific checkout instead of the nixpkgs one.
  haskellPackages = haskellPackages.override {
    overrides = _: hsuper: {
      xmonad-contrib = hsuper.xmonad-contrib.overrideAttrs (_: {
        src = pkgs.fetchgit {
          url = "https://github.com/Julow/xmonad-contrib/";
          rev = "dff6bc6830f1338e5a0830e46491bf7c85fcc38b";
          sha256 = "sha256-sBHzXJhGOzoN6Hizb9tN/rcA1EiD7la+5txvAwRINxQ=";
        };
      });
    };
  };

  # Allow WEP security, which is unfortunately still needed
  # https://github.com/NixOS/nixpkgs/issues/177501
  wpa_supplicant = wpa_supplicant.overrideAttrs (oldAttrs: rec {
    extraConfig = oldAttrs.extraConfig + ''
      CONFIG_WEP=y
    '';
  });
}
