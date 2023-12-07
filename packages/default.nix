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

  # Allow WEP security, which is unfortunately still needed
  # https://github.com/NixOS/nixpkgs/issues/177501
  wpa_supplicant = wpa_supplicant.overrideAttrs (oldAttrs: rec {
    extraConfig = oldAttrs.extraConfig + ''
      CONFIG_WEP=y
    '';
  });
}
