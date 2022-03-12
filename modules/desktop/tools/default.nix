{ lib, pkgs, stdenv }:

# Scripts used from the WM. 

stdenv.mkDerivation {
  name = "desktop-tools";
  src = ./.;
  propagatedBuildInputs = with pkgs; [
    pamixer
    playerctl
    brightnessctl
    dunst # dunstify
    imagemagick
    xclip
  ];
  installPhase = ''
    mkdir -p "$out/bin"
    for sf in *.sh; do
      df=$out/bin/$sf
      {
        echo "#!${pkgs.runtimeShell}"
        cat "$sf"
      } > "$df"
      chmod +x "$df"
      ${stdenv.shell} -n "$df"
    done
  '';
}
