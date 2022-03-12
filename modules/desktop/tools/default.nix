{ lib, pkgs, stdenv }:

# Scripts used from the WM. 

stdenv.mkDerivation {
  name = "desktop-tools";
  src = ./.;
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
