{ lib, pkgs, stdenv }:

# Scripts used from the WM. 

let
  progress-bar =
    pkgs.writeShellScriptBin "progress-bar.sh" ''
      bar() { x=`printf "%*s" "$1" ""`; echo "''${x// /━}"; }
      VALUE="$1"
      PREC=20
      DIV=$((VALUE * PREC / 100))
      if [[ $DIV -le $PREC ]]; then
        REM=`bar $((PREC - DIV))`
      else
        REM="<span color=\"red\">`bar $((DIV - PREC))`</span>"
        DIV=$PREC
      fi
      echo "<span color=\"''${BAR_COLOR:-cyan}\">`bar $DIV`</span>$REM"
    '';

  deps = with pkgs; [
    pulseaudio # pactl
    pamixer
    playerctl
    brightnessctl
    dunst # dunstify
    imagemagick
    xclip
    progress-bar
    acpi
    coreutils
    networkmanager
    xdotool
    gnugrep
  ];
in

stdenv.mkDerivation {
  name = "desktop-tools";
  src = ./.;
  installPhase = ''
    mkdir -p "$out/bin"
    for sf in *.sh; do
      df=$out/bin/$sf
      {
        echo "#!${pkgs.runtimeShell}"
        echo "export PATH=\"${lib.makeBinPath deps}:$out/bin\""
        cat "$sf"
      } > "$df"
      chmod +x "$df"
      ${stdenv.shell} -n "$df"
    done
  '';
}
