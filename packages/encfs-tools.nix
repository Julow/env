{ pkgs }:

let
  gpg = "${pkgs.gnupg}/bin/gpg";

  encfs-gpg-extpass = pkgs.writeShellScript "encfs-gpg-extpass" ''
    set -e

    decrypt_and_print_key_file ()
    {
      ${gpg} --decrypt --quiet --batch -o - "$key_file"
    }

    create_and_print_key_file ()
    {
      local passwd
      passwd=`head -c 60 < /dev/urandom | tr -d '\0'`
      ${gpg} --encrypt --default-recipient-self --quiet --batch -o "$key_file" - <<<"$passwd"
      echo "$passwd"
    }

    if ! [[ -d $encfs_root ]]; then echo "Error: expecting encfs_root variable" >&2; exit 1; fi
    declare key_file="$encfs_root/.key.gpg"

    if [[ -e $key_file ]]; then
      decrypt_and_print_key_file
    else
      create_and_print_key_file
    fi
  '';

in pkgs.writeShellScriptBin "encfs-gpg.sh" ''
  ${pkgs.encfs}/bin/encfs --extpass="${encfs-gpg-extpass}" "$@"
''
