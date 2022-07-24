{ pkgs, config, lib, vim_plugins, ... }:
let

  dot_vim = pkgs.symlinkJoin {
    name = ".vim";
    paths = [ ../vim ];
    postBuild = ''
      pdst=$out/pack/plugins/start
      mkdir -p "$pdst"
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (pname: path: ''
        ln -Ts "${path}" "$pdst/${pname}"
      '') vim_plugins.inputs)}
    '';
  };

in { home.file = { ".vim".source = dot_vim; }; }
