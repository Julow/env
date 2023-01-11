{
  description = "Vim plugins";

  inputs = {
    nixpkgs.flake = false;
    nixpkgs.url = "/etc/nixpkgs"; # Overriden by parent
    "argtextobj.vim".flake = false;
    "argtextobj.vim".url = "github:vim-scripts/argtextobj.vim";
    commentary.flake = false;
    commentary.url = "github:tpope/vim-commentary";
    conflict3.flake = false;
    conflict3.url = "github:mkotha/conflict3";
    dirvish.flake = false;
    dirvish.url = "github:justinmk/vim-dirvish";
    dwm.flake = false;
    dwm.url = "github:spolu/dwm.vim";
    easy-align.flake = false;
    easy-align.url = "github:junegunn/vim-easy-align";
    fugitive.flake = false;
    fugitive.url = "github:tpope/vim-fugitive";
    futhark.flake = false;
    futhark.url = "github:BeneCollyridam/futhark-vim";
    gv.flake = false;
    gv.url = "github:junegunn/gv.vim";
    indent-object.flake = false;
    indent-object.url = "github:michaeljsmith/vim-indent-object";
    json.flake = false;
    json.url = "github:elzr/vim-json";
    markdown.flake = false;
    markdown-folding.flake = false;
    markdown-folding.url =
      "github:masukomi/vim-markdown-folding/3f35acfb753cc9ea22182400b075c5b6e896ad71";
    markdown.url = "github:preservim/vim-markdown";
    nix.flake = false;
    nix.url = "github:Julow/vim-nix/cmd_nix_shell";
    ocaml.flake = false;
    ocaml.url = "github:ocaml/vim-ocaml";
    sneak.flake = false;
    sneak.url = "github:justinmk/vim-sneak";
    surround.flake = false;
    surround.url = "github:tpope/vim-surround";
    xdg_open.flake = false;
    xdg_open.url = "github:arp242/xdg_open.vim";
    ocp-indent = {
      flake = false;
      url = "github:OCamlPro/ocp-indent";
    };
    merlin = {
      flake = false;
      url = "github:ocaml/merlin";
    };
    diffchar.flake = false;
    diffchar.url = "github:rickhowe/diffchar.vim";
  };
  outputs = inputs:
    let
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      inherit (pkgs) lib;
      plugins = builtins.removeAttrs inputs [ "nixpkgs" ] // {
        ocp-indent = pkgs.linkFarm "ocp-indent" [{
          name = "indent/ocaml.vim";
          path = "${inputs.ocp-indent}/tools/ocp-indent.vim";
        }];
        merlin = "${inputs.merlin}/vim/merlin";
      };
    in {
      dot_vim = pkgs.symlinkJoin {
        name = ".vim";
        paths = [ ./. ];
        postBuild = ''
          pdst=$out/pack/plugins/start
          mkdir -p "$pdst"
          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (pname: path: ''
            ln -Ts "${path}" "$pdst/${pname}"
          '') plugins)}
        '';
      };
    };
}
