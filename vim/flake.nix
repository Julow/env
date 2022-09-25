{
  description = "Vim plugins";

  inputs = {
    nixpkgs.flake = false;
    nixpkgs.url = "/etc/nixpkgs"; # Overriden by parent
    "argtextobj.vim".flake = false;
    "argtextobj.vim".url =
      "github:vim-scripts/argtextobj.vim/f3fbe427f7b4ec436416a5816d714dc917dc530b";
    capnp.flake = false;
    capnp.url =
      "github:cstrahan/vim-capnp/954202e2c6c1cb9185082de8ddb7f2823a9d1206";
    commentary.flake = false;
    commentary.url =
      "github:tpope/vim-commentary/627308e30639be3e2d5402808ce18690557e8292";
    conflict3.flake = false;
    conflict3.url =
      "github:mkotha/conflict3/180e387fa464f27346db6a9c577ae6c1a26cd39a";
    dirvish.flake = false;
    dirvish.url =
      "github:justinmk/vim-dirvish/b2b5709b7979bb99b0548d5879c49672891b9b5b";
    dwm.flake = false;
    dwm.url = "github:spolu/dwm.vim/6149e58fdd81f69e4e6a3f239842f3dc23e4872b";
    easy-align.flake = false;
    easy-align.url =
      "github:junegunn/vim-easy-align/12dd6316974f71ce333e360c0260b4e1f81169c3";
    fugitive.flake = false;
    fugitive.url =
      "github:tpope/vim-fugitive/1352646890aafdb6d7e5e9b8092f89b5820de6be";
    futhark.flake = false;
    futhark.url =
      "github:BeneCollyridam/futhark-vim/fd7d053c74f150712eaa73999f44a3f95c8f08ff";
    gv.flake = false;
    gv.url = "github:junegunn/gv.vim/6f6a3afe73a2cb52d8517d1a95ecfc9b09fb3e92";
    indent-object.flake = false;
    indent-object.url =
      "github:michaeljsmith/vim-indent-object/5c5b24c959478929b54a9e831a8e2e651a465965";
    json.flake = false;
    json.url = "github:elzr/vim-json/3727f089410e23ae113be6222e8a08dd2613ecf2";
    markdown.flake = false;
    markdown-folding.flake = false;
    markdown-folding.url =
      "github:masukomi/vim-markdown-folding/3f35acfb753cc9ea22182400b075c5b6e896ad71";
    markdown.url =
      "github:plasticboy/vim-markdown/8e5d86f7b85234d3d1b4207dceebc43a768ed5d4";
    nix.flake = false;
    nix.url = "github:LnL7/vim-nix/63b47b39c8d481ebca3092822ca8972e08df769b";
    ocaml.flake = false;
    ocaml.url =
      "github:ocaml/vim-ocaml/6dcd9efb3278bfc51f9e460f5e2ed45134002c09";
    sneak.flake = false;
    sneak.url =
      "github:justinmk/vim-sneak/95374ad3e4b5ef902854e8f4bcfa9a7a31a91d71";
    surround.flake = false;
    surround.url =
      "github:tpope/vim-surround/f51a26d3710629d031806305b6c8727189cd1935";
    xdg_open.flake = false;
    xdg_open.url =
      "github:arp242/xdg_open.vim/6474b4de866d9986788a327808c0fb4537a18101";
  };
  outputs = inputs:
    let
      pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
      inherit (pkgs) lib;
      plugins = builtins.removeAttrs inputs [ "nixpkgs" ];
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
