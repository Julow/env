{
  description = "My NixOS configurations.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    nur_rycee.url = "gitlab:rycee/nur-expressions/master";
    nur_rycee.flake = false;
    vim_plugins.url = "path:./vim";
    vim_plugins.inputs.nixpkgs.follows = "nixpkgs";
    nix-gc-env.url = "github:Julow/nix-gc-env";
    private_conf.url = "git:/--override-input";
    private_conf.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs:
    let
      patch_nixpkgs = input_nixpkgs: patches:
        let pkgs = input_nixpkgs.legacyPackages.x86_64-linux;
        in pkgs.applyPatches {
          name = "nixpkgs";
          src = pkgs.path;
          inherit patches;
        };

      # nixpkgs = patch_nixpkgs inputs.nixpkgs [
        # patches/desktop-managers-wallpaper-disable.patch
      # ];
      nixpkgs = inputs.nixpkgs;

      mk_nixos = path:
        import "${nixpkgs}/nixos/lib/eval-config.nix" rec {
          system = "x86_64-linux";
          specialArgs = inputs // { inherit system nixpkgs; };
          modules = [ path inputs.private_conf.nixosModules.default ];
        };

    in {
      nixosConfigurations.jules-pc = mk_nixos host/home;
      nixosConfigurations.jules-work = mk_nixos host/work;

      inherit (inputs.vim_plugins) dot_vim;
    };
}
