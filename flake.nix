{
  description = "My NixOS configurations.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
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

      nixpkgs = patch_nixpkgs inputs.nixpkgs [
        nixpkgs_patches/ocamlformat_0_20_0_and_0_20_1.patch
        nixpkgs_patches/ocamlformat_0_21_0.patch
      ];

      mk_nixos = path:
        import "${nixpkgs}/nixos/lib/eval-config.nix" {
          system = "x86_64-linux";
          specialArgs = inputs // { inherit nixpkgs; };
          modules = [ path ];
        };

    in {
      nixosConfigurations.jules-pc = mk_nixos host/home;
      nixosConfigurations.jules-work = mk_nixos host/work;

    };
}
