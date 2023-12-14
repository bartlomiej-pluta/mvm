{
  description = "MVM development";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
  };

  outputs = { self, nixpkgs }: 
  let
    pkgs = import nixpkgs { inherit system; };
    haskellPkgs = pkgs.haskellPackages;
    system = "x86_64-linux";
  in {
    packages.${system}.default = pkgs.haskellPackages.developPackage {
      root = ./.;
    };
  };
}
