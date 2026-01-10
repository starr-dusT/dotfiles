{
  description = "Flake to manage my nixos machines";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    agenix.url = "github:ryantm/agenix";
    nix-cachyos-kernel.url = "github:xddxdd/nix-cachyos-kernel/release";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    nix-flatpak.url = "github:gmodena/nix-flatpak/?ref=v0.6.0";
  };

  outputs =
    inputs@{
      nixpkgs,
      home-manager,
      agenix,
      nix-flatpak,
      nix-cachyos-kernel,
      ...
    }:
    let
      system = "x86_64-linux";
      hosts = builtins.fromJSON (builtins.readFile ./hosts.json);
      lib = nixpkgs.lib;
    in
    {
      nixosConfigurations = lib.mapAttrs (
        hostname: hostConfig:
        lib.nixosSystem {
          specialArgs = {
            inherit
              lib
              system
              inputs
              agenix
              home-manager
              nix-flatpak
              ;
            user = hostConfig.user;
            hostname = "${hostname}";
          };
          modules = [
            ./hosts/${hostConfig.role}/configuration.nix
            ./modules
            agenix.nixosModules.default
            nix-flatpak.nixosModules.nix-flatpak
            home-manager.nixosModules.home-manager
            {
              nixpkgs.overlays = [ nix-cachyos-kernel.overlays.pinned ];
              nix.settings.substituters = [ "https://attic.xuyh0120.win/lantian" ];
              nix.settings.trusted-public-keys = [ "lantian:EeAUQ+W+6r7EtwnmYjeVwx5kOGEBpjlBfPlzGlTNvHc=" ];

              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.extraSpecialArgs = {
                user = hostConfig.user;
              };
            }
          ]
          ++ [
            (
              if builtins.hasAttr "hardware" hostConfig then
                ./hosts/${hostConfig.role}/${hostConfig.hardware}
              else
                ./hosts/${hostConfig.role}/hardware.nix
            )
          ];
        }
      ) hosts;
    };
}
