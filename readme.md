# .dotfiles
> My dotfiles... What do you want? 

My personal dotfiles repo. Documentation is pretty spotty. Maybe I'll fix that 
someday, but I probably won't. 

## Usage

Dotfiles are managed with chezmoi and located in the `home` folder. The NixOS 
flake contained in `provision` contains nix code for installing and configuring 
my various NixOS based machines. See the provision [readme](./provision/readme.md)
for information on initial installation.

## Managed Machines

| | Name | Role | Description |
|---|---|---|---|
| <img src="./resources/img/ships/kestrel.png" width="100"> | [Kestrel](./provision/hosts/kestrel/) | Primary desktop | Main machine that does it all gaming, programming, 3D modelling, etc. |
| <img src="./resources/img/ships/torus.png" width="100"> | [Torus](./provision/hosts/torus/) | Home server | Primary home server for storage, hosting machine backups, and running services like Jellyfin. |
| <img src="./resources/img/ships/bulwark.png" width="100"> | [Bulwark](./provision/hosts/bulwark/) | Steam Deck | NixOS based Steam Deck that can has my linux niceties and acts like a Steam Deck using the great [Jovian Nixos](https://github.com/Jovian-Experiments/Jovian-NixOS). |
| <img src="./resources/img/ships/shivan.png" width="100"> | [Shivan](./provision/hosts/shivan/) | Personal Laptop | Personal (and very slow) laptop for basic mobile work. |
| <img src="./resources/img/ships/osprey.png" width="100"> | [Osprey](./provision/hosts/osprey/) | Secondary desktop | Secondary desktop used for code development. |
