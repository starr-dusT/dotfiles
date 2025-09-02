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
| <img src="./resources/img/ships/bulwark.png" width="100"> | [bulwark](./provision/hosts/bulwark/) | Steam Deck | Black sheep of these configs. I use SteamOS with configurations documented in [host folder](./provision/hosts/bulwark). |
| <img src="./resources/img/ships/kestrel.png" width="100"> | [kestrel](./provision/hosts/kestrel/) | Primary desktop | Main machine that does it all gaming, programming, 3D modelling, etc. |
| <img src="./resources/img/ships/nesario.png" width="100"> | [nesario](./provision/hosts/nesario/) | Mom's Media PC | Media PC with basics to mostly watch baseball. |
| <img src="./resources/img/ships/shivan.png" width="100"> | [shivan](./provision/hosts/shivan/) | Personal Laptop | Personal (and very slow) laptop for basic mobile work. |
| <img src="./resources/img/ships/stormwalker.png" width="100"> | [stormwalker](./provision/hosts/stormwalker/) | Living Room Media PC | Media PC with basics to mostly watch baseball. |
| <img src="./resources/img/ships/torus.png" width="100"> | [torus](./provision/hosts/torus/) | Home server | Primary home server for storage, hosting machine backups, and running services like Jellyfin. |
