# NixOS
> Document deez nuts.

Set of configs files to provision NixOS.

## Install

1. Download current Gnome ISO from [NixOS website](https://nixos.org/download/).

2. Partition drives and perform initial installation with minimal changes to `/etc/nixos/configuration.nix`. See [this](https://nixos.wiki/wiki/Btrfs) for a suggestions on a BTRFS partition layout and install commands. 

4. Boot into installed system.

5. Add new machine's `/etc/ssh/ssh_host_ed25519.pub` to `secrets.nix` and rekey the secrets on an existing machine.

6. Clone dotfiles:

```bash
nix-shell -p vim git neovim
git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```
   
7. Move the installer created `configuration-hardware.nix` to dotfiles (e.g. `provision/hosts/<host>/hardware.nix`).

6. Rebuild the system and initialize chezmoi dotfiles to save America:

```bash
just install <hostname>
chezmoi init && chezmoi apply
```
*Note:* if the `chezmoi.apply` option is enabled in `configuration.nix` the dotfiles should deploy automatically. The chezmoi commands then are not necessary.

7. Profit!

Perform additional setup found in [ADDITIONAL SETUP](ADDITIONAL-SETUP.md).

## Update

`nixos-rebuild` command is built into the `justfile` with this folder, assuming the machine's flake output is named the same at the `hostname` of the machine.
