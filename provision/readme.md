# Provision NixOS
> NixOS was a mistake.

Set of configs files to setup NixOS.

## Install

1. Download and install [NixOS](https://nixos.org/download/) with current GNOME ISO.

2. Boot into installed system and run the following commands:

```bash
nix-shell -p vim git neovim
git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```
   
3. Copy existing configuration files from another host and modify as needed. For most configs, move the installer created `configuration-hardware.nix` to dotfiles (e.g. `provision/hosts/<host>/hardware.nix`).
   
4. If required move agenix keypairs to `~/.ssh/keys/{age,age.pub}`. A new keypair may be required and agenix files will need to be rekeyed on another system.

5. Rebuild the system and initialize chezmoi dotfiles to save America:

```bash
sudo nixos-rebuild switch --impure --flake .#<host>
chezmoi init && chezmoi apply
```
*Note:* if the `chezmoi.apply` option is enabled in `configuration.nix` the dotfiles should deploy automatically. The chezmoi commands then are not necessary.

6. Profit!

Perform additional setup found in [additional-setup](additional-setup.md)

## Update

`nixos-rebuild` command is built into the `justfile` with this folder, assuming the machine's flake output is named the same at the hostname of the machine.
