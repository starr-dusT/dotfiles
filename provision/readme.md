# Provision Nixos
> \*NixOS was a mistake.\*

Set of configs files to setup NixOS.

## Usage

1. Install NixOS with this [guide](https://nixos.wiki/wiki/NixOS_Installation_Guide).
   Use the following paritions and btrfs subvolumes:

   | Name    | Type  | Mount Point | Size         |
   |---------|-------|-------------|--------------|
   | EFI     | vfat  | /boot       | 512MB        |
   | root    | btrfs | /           | rest of disk |
   | home    | btrfs | /home       | subvol       |
   | nix     | btrfs | /nix        | subvol       |
   | persist | btrfs | /persist    | subvol       |
   | log     | btrfs | /var/log    | subvol       |

2. Run the following commands:

   ```bash
   nix-shell -p vim git
   git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
   ```
   
   Move the installer created hardware.nix to dotfiles. E.g. `provision/hosts/<host>/hardware.nix`.
   
   ```bash
   cd ~/.local/share/chezmoi/provision
   sudo nixos-rebuild switch --flake .#<host>
   chezmoi init && chezmoi apply
   ```

3. Profit!

Perform additional setup found in [additional-setup](additional-setup.md)

## Update

`nixos-rebuild` command is aliased to `nu` assuming the flake is named the same at the
hostname of the machine.
