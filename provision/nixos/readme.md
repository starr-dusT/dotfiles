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
   
   Move the installer created hardware.nix to dotfiles.
   
   Copy `.chezmoidata.yaml.example` to `.chezmoidata.yaml` and edit with desired 
   settings then run the following commands:
   
   ```bash
   nixos-update # Assuming hostname is same as flake name
   ```

Perform additional setup found in [additional-setup](additional-setup.md)

## Update

`nixos-update` command is aliased assuming the flake is named the same at the
hostname of the machine.
