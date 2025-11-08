# NixOS
> Document deez nuts.

Set of configs files to provision NixOS.

## Install

1. Download current minimal ISO from [NixOS website](https://nixos.org/download/) and boot from usb or boot into the NixOS installer from [tetragon's](./hosts/tetragon) PXE boot server.

2. Partition drives and perform initial installation with minimal changes to `/mnt/etc/nixos/configuration.nix`. See the [Nixos Installation Guide](https://nixos.wiki/wiki/NixOS_Installation_Guide) for guidance, as well as [BTRFS Wiki](https://nixos.wiki/wiki/Btrfs) for suggestions on a BTRFS partition layout and install commands. Minimal changes to the initial `configuration.nix` include:
   
   - Adding desired user and setting `passwd` after intial reboot.
   - Enabling SSH to produce keys needed for step 5.

4. Boot into installed system.

5. Add new machine's `/etc/ssh/ssh_host_ed25519.pub` to `secrets.nix` and rekey the secrets on an existing machine with `just rekey-secrets`.

6. Clone dotfiles:

```bash
nix-shell -p vim git neovim just
git clone https://github.com/starr-dusT/dotfiles ~/.local/share/chezmoi 
```

7. Move the installer created `configuration-hardware.nix` to dotfiles (e.g. `provision/hosts/<host>/hardware.nix`) if desired. `flake.nix` will load `/etc/nixos/hardware-configuration.nix` if that file does not exist.

6. Rebuild the system and initialize chezmoi dotfiles to save America:

```bash
just install <hostname>
chezmoi init && chezmoi apply
```
*Note:* if the `chezmoi.apply` option is enabled in `configuration.nix` the dotfiles should deploy automatically. The chezmoi commands then are not necessary.

7. Change remote for dotfiles to ssh with: `git remote set-url origin git@github.com:starr-dusT/dotfiles.git`.
8. Reboot and Profit!
9. Perform additional setup found in [ADDITIONAL SETUP](ADDITIONAL-SETUP.md) as needed.

## Update

`nixos-rebuild` command is built into the `justfile` with this folder, assuming the machine's flake output is named the same at the `hostname` of the machine.
