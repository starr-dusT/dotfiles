# Additional Setup

The following documents some NixOS setup that wasn't automated.

## Wireguard Client

Wireguard is nice for a home vpn. NixOS setus up all of the configurations for 
peers automatically on `torus`. Public keys, private keys, and config files are
generated and stored in `~/.wireguard`. Some machines like `kestrel` setup the 
vpn automatically within `configuration.nix`. However, others require a small
amount of manual setup.

Cell phones like `adjudicator` are added with a generated QR code:

```bash
qrencode -t ansiutf8 < <conf file>
```

Computers using NetworkManager like `bulwark` need to have conf file imported:

```bash
nmcli connection import type wireguard file <conf file>
```

## Mount network drives

I find fstab messing about more trobule than it is worth. Credentials need to be 
manually created in `~/.smb`. To mount network drives when needed with the 
following command:

```bash
linux-mount-<network drive name>
```

## Syncthing 

Syncthing is used to sync folders between various computers. The service is 
automatically installed and started with NixOS, but shares currently must be 
setup with the web gui. 

## Git-annex

I use [git-annex](https://git-annex.branchable.com/walkthrough/) to sync some 
of the large files (mostly roms) that I don't want to have fill copies on each 
machine. Current git-annex stores I have are:

- `roms` - `~/mnt/engi/media/roms` from `torus`.
