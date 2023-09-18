# Additional Setup

The following documents some NixOS setup that wasn't automated.

## Wireguard Client

Wireguard is nice for a home vpn and [pivpn](https://pivpn.io/) makes it easy.
Currently my scripts for controlling (and visualizing the state of the vpn) 
assume the connection name for the vpn is the same as the hostname of the system.
This may need to be modified in the future if I end up needing multiple vpn
connections.

1. Create client on server and copy resulting `.conf` file to local machine
2. Import to networkmanager with:
```bash
nmcli connection import type wireguard file <conf file from pivpn>
```
3. Use `nm-connection-editor` to disable automatic connection
4. Connect to vpn with `nmcli connection up <vpn connection name>

## Mount network drives

I find fstab messing about more trobule than it is worth. Credentials are 
stored in ~/.smb. Mount network drives when needed with the following command:

```bash
linux-mount-<network drive name>
```

## Syncthing 

Syncthing is used to sync folders between various computers and android. The 
ansible script should setup and run the service, but shares must be setup
via the web gui. 

## Git-annex

I use [git-annex](https://git-annex.branchable.com/walkthrough/) to sync some 
of the large files (mostly roms) that I don't want to have fill copies on each 
machine. 
- `roms` - `~/mnt/engi/media/roms` from Torus.
