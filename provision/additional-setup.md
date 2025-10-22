# Additional Setup

The following documents some NixOS setup that wasn't automated.

## NIPR email, Teams, etc. in Firefox

- Run `pcscan` and make sure card reader shows up.
- Run `pkcs11-register` to register cac.

## Wireguard Client

Wireguard is nice for a home vpn. NixOS sets up all of the configurations for 
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

I find fstab messing about more trouble than it is worth. Credentials are managed
with agenix. Mount network drives when needed with the following command:

```bash
mount-<network drive name>.sh
```

## Git-annex

I use [git-annex](https://git-annex.branchable.com/walkthrough/) to sync some 
of the large files (mostly roms) that I don't want to have fill copies on each 
machine. Current git-annex stores I have are:

- `roms` from `torus`.

## Chrome

The vast majority of Chrome is setup with [chrome.nix](./optional/modules/programs/chrome.nix)
including installing plugins and various settings. However, setup for [vimium](https://chromewebstore.google.com/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb)
and [New Tab Redirect](https://chromewebstore.google.com/detail/new-tab-redirect/icpgjfneehieebagbmdbhnlpiopdcmna)
could not be fully automated.

- Upon launch New Tab Redirect should prompt to set the new tab page. 
Set to `https://glance.tstarr.us`.

- Pin and re-arrange plugins as desired.

## Obsidian Vault on New Machine

Clone `vulcan` vault from personal github.

```bash
git clone https://<user>:<token>@git.tstarr.us/tstarr/vulcan.git
```
