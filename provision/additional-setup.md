# Additional Setup

The following documents some NixOS setup that wasn't automated.

## Hyprland

Detecting monitor configs is annoying and I don't feel like it is worth the time
to figure out. After doing the first `chezmoi apply` you will need copy `monitors.conf.example`
in the hyprland config folder to `monitors.conf` and edit for correct monitor 
settings.

## Encryption Keys

### Github SSH

Keys for SSH aren't automatically placed with chezmoi `secret` since it complicated
things to much. The key for github SSH must be transferred manually from Bitwarden
or `/run/secrets/keys/github_personal` to `~/.ssh/keys/github_personal`.

### Sops-nix 

Encrypted keys for various aspects of the config are stored in `./secrets/secrets.yaml` 
to the required key for decryption must be placed at `~/.config/sops/age/keys.txt`.

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

## Git-annex

I use [git-annex](https://git-annex.branchable.com/walkthrough/) to sync some 
of the large files (mostly roms) that I don't want to have fill copies on each 
machine. Current git-annex stores I have are:

- `roms` - `~/mnt/engi/media/roms` from `torus`.

## Cadquery and Simplify3d

Cadquery and Simplify3d don't play nice with non-FHS filesystems (and Simplify3d
is proprietary). I run these programs within distrobox. Some notes about using
distrobox for these programs.

If arch-box isn't created, create it and apply chezmoi dot files to secondary
home directory:

```bash
distrobox create --image archlinux --name arch-box --home ~/box/arch
chezmoi apply -D ~/home/arch
```

Then enter the box and run script to install cadquery and dependencies for 
Simplify3d:

```bash
distrobox enter arch-box
cd && ./bin/arch-install-cad
```

As the script suggest then download and install Simplify3d from the script on the
[website](https://www.simplify3d.com/).

The applications can be run within the box with the following commands:

```bash
# Simplify3d
/opt/Simplify3D-5.1.2/LaunchScript.sh
# CQ-editor
cd ~/cq-editor && ./run.sh
```

## Chromium

The vast majority of Chromium is setup with [browser.nix](./modules/desktop/browser.nix)
including installing plugins and various settings. However, setup for [vimium](https://chromewebstore.google.com/detail/vimium/dbepggeogbaibhgnhhndojpepiihcmeb)
and [New Tab Redirect](https://chromewebstore.google.com/detail/new-tab-redirect/icpgjfneehieebagbmdbhnlpiopdcmna)
could not be fully automated.

- Upon launch New Tab Redirect should prompt to set the new tab page. 
Set to `http://localhost:8080`.

- Vimium config is saved in chezmoi. Open the vimium [plugin page](chrome-extension://dbepggeogbaibhgnhhndojpepiihcmeb/pages/options.html)
and restore config from `~/.config/vimium-options.json`.

## Steam

Some games/emulators (in my case Ares) don't fullscreen nicely in window managers.
In that case add something alongs the lines of the following 
launch command to run them within gamescope.

```bash
gamescope -w 2560 -h 1440 -f %command%
```

## Obsidian Vault on New machine

Clone `vulcan` vault from personal github.

```bash
git clone https://<user>:<token>@git.tstarr.us/tstarr/vulcan.git
```

Copy another `.obsidian` folder for the new host (i.e. `.obsidian-<host>`).

Open vault folder in obsidian and change the `.obsidian` folder in settings.
