# Additional Setup

The following documents Void setup that wasn't automated with ansible.

## Radicale and vdirsyncer for contacts/calendar

[Radicale](https://radicale.org/v3.html) is hosted on my home server to provide
DAV synced calendars and contacts. [Vdirsyncer](https://github.com/pimutils/vdirsyncer)
allows for the files to be synced to the linux filesystem for desktop usage
with [khard](https://github.com/lucc/khard) and [khal](https://github.com/pimutils/khal).
The config files are auto-populated by chezmoi with credentials from bitwarden
run the following commands to setup vdirsyncer files:

```bash
vdirsyncer -c ~/.config/vdirsyncer/config_calendar discover 
vdirsyncer -c ~/.config/vdirsyncer/config_contacts discover 
```

Files can be later synced with the following commands:

```bash
vdirsyncer -c ~/.config/vdirsyncer/config_calendar sync
vdirsyncer -c ~/.config/vdirsyncer/config_contacts sync
```

## Wireguard Client

Wireguard is nice for a home vpn and [pivpn](https://pivpn.io/) makes it easy.

1. Create client on server and copy resulting `.conf` file to `/etc/wireguard`
2. Start/stop vpn with `wg-quick`

## BTRFS back-ups with btrbk  

[btrbk](https://github.com/digint/btrbk) is used to create snapshots of the 
root and user volumes. User volumes are backed-up to my home server (Torus),
but root is only stored locally.

```bash
sudo btrbk -c ~/.config/btrbk/home_btrbk.conf -v run # snapshot /home/<user> 
sudo btrbk -c ~/.config/btrbk/root_btrbk.conf -v run # snapshot / 
```

SSH keypair is used for password-less root ssh for remote back-up. See 
[ssh setup](https://github.com/digint/btrbk#setting-up-ssh) from the btrbk 
readme.

anacron is used for daily backups. Copy `home_backup.sh` from the config folder
to `/etc/cron.daily`.

## Mount network drives

I find fstab messing about more troubule than it is worth. Mount network drives 
when needed with the following command:

```bash
linux-mount-<network drive name>
```

## Taskopen for taskwarrior

[taskopen](https://github.com/jschlatow/taskopeni) is easier to install 
manually at this point since it isn't packaged and uses nim. Might get this 
automated in the future.

```bash
curl https://nim-lang.org/choosenim/init.sh -sSf | sh # install nim
git clone https://github.com/jschlatow/taskopen.git
cd taskopen
make PREFIX=/usr
sudo make PREFIX=/usr install
```

## Syncthing 

Syncthing is used to sync folders between various computers and android. The 
ansible script should setup and run the service, but shares must be setup
via the web gui. Currently four shares exists:
- `.warrior` - `.task` and `.timewarrior` folders to sync taskwarrior tasks.
These two folders are symlinked to the home folder where taskwarrior/timewarrior 
expects them.
- `warrior` - contains text files associated with taskwarrior (mostly from
taskopen).
- `phone photos` - personal photos synched from android.
- `phone screenshots` - personal screenshots synced from android.

## Lxappearance

My GTK theme is pulled down by chezmoi, but isn't active by default. This can
be fixed with the lxappearance gui.

## Single GPU Passthrough with Windows

Install libvirt hooks:

```bash
sudo mkdir -p /etc/libvirt/hooks
sudo wget 'https://raw.githubusercontent.com/PassthroughPOST/VFIO-Tools/master/libvirt_hooks/qemu' \
     -O /etc/libvirt/hooks/qemu
sudo chmod +x /etc/libvirt/hooks/qemu
```

reboot....


```bash

```
