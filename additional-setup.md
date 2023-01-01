# Additional Setup

The following documents Void setup that wasn't automated with ansible

## Wireguard Client (outdated)

Wireguard is nice for a home vpn.

1. Create client on server and copy resulting `.conf` file to `/etc/wireguard`
2. Add connection with nmcli

```bash
sudo nmcli connection import type wireguard file /etc/wireguard/your-wg-file.conf
```

The vpn can be enable/disabled through gnome.

## btrbk  

[btrbk](https://github.com/digint/btrbk) is used to create (currently only local) snapshots of the root and user volumes.

```bash
sudo btrbk -c ~/.config/btrbk/home_btrbk.conf -v run # creates user backups and snapshots 
sudo btrbk -c ~/.config/btrbk/root_btrbk.conf -v run # creates root snapshots 
```

SSH keypair is used for password-less root ssh for remote back-up. See btrbk README.

anacron is used for daily backups. Copy `home_backup.sh` to `/etc/cron.daily`.

## Mount network drives

I find fstab messing about more troubule than it is worth. Mount network drives when needed with the following commands:

Engi:`linux-mount-engi`

## Taskopen for taskwarrior

taskopen needs is easier to install manually at this point since it isn't packaged and uses nim. Might get this automated in the future.

```bash
curl https://nim-lang.org/choosenim/init.sh -sSf | sh # install nim for compile
git clone https://github.com/jschlatow/taskopen.git
cd taskopen
make PREFIX=/usr
sudo make PREFIX=/usr install
```
