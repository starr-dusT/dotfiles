# Additional Setup

The following documents Fedora setup that wasn't automated with ansible

## Zen-link Kernel for Fedora

I like a kernel simliar to the Arch Zen kernel for gaming. [Nobara](https://nobaraproject.org/) provides one in this [copr repo](https://copr.fedorainfracloud.org/coprs/sentry/kernel-fsync/).

```bash
sudo dnf copr enable sentry/kernel-fsync
sudo dnf update --refresh
```

## Properitary Nvidia Drivers

Nvidia drivers are installed with this nice [copr repo](https://copr.fedorainfracloud.org/coprs/t0xic0der/nvidia-auto-installer-for-fedora/).

```bash
sudo dnf copr enable t0xic0der/nvidia-auto-installer-for-fedora -y
sudo dnf install nvautoinstall -y
sudo nvautoinstall rpmadd
sudo nvautoinstall driver
sudo nvautoinstall ffmpeg
sudo nvautoinstall vulkan
sudo nvautoinstall vidacc
```

## Wireguard Client

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

SSH keypair is used for password-less root ssh for remote back-up. See Github.

anacron is used for daily backups. Copy home_backup.sh to /etc/cron.daily.

## Automount network drive with fstab

Fstab can be mounted when the network drive is accessed. This is done for the "engi" home server.

```bash
sudo mkdir -p /mnt/engi

# Add following line to fstab
//<server-ip>/engi     /mnt/engi 	cifs 	uid=1000,credentials=/home/tstarr/.smb,iocharset=utf8,noauto,x-systemd.automount 0 0
```
## Taskopen for taskwarrior

taskopen needs is easier to install manually at this point since the fedora package is very old.

```bash
curl https://nim-lang.org/choosenim/init.sh -sSf | sh # install nim for compile
git clone https://github.com/jschlatow/taskopen.git
cd taskopen
make PREFIX=/usr
sudo make PREFIX=/usr install
```

## Taskwarrior-tui

Download a release [here](https://github.com/kdheepak/taskwarrior-tui) and move to /usr/bin.

## display manager (or lack thereof)

I disable gdm and login from tty

```bash
sudo systemctl disable gdm
```
