## enable dbus

```bash
sudo ln -s /etc/sv/dbus /var/service
```
## enable polkitd

```bash
sudo ln -s /etc/sv/polkitd /var/service
```

## enable bluetooth

```bash
sudo ln -s /etc/sv/bluetoothd /var/service
```

## enable sshd 

```bash
sudo ln -s /etc/sv/sshd /var/service
```

## enable non-free and multilib

```bash
xbps-install -Syv void-repo-nonfree
xbps-install -Syv void-repo-multilib
xbps-install -Syv void-repo-multilib-nonfree
```

## void packages

```bash
$ git clone https://github.com/void-linux/void-packages.git
$ cd void-packages
$ ./xbps-src binary-bootstrap
echo XBPS_ALLOW_RESTRICTED=yes >> etc/conf

# Example install discord
./xbps-src pkg discord
sudo xbps-install --repository hostdir/binpkgs/nonfree discord
```

## user services

```bash
sudo mkdir -p /etc/sv/tstarr
sudo touch /etc/sv/tstarr/run
sudo chmod +x /etc/sv/tstarr/run
```

Add following:

```bash
#!/bin/sh

USER="tstarr"
GROUPS="$(id -Gn "$USER" | tr ' ' ':')"
SVDIR="/home/${USER}/.service"


if [ -d ${SVDIR} ]; then
 chpst -u"${USER}:${GROUPS}" runsvdir ${SVDIR} 
fi
```

```bash
sudo ln -s /etc/sv/tstarr /var/service
```

Example with syncthing:

```bash
mkdir -p $HOME/.service/syncthing
touch $HOME/.service/syncthing/run
chmod +x $HOME/.service/syncthing/run
```

Add the following:

```bash
#!/bin/sh

export HOME=/home/tstarr/
exec /usr/bin/syncthing > /dev/null 2>&1
```

## OH-my-zsh

```bash
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
```

## Lxappearance

GTK theme can be set with lxappearance

## poweroff without root

add to /etc/sudoers:

```bash
tstarr ALL=NOPASSWD:/bin/poweroff
```

