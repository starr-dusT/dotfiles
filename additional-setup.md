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