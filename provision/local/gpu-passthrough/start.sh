#!/run/current-system/sw/bin/bash

set -x

# Stop your display manager. If you're on kde it'll be sddm.service. Gnome users should use 'killall gdm-x-session' instead
systemctl stop display-manager.service

# Unbind VTconsoles
echo 0 > /sys/class/vtconsole/vtcon0/bind
echo 0 > /sys/class/vtconsole/vtcon1/bind

# Unbind EFI-Framebuffer
echo efi-framebuffer.0 > /sys/bus/platform/drivers/efi-framebuffer/unbind || true
echo simple-framebuffer.0 > /sys/bus/platform/drivers/simple-framebuffer/unbind || true
echo vesa-framebuffer.0 > /sys/bus/platform/drivers/vesa-framebuffer/unbind || true

# ZzzzzzZzzzz
sleep 1

# Unload all Amd drivers
modprobe -r drm_kms_helper
modprobe -r amdgpu
modprobe -r radeon
modprobe -r drm

# Load VFIO kernel module
modprobe vfio
modprobe vfio_pci
modprobe vfio_iommu_type1
