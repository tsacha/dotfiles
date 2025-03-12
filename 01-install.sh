#!/bin/bash

systemd-cryptenroll /dev/nvme1n1p2 --wipe-slot=tpm2 --tpm2-device=auto --tpm2-pcrs=7

# HOOKS=(base systemd autodetect microcode modconf kms keyboard sd-vconsole block sd-encrypt filesystems fsck

echo "root UUID=$(blkid /dev/nvme1n1p2  -s UUID -o value) none tp2-device=auto" > /etc/crypttab.initramfs

systemctl enable --now NetworkManager
systemctl enable --now pcscd.service
systemctl enable --now ly.service

groupadd uinput
usermod -aG input sacha
usermod -aG uinput sacha

echo 'KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"' > /etc/udev/rules.d/99-input.rules


git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
