#!/bin/bash


# Ça change tout le temps, juste vérifier.
# archinstall fait du bon taf généralement
# HOOKS=(base systemd autodetect microcode modconf kms keyboard sd-vconsole block sd-encrypt filesystems fsck

systemd-cryptenroll /dev/nvme1n1p2 --wipe-slot=tpm2 --tpm2-device=auto --tpm2-pcrs=7
echo "root UUID=$(blkid /dev/nvme1n1p2  -s UUID -o value) none tp2-device=auto" > /etc/crypttab.initramfs

systemctl enable --now pcscd.service # Yubikey

cp ./home/dot_layouts/ergol.xkb_symbols /usr/share/X11/xkb/symbols/ergol_anglemod

git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
