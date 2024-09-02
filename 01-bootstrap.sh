#!/bin/bash
read -r -p "Encrypted disk name? " cryptname

mountpoint /mnt/boot > /dev/null
if [ $? -eq 0 ]; then
    echo -n "Umount /mnt/boot… "
    umount /mnt/boot
    echo "Done."
fi

mountpoint /mnt > /dev/null
if [ $? -eq 0 ]; then
    echo -n "Umount /mnt… "
    umount /mnt
    echo "Done."
fi

if [ -b "/dev/mapper/"$cryptname ]; then
    echo -n "Close existing luks device… "
    cryptsetup luksClose /dev/mapper/$cryptname
    echo "Done."
fi

read -r -p "Disk to use? " disk
read -r -p "Auto partitioning? (y/N) : " auto_p
if [ "$auto_p" == "y" ]; then
    boot_size_mb=4096
    sgdisk -og $disk
    start_sector=$(sgdisk -F $disk)
    sector_size=$(cat /sys/block/$(basename $disk)/queue/physical_block_size)
    alignment=$(sgdisk -D $disk)
    end_sector=$(sgdisk -E $disk)
    let end_boot_sector="(($boot_size_mb * 1024 * 1024 / $sector_size) + $alignment - 1)"

    echo "Create partitions…"
    sgdisk -o $disk > /dev/null
    sgdisk -n 1:$start_sector:$end_boot_sector -c 1:"EFI System Partition" -t 1:ef00 $disk > /dev/null
    sgdisk -n 2:$(($end_boot_sector + 1)):$end_sector -c 2:"Linux partition" -t 2:8304 $disk > /dev/null
    echo "Done."
    boot_guid=$(sgdisk -i 1 $disk | grep "unique GUID" | awk '{ print $NF }' | tr '[:upper:]' '[:lower:]')
    data_guid=$(sgdisk -i 2 $disk | grep "unique GUID" | awk '{ print $NF }' | tr '[:upper:]' '[:lower:]')
else
    gdisk $disk
    echo -n "Partition number of boot partition? "
    read -r boot_nb
    echo -n "Partition number of data partition? "
    read -r data_nb
    boot_guid=$(sgdisk -i $boot_nb $disk | grep "unique GUID" | awk '{ print $NF }' | tr '[:upper:]' '[:lower:]')
    data_guid=$(sgdisk -i $data_nb $disk | grep "unique GUID" | awk '{ print $NF }' | tr '[:upper:]' '[:lower:]')
fi

data_disk=/dev/disk/by-partuuid/$data_guid
boot_disk=/dev/disk/by-partuuid/$boot_guid

encrypt_hook=""
encrypt_hook="sd-encrypt"
read -s -p "Luks password: " luks_password
echo
echo -n $luks_password | cryptsetup -q luksFormat $data_disk -
echo "Luks created."
echo -n $luks_password | cryptsetup luksOpen $data_disk $cryptname -
echo "Luks opened."
data_uuid=`blkid $data_disk -s UUID -o value`
data_disk=/dev/mapper/$cryptname

echo -n "Fais ton taf Sacha du futur et rajoute le TPM dans le LUKS."
echo -n "systemd-cryptenroll --tpm2-device=auto /dev/nvme1n1p2"

read -r -p "Format boot partition? y/N " format_boot
if [ "$format_boot" == "y" ]; then
    echo -n "Format boot partition… "
    mkfs.vfat -F32 $boot_disk > /dev/null 2> /dev/null
    echo "Done."
fi

echo -n "Format data partition… "
mkfs -t ext4 $data_disk > /dev/null 2> /dev/null
echo "Done."

mount $data_disk /mnt
if [ ! -e /mnt/boot ]; then
    echo "Creating /boot partition."
    mkdir /mnt/boot
fi
mount $boot_disk /mnt/boot
pacstrap /mnt base base-devel linux linux-firmware

arch-chroot /mnt bootctl install

cat <<EOF > /mnt/boot/loader/loader.conf
default arch
timeout 2
EOF

read -r -p "Nvidia? (y/N) : " nvidia
if [ "$nvidia" == "y" ]; then
    cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options rd.luks.name=$data_uuid=$cryptname root=$data_disk rootfstype=ext4 add_efi_memmap nvidia_drm.modeset=1
EOF
else
    cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options rd.luks.name=$data_uuid=$cryptname root=$data_disk rootfstype=ext4 add_efi_memmap
EOF
fi

cat <<EOF > /mnt/boot/loader/entries/arch-fallback.conf
title   Arch Linux Fallback
linux   /vmlinuz-linux
initrd  /initramfs-linux-fallback.img
options rd.luks.name=$data_uuid=$cryptname root=$data_disk rootfstype=ext4 add_efi_memmap
EOF

vendor=$(grep "model name" /proc/cpuinfo | awk '{ print $4 }'  | sed 's/(R)//g' | tr '[:upper:]' '[:lower:]' | head -n 1)
arch-chroot /mnt pacman -S --noconfirm $vendor-ucode
sed -i "/^linux.*/a initrd  /$vendor-ucode.img" /mnt/boot/loader/entries/arch.conf

arch-chroot /mnt bootctl update
genfstab -U /mnt > /mnt/etc/fstab

cat <<EOF > /mnt/etc/mkinitcpio.conf
MODULES="nvme atkbd"
BINARIES=""
FILES=""
HOOKS="base systemd keyboard autodetect sd-vconsole modconf block $encrypt_hook filesystems fsck"
EOF

read -r -p "Hostname ? " hostname

echo $hostname > /mnt/etc/hostname
cat <<EOF > /mnt/etc/hosts
127.0.0.1	localhost.localdomain	localhost $hostname
::1		localhost.localdomain	localhost $hostname
EOF

cat <<EOF > /mnt/etc/vconsole.conf
KEYMAP=fr
XKBLAYOUT=fr
XKBMODEL=altwin:swap_lalt_lwin
EOF
chattr +i /mnt/etc/vconsole.conf

echo 'LANG=en_US.UTF-8' > /mnt/etc/locale.conf
cat <<EOF > /mnt/etc/locale.gen
en_US.UTF-8 UTF-8
fr_FR.UTF-8 UTF-8
EOF
arch-chroot /mnt locale-gen

rm -f /mnt/etc/localtime
ln -s /usr/share/zoneinfo/Europe/Paris /mnt/etc/localtime

nb_cpus=$(cat /proc/cpuinfo | grep processor | wc -l)
sed -i -E "s/^#MAKEFLAGS=.*/MAKEFLAGS=\"-j $nb_cpus\"/g" /mnt/etc/makepkg.conf
userdel -R /mnt -r sacha
groupdel -R /mnt users
groupadd -R /mnt users
useradd -c "Sacha Trémoureux" -g users -m -R /mnt sacha

read -r -s -p "Root passwd? " root_passwd
echo
read -r -s -p "User passwd? " user_passwd
echo

echo root:$root_passwd | chpasswd -R /mnt
echo sacha:$user_passwd | chpasswd -R /mnt
arch-chroot /mnt /usr/bin/sed -i 's/# %wheel ALL=(ALL:ALL) ALL/%wheel ALL=(ALL:ALL) NOPASSWD:ALL/g' /etc/sudoers

arch-chroot /mnt pacman -S --noconfirm fish
usermod -R /mnt -G wheel -a sacha
usermod -R /mnt -s /usr/bin/fish sacha
usermod -R /mnt -s /bin/fish root
