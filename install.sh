#!/bin/bash
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

if [ -b "/dev/mapper/encrypted_disk" ]; then
    echo -n "Close existing luks device… "
    cryptsetup luksClose /dev/mapper/encrypted_disk
    echo "Done."
fi

read -r -p "Disk to use? " disk

read -r -p "Auto partitioning? (y/N) : " auto_p
if [ "$auto_p" == "y" ]; then
    boot_size_mb=512
    sgdisk -og $disk
    start_sector=$(sgdisk -F $disk)
    sector_size=$(cat /sys/block/$(basename $disk)/queue/physical_block_size)
    alignment=$(sgdisk -D $disk)
    end_sector=$(sgdisk -E $disk)
    let end_boot_sector="(($boot_size_mb * 1024 * 1024 / $sector_size) + $alignment - 1)"

    echo "Create partitions…"
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
read -r -p "Create LUKS device? (y/N) : " luks
if [ "$luks" == "y" ]; then
    encrypt_hook="encrypt"
    read -s -p "Luks password: " luks_password
    echo
    echo -n $luks_password | cryptsetup -q luksFormat $data_disk -
    echo "Luks created."
    echo -n $luks_password | cryptsetup luksOpen $data_disk encrypted_disk -
    echo "Luks opened."
    data_uuid=`blkid $data_disk -s UUID -o value`
    data_disk=/dev/mapper/encrypted_disk
fi

read -r -p "Format boot partition? y/N " format_boot
if [ "$format_boot" == "y" ]; then
    echo -n "Format boot partition… "
    mkfs.vfat -F32 $boot_disk > /dev/null 2> /dev/null
    echo "Done."
fi

fs_options=""
read -r -p "Filesystem for data partition? (ext4 by default) " fs_type
if [ -z "$fs_type" ]; then
    fs_type="ext4"
fi

if [ "$fs_type" == "btrfs" ]; then
    fs_options=" -f"
fi;

echo -n "Format data partition… "
mkfs -t $fs_type $fs_options $data_disk > /dev/null 2> /dev/null
echo "Done."

mount $data_disk /mnt
if [ ! -e /mnt/boot ]; then
    echo "Creating /boot partition."
    mkdir /mnt/boot
fi
mount $boot_disk /mnt/boot
pacstrap /mnt base

arch-chroot /mnt bootctl install

echo 'default arch' > /mnt/boot/loader/loader.conf
if [ "$luks" == "y" ]; then
    cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options cryptdevice=UUID=$data_uuid:encrypted_disk root=$data_disk rootfstype=$fs_type add_efi_memmap
EOF
else
    cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options root=$data_disk rootfstype=$fs_type add_efi_memmap
EOF
fi

arch-chroot /mnt bootctl update
genfstab -U /mnt > /mnt/etc/fstab


read -r -p "VMWare guest? (y/N) : " vmware
if [ "$vmware" == "y" ]; then
    arch-chroot /mnt pacman -S --noconfirm open-vm-tools
    modules_vmware="vmw_balloon vmw_pvscsi vsock vmw_vsock_vmci_transport"
    arch-chroot /mnt systemctl enable vmtoolsd
fi

read -r -p "NVMe support? (y/N) : " nvme
if [ "$nvme" == "y" ]; then
    modules_nvme="nvme"
fi

cat <<EOF > /boot/etc/mkinitcpio.conf
MODULES="$modules_vmware $modules_nvme"
BINARIES=""
FILES=""
HOOKS="base udev autodetect modconf block keyboard keymap $encrypt_hook filesystems fsck"
EOF

arch-chroot /mnt mkinitcpio -p linux

read -r -p "Hostname ? " hostname

echo $hostname > /mnt/etc/hostname
cat <<EOF > /mnt/etc/hosts
127.0.0.1	localhost.localdomain	localhost $hostname
::1		localhost.localdomain	localhost $hostname
EOF
echo 'KEYMAP=fr-bepo-latin9' > /mnt/etc/vconsole.conf
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
arch-chroot /mnt userdel sacha
arch-chroot /mnt useradd sacha -c "Sacha Trémoureux" -g 100 -G wheel,network,video,audio,optical,floppy,storage,scanner,input,power,http,log,sys,rfkill -m

read -r -s -p "Root passwd? " root_passwd
echo
read -r -s -p "User passwd? " user_passwd
echo

arch-chroot /mnt /bin/bash -c "echo root:$root_passwd | chpasswd"
arch-chroot /mnt /bin/bash -c "echo sacha:$user_passwd | chpasswd"

read -r -p "Install NetworkManager y/N? : " nm_install
echo
if [ "$nm_install" == "y" ]; then
    arch-chroot /mnt pacman -S --noconfirm networkmanager
    arch-chroot /mnt systemctl enable NetworkManager
fi

read -r -p "Install systemd-networkd y/N? : " ntwkd_install
echo
if [ "$ntwkd_install" == "y" ]; then
    mkdir -p /mnt/etc/systemd/network
    read -r -p "Network interface ? " interface
    read -r -p "DHCP ? Y/n : " dhcp
    if [ "$dhcp" == "n" ]; then
	read -r -p "IPv4 ? " ipv4
	read -r -p "CIDR ? " cidr4
	read -r -p "Gateway ? " gateway4
	read -r -p "DNS ? " dns
	cat <<EOF > /mnt/etc/systemd/network/$interface.network
[Match]
Name=$interface

[Network]
Address=$ipv4/$cidr4
Gateway=$gateway4
DNS=$dns

IPv6AcceptRA=true
EOF
    else
	cat <<EOF > /mnt/etc/systemd/network/$interface.network
[Match]
Name=$interface

[Network]
DHCP=ipv4
IPv6AcceptRA=true
EOF
    fi
    arch-chroot /mnt systemctl enable systemd-networkd
fi

arch-chroot /mnt pacman -S --noconfirm base-devel yajl vim tmux gdisk btrfs-progs efibootmgr w3m rsync ansible git openssh net-tools reflector parallel the_silver_searcher wpa_supplicant bash-completion irssi python-yaml rsync isync docker jre8-openjdk icedtea-web bind-tools gnuplot zbar davf2s cadaver gmime xapian-core xtrans autoconf-archive

cat /mnt/etc/pacman.conf | grep archlinuxfr > /dev/null
if [ ! -z $? ]; then
    cat <<EOF >> /mnt/etc/pacman.conf

[archlinuxfr]
SigLevel = Never
Server = http://repo.archlinux.fr/\$arch
EOF
fi

arch-chroot /mnt pacman -Sy --noconfirm yaourt

read -r -p "Install desktop environment y/N? : " desktop
echo
if [ "$desktop" == "y" ]; then
    arch-chroot /mnt pacman -S --noconfirm xorg-server mesa xf86-input-libinput xf86-input-synaptics xf86-video-intel xorg-xbacklight xorg-xinit emacs auctex i3-wm i3lock i3status rofi dmenu conky st xfce4-terminal thunar thunar-archive-plugin thunar-media-tags-plugin thunar-volman pulseaudio pavucontrol compton ttf-dejavu adobe-source-code-pro-fonts gajim feh firefox thunderbird libreoffice-fresh sxiv redshift okular vinagre freerdp spice phonon-qt4-gstreamer transmission-qt qt4 xfce4-notifyd vlc evince atom texlive-most texlive-lang inkscape pandoc ttf-liberation ttf-dejavu ttf-linux-libertine ttf-linux-libertine-g arandr sway network-manager-applet sddm keybase ttf-fira-sans ttf-fira-mono
    arch-chroot /mnt systemctl enable sddm

    if [ "$vmware" == "y" ]; then
	arch-chroot /mnt pacman -S xf86-input-vmmouse xf86-video-vmware
	echo 'needs_root_rights=yes' > /mnt/X11/Xwrapper.config
    fi

    read -r -p "Install Gnome environment y/N? : " gnome
    echo
    if [ "$gnome" == "y" ]; then
	arch-chroot /mnt pacman -S --noconfirm gnome gnome-extra
    fi
    read -r -p "Install KDE environment y/N? : " kde
    echo
    if [ "$kde" == "y" ]; then
	arch-chroot /mnt pacman -S --noconfirm plasma plasma-meta
    fi

    read -r -p "Install laptop packages y/N? : " laptop
    echo
    if [ "$laptop" == "y" ]; then
	arch-chroot /mnt pacman -S --noconfirm acpi
	cat <<EOF > /mnt/etc/X11/xorg.conf/30-touchpad.conf
Section "InputClass"
    Identifier "devname"
    Driver "libinput"
    Option "Tapping" "on"
EndSection
EOF
    fi

    arch-chroot /mnt mkdir /home/sacha/Cloud
    arch-chroot /mnt mkdir /home/sacha/Big-Cloud
    arch-chroot /mnt mkdir /home/sacha/Downloads
    arch-chroot /mnt mkdir /home/sacha/Git
    arch-chroot /mnt mkdir /home/sacha/Public
    arch-chroot /mnt mkdir /home/sacha/Mails
    arch-chroot /mnt mkdir /home/sacha/Mails/Sent/
    arch-chroot /mnt mkdir /home/sacha/Mails/Drafts/
    arch-chroot /mnt mkdir /home/sacha/Mails/Trash/
    arch-chroot /mnt mkdir /home/sacha/.config
    arch-chroot /mnt mkdir /home/sacha/.config/systemd/
    arch-chroot /mnt mkdir /home/sacha/.config/systemd/user
    arch-chroot /mnt mkdir /home/sacha/.config/compton/
    arch-chroot /mnt mkdir /home/sacha/.config/i3
    arch-chroot /mnt mkdir /home/sacha/.config/conky
    arch-chroot /mnt mkdir /home/sacha/.config/rofi
    arch-chroot /mnt mkdir /home/sacha/.config/xfce4
    arch-chroot /mnt mkdir /home/sacha/.config/xfce4/terminal
    arch-chroot /mnt ln -s /home/sacha/Cloud/Documents/ /home/sacha/Documents
    arch-chroot /mnt ln -s /home/sacha/Big-Cloud/Music /home/sacha/Music
    arch-chroot /mnt ln -s /home/sacha/Big-Cloud/Pictures /home/sacha/Pictures
    arch-chroot /mnt ln -s /home/sacha/Big-Cloud/Videos /home/sacha/Videos
    rm -Rf /home/sacha/Git/dotfiles/
    arch-chroot /mnt git clone https://github.com/tsacha/dotfiles /home/sacha/Git/dotfiles

    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/i3/config /home/sacha/.config/i3/config
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/compton/compton.conf /home/sacha/.config/compton/compton.conf
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/conky/conkyrc /home/sacha/.config/conky/conkyrc
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/conky/conky-i3bar /home/sacha/.config/conky/conky-i3bar
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/shell/bashrc /home/sacha/.bashrc
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/terminal/terminalrc /home/sacha/.config/xfce4/terminal/terminalrc
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/mbsync/mbsyncrc /home/sacha/.mbsyncrc
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/rofi/config /home/sacha/.config/rofi/config
    arch-chroot /mnt chmod 755 /home/sacha/.config/conky/conky-i3bar

    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/compton.service /home/sacha/.config/systemd/user/compton.service
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/nm-applet.service /home/sacha/.config/systemd/user/nm-applet.service
    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/redshift.service /home/sacha/.config/systemd/user/redshift.service

    arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/xorg/keyboard-layout.conf /etc/X11/xorg.conf.d/10-keyboard-layout.conf
    chown sacha.users -Rf /mnt/home/sacha
fi