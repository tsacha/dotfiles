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
    echo -n $luks_password | cryptsetup luksOpen $data_disk $cryptname -
    echo "Luks opened."
    data_uuid=`blkid $data_disk -s UUID -o value`
    data_disk=/dev/mapper/$cryptname
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

read -r -p "Fix Razer sleep y/N? : " razer
echo
if [ "$razer" == "y" ]; then
    razer_fix_sleep=" button.lid_init_state=open"
else
    razer_fix_sleep=""
fi

echo 'default arch' > /mnt/boot/loader/loader.conf
if [ "$luks" == "y" ]; then
    cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options cryptdevice=UUID=$data_uuid:$cryptname root=$data_disk rootfstype=$fs_type add_efi_memmap$razer_fix_sleep
EOF
else
    cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options root=$data_disk rootfstype=$fs_type add_efi_memmap$razer_fix_sleep
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

cat <<EOF > /mnt/etc/mkinitcpio.conf
MODULES="$modules_vmware nvme atkbd"
BINARIES=""
FILES=""
HOOKS="base udev modconf block keyboard keymap $encrypt_hook filesystems fsck"
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
arch-chroot /mnt useradd sacha -c "Sacha Trémoureux" -g users -G wheel,network,video,audio,optical,floppy,storage,scanner,input,power,http,log,sys,rfkill -m

read -r -s -p "Root passwd? " root_passwd
echo
read -r -s -p "User passwd? " user_passwd
echo

arch-chroot /mnt /bin/bash -c "echo root:$root_passwd | chpasswd"
arch-chroot /mnt /bin/bash -c "echo sacha:$user_passwd | chpasswd"
arch-chroot /mnt /usr/bin/sed -i '# %wheel ALL=(ALL) ALL/wheel ALL=(ALL) ALL/g' /etc/sudoers

read -r -p "Install NetworkManager y/N? : " nm_install
echo
if [ "$nm_install" == "y" ]; then
    arch-chroot /mnt pacman -S --noconfirm networkmanager networkmanager-openvpn
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

arch-chroot /mnt pacman -S --noconfirm base-devel yajl vim tmux gdisk btrfs-progs efibootmgr w3m rsync ansible git subversion bzr openssh net-tools reflector parallel the_silver_searcher wpa_supplicant bash-completion irssi python-yaml rsync isync docker jre8-openjdk icedtea-web bind-tools gnuplot zbar davfs2 cadaver gmime xapian-core xtrans autoconf-archive openvpn lsof sshfs arch-install-scripts ntfs-3g tcpdump go go-tools zsh firewalld dnsmasq ntp htop openbsd-netcat jq wget ipcalc llvm yapf nfs-utils virtualbox linux-headers xorg-server mesa xf86-input-libinput xf86-input-synaptics xf86-video-intel xorg-xbacklight xorg-xinit emacs auctex i3-wm i3lock i3status rofi dmenu conky xfce4-terminal thunar thunar-archive-plugin thunar-media-tags-plugin thunar-volman pulseaudio pavucontrol compton ttf-dejavu ttf-droid adobe-source-code-pro-fonts gajim feh firefox thunderbird libreoffice-fresh sxiv redshift okular vinagre freerdp spice phonon-qt5-gstreamer transmission-qt xfce4-notifyd vlc evince atom texlive-most inkscape pandoc ttf-liberation ttf-dejavu ttf-linux-libertine ttf-linux-libertine-g arandr sway network-manager-applet sddm keybase ttf-fira-sans ttf-fira-mono pass virt-manager openssh-askpass virt-viewer qemu qemu-arch-extra qemu-guest-agent samba cups a2ps wireshark-gtk vnstat scrot gimp markdown gnome-alsamixer alsa-utils pamixer termite noto-fonts noto-fonts-emoji noto-fonts-extra lxappearance-gtk3 system-config-printer hplip lxc rdesktop playerctl acpi flameshot vagrant terraform vault exa fd bat flatpak

arch-chroot /mnt flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
arch-chroot /mnt flatpak install -y flathub com.spotify.Client
arch-chroot /mnt flatpak install -y flathub org.telegram.desktop
arch-chroot /mnt flatpak install -y flathub com.visualstudio.code
arch-chroot /mnt flatpak install -y flathub com.dropbox.Client


arch-chroot /mnt systemctl enable org.cups.cupsd
arch-chroot /mnt systemctl enable ntpd
arch-chroot /mnt systemctl enable cups-browsed.service
echo "a4" > /mnt/etc/papersize
arch-chroot /mnt systemctl enable nscd.service
arch-chroot /mnt systemctl enable libvirtd.service
arch-chroot /mnt usermod -G lp -a sacha
arch-chroot /mnt usermod -G libvirt -a sacha
arch-chroot /mnt usermod -G kvm -a sacha
arch-chroot /mnt systemctl enable sddm

if [ "$vmware" == "y" ]; then
    arch-chroot /mnt pacman -S xf86-input-vmmouse xf86-video-vmware
    echo 'needs_root_rights=yes' > /mnt/X11/Xwrapper.config
fi
cat <<EOF > /mnt/etc/modprobe.d/nobeep.conf
blacklist pcspkr
EOF

arch-chroot /mnt pacman -S --noconfirm gnome gnome-extra
arch-chroot /mnt pacman -S --noconfirm plasma plasma-meta kde-applications

read -r -p "Install Nvidia proprietary drivers y/N? : " nvidia
echo
if [ "$nvidia" == "y" ]; then
    arch-chroot /mnt pacman -S --noconfirm nvidia libva-vdpau-driver
    cat <<EOF > /mnt/etc/modprobe.d/nvidia.conf
blacklist nouveau
EOF
fi
read -r -p "Laptop configuration y/N? : " laptop
echo
if [ "$laptop" == "y" ]; then
    cat <<EOF > /mnt/etc/X11/xorg.conf.d/30-touchpad.conf
Section "InputClass"
    Identifier "devname"
    Driver "libinput"
    Option "Tapping" "on"
EndSection
EOF
fi

arch-chroot /mnt ln -f -s /home/sacha/Dropbox/Documents/ /home/sacha/Documents
arch-chroot /mnt ln -f -s /home/sacha/Dropbox/Documents/Work/iRaiser /home/sacha/Work
arch-chroot /mnt ln -f -s /home/sacha/Dropbox/Pictures/ /home/sacha/Pictures
arch-chroot /mnt mkdir /home/sacha/Downloads
arch-chroot /mnt mkdir /home/sacha/Git
arch-chroot /mnt mkdir /home/sacha/Git/Work
arch-chroot /mnt mkdir /home/sacha/Work
arch-chroot /mnt mkdir /home/sacha/.config
arch-chroot /mnt mkdir /home/sacha/.config/systemd/
arch-chroot /mnt mkdir /home/sacha/.config/systemd/user
arch-chroot /mnt mkdir /home/sacha/.config/compton/
arch-chroot /mnt mkdir /home/sacha/.config/i3
arch-chroot /mnt mkdir /home/sacha/.config/conky
arch-chroot /mnt mkdir /home/sacha/.config/rofi
arch-chroot /mnt mkdir /home/sacha/.config/rofi-pass
arch-chroot /mnt mkdir /home/sacha/.config/xfce4
arch-chroot /mnt mkdir /home/sacha/.config/xfce4/terminal
rm -Rf /mnt/home/sacha/Git/dotfiles/
arch-chroot /mnt git clone https://github.com/tsacha/dotfiles /home/sacha/Git/dotfiles
arch-chroot /mnt git clone https://github.com/robbyrussell/oh-my-zsh.git /home/sacha/.oh-my-zsh/
arch-chroot /mnt git clone https://github.com/robbyrussell/oh-my-zsh.git /root/.oh-my-zsh/

arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/i3/config /home/sacha/.config/i3/config
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/compton/compton.conf /home/sacha/.config/compton/compton.conf
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/conky/conkyrc /home/sacha/.config/conky/conkyrc
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/conky/conky-i3bar /home/sacha/.config/conky/conky-i3bar
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/shell/bashrc /home/sacha/.bashrc
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/shell/zshrc /home/sacha/.zshrc
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/shell/zshrc-root /root/.zshrc
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/terminal/terminalrc /home/sacha/.config/xfce4/terminal/terminalrc
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/mbsync/mbsyncrc /home/sacha/.mbsyncrc
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/rofi/config /home/sacha/.config/rofi/config
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/rofi-pass/config /home/sacha/.config/rofi-pass/config
arch-chroot /mnt chmod 755 /home/sacha/.config/conky/conky-i3bar

arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/xfce4-notifyd.service /home/sacha/.config/systemd/user/xfce4-notifyd.service
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/compton.service /home/sacha/.config/systemd/user/compton.service
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/nm-applet.service /home/sacha/.config/systemd/user/nm-applet.service
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/redshift.service /home/sacha/.config/systemd/user/redshift.service
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/systemd/systemd /home/sacha/.config/systemd/user/systemd

arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/xorg/keyboard-layout.conf /etc/X11/xorg.conf.d/10-keyboard-layout.conf
arch-chroot /mnt mkdir /home/sacha/.config/systemd/user/default.target.wants
arch-chroot /mnt mkdir /home/sacha/.config/systemd/user/emacs.service.d
arch-chroot /mnt ln -f -s /usr/lib/systemd/user/emacs.service /home/sacha/.config/systemd/user/default.target.wants/emacs.service
arch-chroot /mnt ln -f -s /home/sacha/.config/systemd/user/default.target.wants/redshift.service /home/sacha/Git/dotfiles/systemd/redshift.service
arch-chroot /mnt ln -f -s /home/sacha/.config/systemd/user/default.target.wants/dropbox.service /home/sacha/Git/dotfiles/systemd/dropbox.service
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/override-emacs-unit.conf /home/sacha/.config/systemd/user/emacs.service.d/override.conf
arch-chroot /mnt mkdir /home/sacha/.config/termite
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/termite/config.dark /home/sacha/.config/termite/config
arch-chroot /mnt ln -f -s /home/sacha/Git/dotfiles/mime/mimeapps.list /home/sacha/.config/mimeapps.list
arch-chroot /mnt ln -f -s /home/sacha/Security/Store /home/sacha/.password-store
arch-chroot /mnt ln -f -s /home/sacha/Security/Work/Ansible/ansible_credentials.json /home/sacha/.ansible_credentials.json
arch-chroot /mnt ln -f -s /home/sacha/Security/Work/AWS/credentials /home/sacha/.aws/credentials
arch-chroot /mnt ln -f -s /home/sacha/Security/Work/AWS/config /home/sacha/.aws/config
arch-chroot /mnt chown sacha.users -Rf /home/sacha
arch-chroot /mnt usermod -s /usr/bin/zsh sacha
arch-chroot /mnt usermod -s /bin/zsh root
