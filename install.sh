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

echo 'default arch' > /mnt/boot/loader/loader.conf
cat <<EOF > /mnt/boot/loader/entries/arch.conf
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
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
echo 'KEYMAP=fr-bepo-latin9' > /mnt/etc/vconsole.conf
echo 'LANG=en_US.UTF-8' > /mnt/etc/locale.conf
cat <<EOF > /mnt/etc/locale.gen
en_US.UTF-8 UTF-8
fr_FR.UTF-8 UTF-8
EOF
arch-chroot /mnt locale-gen


arch-chroot /mnt pacman -S --noconfirm \
    vim \
    tmux \
    gdisk \
    btrfs-progs \
    efibootmgr \
    w3m \
    rsync \
    ansible \
    git \
    openssh \
    net-tools \
    reflector \
    parallel \
    the_silver_searcher \
    wpa_supplicant \
    bash-completion \
    python-yaml \
    rsync \
    isync \
    docker \
    docker-compose \
    bind-tools \
    gmime \
    lsof \
    sshfs \
    arch-install-scripts \
    tcpdump \
    go \
    go-tools \
    zsh \
    ntp \
    htop \
    openbsd-netcat \
    unzip \
    jq \
    yajl \
    wget \
    ipcalc \
    yapf \
    nfs-utils \
    linux-headers \
    xorg-server \
    xorg-xinput \
    xorg-xwayland \
    mesa \
    xf86-input-libinput \
    xf86-input-synaptics \
    xf86-video-intel \
    xorg-xbacklight \
    xorg-xinit \
    emacs \
    i3-wm \
    i3lock \
    swaylock \
    i3status \
    rofi \
    dmenu \
    conky \
    xfce4-terminal \
    pulseaudio \
    pavucontrol \
    picom \
    adobe-source-code-pro-fonts \
    feh \
    chromium \
    firefox \
    thunderbird \
    libreoffice-fresh \
    sxiv \
    redshift \
    okular \
    remmina \
    libvncserver \
    spice-gtk \
    freerdp \
    xfce4-notifyd \
    phonon-qt5-gstreamer \
    vlc \
    evince \
    pandoc \
    arandr \
    sway \
    network-manager-applet \
    lightdm \
    lightdm-gtk-greeter \
    virt-manager \
    openssh-askpass \
    virt-viewer \
    qemu \
    qemu-arch-extra \
    qemu-guest-agent \
    wireshark-qt \
    scrot \
    gimp \
    markdown \
    alsa-utils \
    pamixer \
    alacritty \
    noto-fonts \
    noto-fonts-emoji \
    noto-fonts-extra \
    lxappearance-gtk3 \
    lxc \
    rdesktop \
    acpi \
    flameshot \
    vagrant \
    terraform \
    vault \
    exa \
    fd \
    flatpak \
    httpie \
    libvirt \
    python-black \
    rofimoji \
    arc-gtk-theme \
    arc-icon-theme \
    qt5ct \
    breeze \
    breeze-gtk \
    oxygen \
    gnome-themes-standard \
    okular \
    ttf-dejavu \
    ttf-droid \
    ttf-liberation \
    ttf-dejavu \
    ttf-linux-libertine \
    ttf-linux-libertine-g \
    ttf-fira-sans \
    ttf-fira-mono \
    ttf-iosevka-nerd \
    gopass \
    xclip \
    age \
    chezmoi

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
arch-chroot /mnt /usr/bin/sed -i 's/# %wheel ALL=(ALL) ALL/%wheel ALL=(ALL) ALL/g' /etc/sudoers
arch-chroot /mnt /usr/bin/sed -Ei 's/^#greeter-session.*/greeter-session=lightdm-gtk-greeter/g' /etc/lightdm/lightdm.conf
arch-chroot /mnt /usr/bin/sed -Ei 's/^#?background=.*/background=\/var\/cache\/background/g' /etc/lightdm/lightdm-gtk-greeter.conf

usermod -R /mnt -G wheel -a sacha
arch-chroot /mnt systemctl enable ntpd
arch-chroot /mnt systemctl enable nscd.service
arch-chroot /mnt systemctl enable firewalld.service
arch-chroot /mnt systemctl enable libvirtd.service
arch-chroot /mnt systemctl enable NetworkManager.service
usermod -R /mnt -G libvirt -a sacha
usermod -R /mnt -G kvm -a sacha
usermod -R /mnt -G docker -a sacha
arch-chroot /mnt systemctl enable lightdm

read -r -p "VMWare configuration y/N? : " vmware
if [ "$vmware" == "y" ]; then
    arch-chroot /mnt pacman -S xf86-input-vmmouse xf86-video-vmware open-vm-tools
    echo 'needs_root_rights=yes' > /mnt/etc/X11/Xwrapper.config
    arch-chroot /mnt systemctl enable vmware-vmblock-fuse.service
    arch-chroot /mnt systemctl enable vmtoolsd.service
fi
cat <<EOF > /mnt/etc/modprobe.d/nobeep.conf
blacklist pcspkr
EOF

read -r -p "Install Nvidia proprietary drivers y/N? : " nvidia
echo
if [ "$nvidia" == "y" ]; then
    arch-chroot /mnt pacman -S --noconfirm nvidia libva-vdpau-driver nvidia-settings
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

cat <<EOF > /mnt/etc/X11/xorg.conf.d/10-keyboard-layout.conf
Section "InputClass"
    Identifier         "Keyboard Layout"
    MatchIsKeyboard    "yes"
    Option             "XkbLayout"  "fr"
    Option             "XkbVariant" "bepo"
EndSection
EOF

rm -Rf /mnt/home/sacha/Git/dotfiles
arch-chroot /mnt git clone https://github.com/tsacha/dotfiles /home/sacha/Git/dotfiles
arch-chroot /mnt chown sacha.users -Rf /home/sacha
arch-chroot -u sacha:users /mnt bash -c "HOME=/home/sacha; chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb apply"

usermod -R /mnt -s /usr/bin/zsh sacha
usermod -R /mnt -s /bin/zsh root
