#!/bin/bash
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
    fd \
    fzf \
    iperf3 \
    ripgrep \
    btop \
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
    virt-viewer \
    qemu-full \
    qemu-arch-extra \
    qemu-guest-agent \
    wireshark-qt \
    scrot \
    gimp \
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

arch-chroot /mnt /usr/bin/sed -Ei 's/^#greeter-session.*/greeter-session=lightdm-gtk-greeter/g' /etc/lightdm/lightdm.conf
arch-chroot /mnt /usr/bin/sed -Ei 's/^#?background=.*/background=\/var\/cache\/background/g' /etc/lightdm/lightdm-gtk-greeter.conf

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
arch-chroot /mnt localectl set-x11-keymap fr altwin:swap_lalt_lwin bepo

rm -Rf /mnt/home/sacha/Git/dotfiles
arch-chroot /mnt git clone https://github.com/tsacha/dotfiles /home/sacha/Git/dotfiles
arch-chroot /mnt chown sacha.users -Rf /home/sacha

