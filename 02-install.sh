#!/bin/bash
arch-chroot /mnt pacman -S --noconfirm \
    acpi \
    adobe-source-code-pro-fonts \
    age \
    alacritty \
    alsa-utils \
    ansible \
    arandr \
    arc-gtk-theme \
    arc-icon-theme \
    arch-install-scripts \
    aws-cli \
    bash-completion \
    bat \
    bind-tools \
    breeze \
    breeze-gtk \
    btop \
    btrfs-progs \
    chezmoi \
    chromium \
    conky \
    consul \
    consul-template \
    delve \
    dmenu \
    docker \
    docker-compose \
    dust \
    efibootmgr \
    emacs \
    evince \
    eza \
    fd \
    fd \
    feh \
    firefox \
    fisher \
    flameshot \
    flatpak \
    freerdp \
    fzf \
    gdisk \
    gimp \
    git \
    git-lfs \
    github-cli \
    glow \
    gnome-settings-daemon \
    gnome-themes-standard \
    go \
    go-tools \
    gopass \
    gopls \
    htop \
    httpie \
    hyperv \
    i3-wm \
    i3lock \
    i3status \
    inetutils \
    ipcalc \
    iperf3 \
    isync \
    jq \
    libreoffice-fresh \
    libva-mesa-driver \
    libvirt \
    libvncserver \
    lightdm \
    lightdm-gtk-greeter \
    linux-headers \
    locate \
    lsof \
    lxappearance-gtk3 \
    lxc \
    mesa \
    mesa-vdpau \
    net-tools \
    network-manager-applet \
    networkmanager-openvpn \
    nfs-utils \
    nomad \
    noto-fonts \
    noto-fonts-emoji \
    noto-fonts-extra \
    ntfs-3g \
    ntp \
    okular \
    okular \
    openbsd-netcat \
    openssh \
    oxygen \
    pamixer \
    parallel \
    pavucontrol \
    phonon-qt5-gstreamer \
    picom \
    pipewire-alsa \
    pipewire-audio \
    pipewire-pulse \
    python-black \
    python-yaml \
    qemu-arch-extra \
    qemu-full \
    qemu-guest-agent \
    qt5ct \
    rdesktop \
    redshift \
    reflector \
    remmina \
    ripgrep \
    rofi \
    rofi-emoji \
    rsync \
    rsync \
    scrot \
    spice-gtk \
    sshfs \
    starship \
    sway \
    swaylock \
    sxiv \
    tcpdump \
    terraform \
    the_silver_searcher \
    thunderbird \
    tmux \
    tpm2-tss \
    ttf-dejavu \
    ttf-dejavu \
    ttf-droid \
    ttf-fira-mono \
    ttf-fira-sans \
    ttf-iosevka-nerd \
    ttf-liberation \
    ttf-linux-libertine \
    ttf-linux-libertine-g \
    udiskie \
    unzip \
    vagrant \
    vault \
    vim \
    virt-manager \
    virt-viewer \
    virt-what \
    vlc \
    w3m \
    wget \
    wireshark-qt \
    wpa_supplicant \
    xclip \
    xf86-input-libinput \
    xf86-input-synaptics \
    xf86-video-intel \
    xfce4-notifyd \
    xfce4-terminal \
    xorg-server \
    xorg-xbacklight \
    xorg-xinit \
    xorg-xinput \
    xorg-xwayland \
    yajl \
    yapf \
    zsh

arch-chroot /mnt /usr/bin/sed -Ei 's/^#greeter-session.*/greeter-session=lightdm-gtk-greeter/g' /etc/lightdm/lightdm.conf
arch-chroot /mnt /usr/bin/sed -Ei 's/^#autologin-user.*/autologin-user=sacha/g' /etc/lightdm/lightdm.conf
arch-chroot /mnt /usr/bin/sed -Ei 's/^#?background=.*/background=\/var\/cache\/background/g' /etc/lightdm/lightdm-gtk-greeter.conf

arch-chroot /mnt systemctl enable ntpd
arch-chroot /mnt systemctl enable nscd.service
arch-chroot /mnt systemctl enable firewalld.service
arch-chroot /mnt systemctl enable libvirtd.service
arch-chroot /mnt systemctl enable NetworkManager.service
groupadd -R /mnt -r autologin
usermod -R /mnt -G autologin -a sacha
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
arch-chroot /mnt localectl set-x11-keymap fr altwin:swap_lalt_lwin bepo_afnor

rm -Rf /mnt/home/sacha/Git/dotfiles
arch-chroot /mnt git clone https://github.com/tsacha/dotfiles /home/sacha/Git/dotfiles
arch-chroot /mnt chown sacha.users -Rf /home/sacha

