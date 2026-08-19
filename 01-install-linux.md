# Installation Linux (Arch)

Pense-bête post-installation : utiliser `archinstall` pour l'installation de
base, puis vérifier que les hooks de l'initramfs couvrent le chiffrement
(`systemd` et `sd-encrypt`).

## Dotfiles publics

Installer les prérequis puis cloner le dépôt public :

```bash
sudo pacman -Syu --needed git chezmoi fish gnupg pcsclite ccid curl
mkdir -p ~/Git
git clone https://github.com/tsacha/dotfiles.git ~/Git/dotfiles
```

## TPM et démarrage chiffré

Remplacer `/dev/nvme1n1p2` par la partition LUKS racine avant d'exécuter ces
commandes en root :

```bash
systemd-cryptenroll /dev/nvme1n1p2 --wipe-slot=tpm2 --tpm2-device=auto --tpm2-pcrs=7
echo "root UUID=$(blkid /dev/nvme1n1p2 -s UUID -o value) none tpm2-device=auto" > /etc/crypttab.initramfs
```

## YubiKey

Activer le démon PC/SC, puis importer la clé publique depuis une session de
l'utilisateur normal :

```bash
sudo systemctl enable --now pcscd.service
gpg --fetch-keys https://keys.openpgp.org/vks/v1/by-fingerprint/50C5DE2634B79DEA87206C8B27165D579CEB4919
gpg --card-status
gpg --list-secret-keys
ssh -T git@github.com
git -C ~/Git/dotfiles remote set-url origin git@github.com:tsacha/dotfiles.git
git -C ~/Git/dotfiles remote add gh git@github.com:tsacha/dotfiles.git
```

La sortie de `gpg --list-secret-keys` doit afficher `sec#` et les sous-clés
`ssb>` : les opérations privées restent alors sur la YubiKey.

## Layout Ergol

```bash
sudo curl -fsSL -o /usr/share/X11/xkb/symbols/ergol_anglemod \
  https://github.com/Nuclear-Squid/ergol/releases/download/ergol-v1.0.2/ergol_angle_mod.xkb_symbols
```

## Paquets Arch et AUR

Depuis une session utilisateur, installer les paquets officiels, l'assistant
AUR, les paquets AUR, puis appliquer les dotfiles :

```bash
chezmoi execute-template -S ~/Git/dotfiles --file pkglist.txt.tmpl | sudo pacman -S --needed -
git clone https://aur.archlinux.org/yay.git /tmp/yay
(cd /tmp/yay && makepkg -si)
chezmoi execute-template -S ~/Git/dotfiles --file pkglist-aur.txt.tmpl | yay -S --needed -
chezmoi -S ~/Git/dotfiles apply
```

## Configuration privée

Cloner le dépôt `security`, puis lancer Fish pour disposer de l'alias `cms`
installé par les dotfiles :

```bash
git clone git@github.com:tsacha/security.git ~/Git/security
fish
```

```fish
cms apply -v
```
