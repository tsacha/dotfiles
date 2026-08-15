# Installation Windows + WSL2

Ce poste de développement tourne dans WSL2/Arch. Windows fournit le terminal,
le clavier et l'accès à la YubiKey.

## 1. Installer les prérequis Windows

Dans PowerShell lancé en administrateur :

```powershell
wsl --update
wsl --install archlinux
Get-WindowsCapability -Online | Where-Object Name -Like 'OpenSSH.Client*'
Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0
winget install --exact --source winget GnuPG.Gpg4win Git.Git albertony.npiperelay raphamorim.rio twpayne.chezmoi
```

Redémarrer le terminal après `winget` : Git et Gpg4win viennent d'être ajoutés
au `PATH`.

### Police et clavier

Installer GeistMono Nerd Font (les fichiers Nerd Fonts statiques, et non la
police Geist de Vercel, sont requis par Rio) :

```powershell
Invoke-WebRequest https://github.com/ryanoasis/nerd-fonts/releases/latest/download/GeistMono.zip -OutFile $env:TEMP\GeistMono.zip
Expand-Archive $env:TEMP\GeistMono.zip $env:TEMP\GeistMono -Force
$fonts = "$env:LOCALAPPDATA\Microsoft\Windows\Fonts"
mkdir -Force $fonts
Get-ChildItem $env:TEMP\GeistMono\GeistMonoNerdFont-*.otf | ForEach-Object {
    Copy-Item $_.FullName $fonts
    New-ItemProperty 'HKCU:\Software\Microsoft\Windows NT\CurrentVersion\Fonts' `
        -Name "$($_.BaseName) (OpenType)" -Value "$fonts\$($_.Name)" -Force | Out-Null
}
```

Installer ensuite le layout Ergo-L avec
[l'installeur officiel](https://github.com/Nuclear-Squid/ergol/releases/download/ergol-v1.0.2/ergol_angle_mod_kbd.exe),
puis rouvrir la session Windows. `ergol.ahk` reste seulement un secours
userland. Le remappage Caps Lock ↔ Escape se fait via `Scancode Map` sous
`HKLM\SYSTEM\CurrentControlSet\Control\Keyboard Layout`, puis un redémarrage.

## 2. Préparer la YubiKey sous Windows

Gpg4win garde la carte ; WSL s'y connectera ensuite à travers `npiperelay`.
Avant le premier clone SSH, créer la configuration de l'agent :

```powershell
mkdir -Force $env:APPDATA\gnupg
@"
enable-win32-openssh-support
"@ | Set-Content $env:APPDATA\gnupg\gpg-agent.conf
```

Dans une console PowerShell lancée en administrateur, libérer le pipe SSH :

```powershell
Stop-Service ssh-agent
Set-Service ssh-agent -StartupType Disabled
```

Puis, dans un terminal normal :

```powershell
gpgconf --kill gpg-agent
gpg-connect-agent /bye
gpg --card-status
gpg --fetch-keys https://keys.openpgp.org/vks/v1/by-fingerprint/50C5DE2634B79DEA87206C8B27165D579CEB4919
gpg --card-status
ssh-add -L
git config --global core.sshCommand C:/Windows/System32/OpenSSH/ssh.exe
ssh -T git@github.com
```

## 3. Cloner et appliquer les dotfiles Windows

```powershell
New-Item -ItemType Directory -Force $env:USERPROFILE\Git
git clone git@github.com:tsacha/dotfiles.git $env:USERPROFILE\Git\dotfiles
git -C $env:USERPROFILE\Git\dotfiles remote add gh git@github.com:tsacha/dotfiles.git
chezmoi -S $env:USERPROFILE\Git\dotfiles apply
```

Cette application ne pose côté Windows que `AppData` (Rio et GPG). Les
dotfiles Linux seront appliqués depuis un clone séparé dans WSL.

## 4. Initialiser Arch dans WSL

Au premier démarrage, Arch ouvre une session `root` :

```bash
pacman -Syu --needed git sudo base-devel chezmoi
useradd -m -G wheel sacha
passwd sacha
printf '%s\n' '%wheel ALL=(ALL:ALL) ALL' | install -m 0440 /dev/stdin /etc/sudoers.d/wheel
visudo -cf /etc/sudoers.d/wheel
printf '[boot]\nsystemd=true\n[user]\ndefault=sacha\n' > /etc/wsl.conf
```

Dans PowerShell, fermer la distribution afin de prendre en compte l'utilisateur
par défaut :

```powershell
wsl --terminate archlinux
```

Rouvrir ensuite Arch en tant que `sacha`. systemd est nécessaire, car les
relais de la YubiKey sont des services utilisateur.

## 5. Installer les dotfiles dans WSL

Les dépôts vivent sur ext4, sous `~/Git`, jamais sous `/mnt/c`. Le premier clone
peut être fait depuis la copie Windows : le relais SSH n'est pas encore installé.

```bash
mkdir -p ~/Git
git clone /mnt/c/Users/<utilisateur-windows>/Git/dotfiles ~/Git/dotfiles
cd ~/Git/dotfiles
chezmoi execute-template -S . --file pkglist.txt.tmpl | sudo pacman -S --needed -
git clone https://aur.archlinux.org/yay-bin.git /tmp/yay-bin
(cd /tmp/yay-bin && makepkg -si)
chezmoi execute-template -S . --file pkglist-aur.txt.tmpl | yay -S --needed -
chezmoi -S . apply
chsh -s /usr/bin/fish
```

Remplacer `<utilisateur-windows>` par le nom du profil Windows.

## 6. Activer la YubiKey dans WSL

Une fois les dotfiles appliqués, activer les relais GPG et SSH, puis importer la
clé publique dans le trousseau WSL :

```bash
systemctl --user daemon-reload
systemctl --user stop gpg-agent.service gpg-agent.socket gpg-agent-ssh.socket gpg-agent-extra.socket gpg-agent-browser.socket
systemctl --user enable --now gpg-agent-relay gpg-agent-ssh-relay
gpg --card-status
gpg --fetch-keys https://keys.openpgp.org/vks/v1/by-fingerprint/50C5DE2634B79DEA87206C8B27165D579CEB4919
gpg --card-status
ssh-add -L
git remote set-url origin git@github.com:tsacha/dotfiles.git
git remote add gh git@github.com:tsacha/dotfiles.git
```

Windows et WSL ont chacun leur trousseau de clés publiques. Les clés secrètes
ne sont pas copiées : `sec#` et `ssb>` indiquent qu'elles restent sur la
YubiKey. Les unités GnuPG natives sont masquées dans WSL ; `dirmngr` reste
local pour les opérations réseau.

## Configuration privée

Le dépôt `security` fournit notamment l'identité Git nécessaire à la signature.
Cloner le dépôt après l'activation du relais SSH :

```bash
git clone git@github.com:tsacha/security.git ~/Git/security
```

Fermer puis rouvrir WSL afin d'utiliser Fish (et donc l'alias `cms` installé
par les dotfiles), puis appliquer la configuration privée :

```fish
cms apply -v
```

## Vérifier

```bash
chezmoi apply --dry-run
test_repo=$(mktemp -d)
git -C "$test_repo" init
git -C "$test_repo" -c user.name=Test -c user.email=test@example.invalid commit --allow-empty -S -m test
git -C "$test_repo" log -1 --show-signature
```

La commande `ssh-add -L` doit aussi afficher la clé SSH de la YubiKey.

## Nettoyer le bootstrap Windows

Quand le clone WSL fonctionne et que les vérifications sont terminées, le clone
temporaire Windows n'est plus nécessaire :

```powershell
Remove-Item -Recurse -Force $env:USERPROFILE\Git\dotfiles
```

Le clone de référence reste `~/Git/dotfiles` dans WSL.
