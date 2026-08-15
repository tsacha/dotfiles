# Installation macOS

## Dotfiles publics

Installer les outils de bootstrap, puis cloner et appliquer le dépôt public.
Le premier clone passe par HTTPS : la configuration SSH de la YubiKey n'est pas
encore disponible.

```bash
xcode-select --install
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install chezmoi
mkdir -p ~/Git
git clone https://github.com/tsacha/dotfiles.git ~/Git/dotfiles
chezmoi -S ~/Git/dotfiles apply
```

`apply` installe le Brewfile et applique les defaults système (trackpad, dock,
raccourcis clavier et layout Ergo-L). Se déconnecter puis se reconnecter si le
layout n'est pas immédiatement actif.

## skhd

```bash
brew install koekeishiya/formulae/skhd
brew services start skhd
```

## YubiKey / GPG

Les dotfiles publics viennent d'installer `gpg-agent.conf`, avec le support
SSH et `pinentry-mac`. Redémarrer l'agent, importer la clé publique et créer
les stubs de carte :

```bash
gpgconf --kill gpg-agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpg --fetch-keys https://keys.openpgp.org/vks/v1/by-fingerprint/50C5DE2634B79DEA87206C8B27165D579CEB4919
gpg --card-status
gpg --list-secret-keys
ssh-add -L
ssh -T git@github.com
git -C ~/Git/dotfiles remote set-url origin git@github.com:tsacha/dotfiles.git
git -C ~/Git/dotfiles remote add gh git@github.com:tsacha/dotfiles.git
```

`gpg --list-secret-keys` doit afficher `sec#` et les sous-clés `ssb>` :
les opérations privées restent sur la YubiKey.

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
