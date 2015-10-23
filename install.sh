#!/bin/bash
cd ~
mkdir .ssh/ 2> /dev/null
hostname=$1
port=$2
user=$3
if [ -z $hostname ] || [ -z $user ] || [ -z $port ]; then
  echo 'No remote git server provided.'
  exit 1;
fi;

cat << EOF > .ssh/config
Host install
HostName $hostname
Port $port
User $user
IdentityFile ~/.ssh/install_rsa
EOF

ssh -o BatchMode=yes install true 2> /dev/null
if [ $? -eq 255 ]; then
  echo "Can't connect to remote git server.";
  echo 'Generating SSH key…'
  rm .ssh/install_rsa* 2> /dev/null
  ssh-keygen -f .ssh/install_rsa -q -N ''
  echo "Pubkey : $(cat .ssh/install_rsa.pub)"
  exit 2;
fi;

clone() {
  target=$1
  remote_git=$2
  branch=$3
  if [ ! -d $target ]; then
	  git clone "$remote_git" -o install -b $branch $target
  else
	  cd $target
	  git checkout $branch
	  git pull install
	  cd ..
  fi;
}


clone 'Documents' 'git+ssh://install:/srv/Sacha/Documents' 'master'
clone 'Templates' 'git+ssh://install:/srv/Sacha/Templates' 'master'


mkdir Git\ repositories 2> /dev/null
cd Git\ repositories
clone 'GnuPG' 'git+ssh://install:/srv/Sacha/Git repositories/GnuPG' 'laptop'
if [ ! -h ~/.gnupg ]; then
  ln -s ~/Git\ repositories/GnuPG ~/.gnupg
fi

mkdir -p ~/.config/systemd/user/ 2> /dev/null
cd ~
ln ~/Git\ repositories/GnuPG/gpg-agent.service ~/.config/systemd/user/gpg-agent.service 2> /dev/null
systemctl --user enable gpg-agent
systemctl --user start gpg-agent
/bin/echo UPDATESTARTUPTTY | gpg-connect-agent > /dev/null

SSH_AUTH_SOCK=/home/sacha/.gnupg/S.gpg-agent.ssh
export SSH_AUTH_SOCK

cd Git\ repositories
clone 'dotfiles' 'git@github.com:tsacha/dotfiles.git' 'master'
clone 'emacs' 'git@github.com:tsacha/.emacs.d.git' 'master'
if [ ! -h ~/.emacs.d ]; then
  rm -Rf ~/.emacs.d
  ln -s ~/Git\ repositories/emacs ~/.emacs.d
fi
cd ~/.emacs.d/
git submodule init
git submodule update
cd elisp/org-mode/
make autoloads
cd ~/.emacs.d/
cd elisp/mu/
autoreconf -i
./configure
make

mkdir -p ~/Mails/queue/cur/
touch ~/Mails/queue/.noindex


cd ~
rm -f .Xresources
ln -s ~/Git\ repositories/dotfiles/desktop/Xresources ~/.Xresources
rm -Rf .i3/

mkdir .i3/
ln -s ~/Git\ repositories/dotfiles/desktop/i3config ~/.i3/config

rm -f .compton.conf
ln -s ~/Git\ repositories/dotfiles/desktop/compton.conf ~/.compton.conf

rm -f .bashrc
ln -s ~/Git\ repositories/dotfiles/shell/bashrc ~/.bashrc

rm -f .mpd.conf
ln -s ~/Git\ repositories/dotfiles/tools/mpd.conf ~/.mpd.conf

rm -f .i3status.conf
ln -s ~/Git\ repositories/dotfiles/desktop/i3status.conf ~/.i3status.conf

rf -f .offlineimap.py
ln -s ~/Git\ repositories/dotfiles/tools/offlineimap.py ~/.offlineimap.py

rm -f .offlineimaprc
ln -s ~/Git\ repositories/dotfiles/tools/offlineimaprc ~/.offlineimaprc

rm -f .authinfo.gpg
ln -s ~/Documents/Security/mails/authinfo.gpg ~/.authinfo.gpg

# Fetch & index mails
offlineimap
~/.emacs.d/elisp/mu/mu/mu index -m ~/Mails/

rm -f ~/.config/rss2email.cfg
ln -s ~/Git\ repositories/dotfiles/tools/rss2email.cfg ~/.config/rss2email.cfg
ln ~/Git\ repositories/dotfiles/tools/r2e.service ~/.config/systemd/user/r2e.service
ln ~/Git\ repositories/dotfiles/tools/r2e.timer ~/.config/systemd/user/r2e.timer
ln ~/Git\ repositories/dotfiles/tools/offlineimap.service ~/.config/systemd/user/offlineimap.service
ln ~/Git\ repositories/dotfiles/tools/muindex.service ~/.config/systemd/user/muindex.service
ln ~/Git\ repositories/emacs/emacs.service ~/.config/systemd/user/emacs.service

systemctl --user enable emacs
systemctl --user start emacs

systemctl --user enable r2e.timer
systemctl --user start r2e.timer
