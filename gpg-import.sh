#!/bin/bash
read -r -p "GPG master key ready y/N? : " gpgrdy
if [ "$gpgrdy" == "y" ]; then
    killall -9 gpg-agent
    if [ -b "/dev/mapper/gpgkey" ]; then
	if [ ! -d /tmp/gpgkey ]; then
	    mkdir /tmp/gpgkey
	fi

	echo "GPG master key is already open."
	mountpoint /tmp/gpgkey > /dev/null
	if [ $? -gt 0 ]; then
	    mount /dev/mapper/gpgkey /tmp/gpgkey
	fi
    else
	if [ ! -d /tmp/gpgkey ]; then
	    mkdir /tmp/gpgkey
	fi
	read -r -p "GPG master key device : " gpgkey
	echo
	read -r -s -p "GPG master key device password : " gpgkeypasswd
	echo
	echo -n $gpgkeypasswd | cryptsetup luksOpen $gpgkey gpgkey -
	mount /dev/mapper/gpgkey /tmp/gpgkey
    fi
    if [ ! -d /tmp/gpgworkdir ]; then
	mkdir /tmp/gpgworkdir
	chmod 700 /tmp/gpgworkdir
    fi
    gpg --homedir /tmp/gpgworkdir --pinentry-mode loopback --import /tmp/gpgkey/vault/vault-pub
    gpg --homedir /tmp/gpgworkdir --pinentry-mode loopback --import /tmp/gpgkey/vault/vault-sec
    if [ ! -d /tmp/gpgprivate ]; then
	mkdir /tmp/gpgprivate
	chmod 700 /tmp/gpgprivate
    fi
    pushd /tmp/gpgprivate
    gpg --homedir /tmp/gpgworkdir --pinentry-mode loopback --decrypt /tmp/gpgkey/private.tar.gpg | tar xvf -
    popd
    chown sacha.users -R /tmp/gpgprivate
    su - sacha -c 'gpg --import /tmp/gpgprivate/private-keys/*.priv'
    su - sacha -c 'gpg --import /tmp/gpgkey/pubkey'
    echo "Vault import…"
    su - sacha -c 'gpg --import /tmp/gpgkey/vault/vault-pub'
    su - sacha -c 'gpg --import /tmp/gpgkey/vault/vault-sec'
    find /tmp/gpgprivate -type f -exec shred {} \;
    rm -Rf /tmp/gpgprivate
    find /tmp/gpgworkdir -type f -exec shred {} \;
    rm -Rf /tmp/gpgworkdir
    umount /tmp/gpgkey
    cryptsetup luksClose gpgkey
    rm -Rf /tmp/gpgkey
    killall -9 gpg-agent
    su - sacha -c "systemctl --user start gpg-agent-ssh.socket"
    su - sacha -c "gpg -K --with-keygrip 'Sacha Trémoureux (Néandre)' | grep '\[A\]' -b1 | tail -n1 | awk '{ print \$NF }' > ~/.gnupg/sshcontrol"
    signingkey=`su - sacha -c "gpg -K --with-keygrip 'Sacha Trémoureux (Néandre)' | head -n2 | tail -n1 | awk '{ print \$NF }'"`
    cat <<EOF > /home/sacha/.gitconfig
[user]
  signingkey = $signingkey
  name = Sacha Trémoureux
  email = sacha@tremoureux.fr
EOF
    chown sacha.users /home/sacha/.gitconfig
 fi
