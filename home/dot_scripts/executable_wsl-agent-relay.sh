#!/usr/bin/env bash
set -euo pipefail

if (($# != 2)); then
	echo "usage: ${0##*/} SOCKET TARGET" >&2
	exit 64
fi

sock=$1
target=$2

case $sock in
gpgconf-agent-socket)
	sock=$(gpgconf --list-dirs agent-socket)
	;;
gpgconf-agent-ssh-socket)
	sock=$(gpgconf --list-dirs agent-ssh-socket)
	;;
esac

win_env() {
	/mnt/c/Windows/System32/cmd.exe /c "echo %$1%" 2>/dev/null | tr -d '\r'
}

npiperelay=${NPIPERELAY:-$(command -v npiperelay.exe || true)}
if [ -z "$npiperelay" ]; then
	lad="$(wslpath "$(win_env LOCALAPPDATA)")"
	for candidate in \
		"$lad/Microsoft/WinGet/Links/npiperelay.exe" \
		"/mnt/c/Program Files/WinGet/Links/npiperelay.exe" \
		"$lad"/Microsoft/WinGet/Packages/albertony.npiperelay_*/npiperelay.exe \
		"/mnt/c/Program Files/WinGet/Packages"/albertony.npiperelay_*/npiperelay.exe; do
		if [ -x "$candidate" ]; then
			npiperelay=$candidate
			break
		fi
	done
fi
if [ -z "$npiperelay" ]; then
	echo "npiperelay.exe not found; set NPIPERELAY to its path" >&2
	exit 1
fi

case $target in
//./pipe/*)
	flags="-ei -s"
	;;
*)
	if [[ $target != *:* ]]; then
		target="$(win_env LOCALAPPDATA)\\gnupg\\$target"
	fi
	flags="-ei -ep -s -a"
	;;
esac

gca=
program_files="$(wslpath "$(win_env ProgramFiles)")"
program_files_x86="$(wslpath "$(win_env 'ProgramFiles(x86)')")"
for c in "$program_files/GnuPG/bin/gpg-connect-agent.exe" \
	"$program_files_x86/GnuPG/bin/gpg-connect-agent.exe"; do
	if [ -x "$c" ]; then
		gca=$c
		break
	fi
done

agent_ready() {
	if [ -n "$gca" ] && ! "$gca" /bye >/dev/null 2>&1; then
		return 1
	fi
	case $target in
	//./pipe/*) return 0 ;;
	*) [ -e "$(wslpath "$target")" ] ;;
	esac
}

ready=
for _ in {1..15}; do
	if agent_ready; then
		ready=1
		break
	fi
	sleep 1
done
if [ -z "$ready" ]; then
	echo "Windows gpg-agent not reachable (target: $target)" >&2
	exit 1
fi

mkdir -p "$(dirname "$sock")"
rm -f "$sock"
# Preserve backslashes through socat's EXEC shell.
exec_target=${target//\\/\\\\\\\\}
exec socat "UNIX-LISTEN:$sock,fork,unlink-early,mode=0600" \
	"EXEC:'$npiperelay' $flags '$exec_target',nofork"
