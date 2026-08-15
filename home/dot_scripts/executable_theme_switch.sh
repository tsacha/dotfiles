#!/usr/bin/env bash

cm=(
	chezmoi
	-S "$HOME/Git/dotfiles"
	-c "$HOME/Git/dotfiles/chezmoi.toml"
	--persistent-state "$HOME/.config/chezmoi/chezmoistate.boltdb"
)
theme_targets=(
	"$HOME/.gitconfig"
	"$HOME/.config/gitu/config.toml"
	"$HOME/.config/fish/conf.d/theme.fish"
	"$HOME/.config/k9s/skins/theme.yaml"
	"$HOME/.config/tmux/tmux.conf"
	"$HOME/.config/tmux/theme.conf"
)

reload_tmux() {
	if command -v tmux >/dev/null 2>&1 && tmux list-sessions >/dev/null 2>&1; then
		tmux source-file "$HOME/.config/tmux/tmux.conf"
		tmux refresh-client -S
	fi
}

apply_theme() {
	"${cm[@]}" apply --force "${theme_targets[@]}" "$@"
	reload_tmux
}

if [[ -n ${WSL_DISTRO_NAME:-} || -n ${WSL_INTEROP:-} ]]; then
	personalize='HKCU\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize'
	if reg.exe query "$personalize" /v AppsUseLightTheme 2>/dev/null | grep -q '0x0'; then
		scheme=light
		value=1
	else
		scheme=dark
		value=0
	fi
	reg.exe add "$personalize" /v AppsUseLightTheme /t REG_DWORD /d "$value" /f >/dev/null
	reg.exe add "$personalize" /v SystemUsesLightTheme /t REG_DWORD /d "$value" /f >/dev/null
	powershell.exe -NoProfile -Command 'Add-Type -Namespace Win32 -Name UI -MemberDefinition "[DllImport(`"user32.dll`", CharSet=CharSet.Auto)] public static extern IntPtr SendMessageTimeout(IntPtr hWnd, uint Msg, UIntPtr wParam, string lParam, uint fuFlags, uint uTimeout, out UIntPtr lpdwResult);"; $r=[UIntPtr]::Zero; [void][Win32.UI]::SendMessageTimeout([IntPtr]0xFFFF, 0x001A, [UIntPtr]::Zero, "ImmersiveColorSet", 2, 1000, [ref]$r)' >/dev/null 2>&1
	rio_config="$(wslpath "$(cmd.exe /c 'echo %LOCALAPPDATA%' 2>/dev/null | tr -d '\r')")/rio/config.toml"
	[ -f "$rio_config" ] && sed -i "s/^theme = .*/theme = \"theme-$scheme\"/" "$rio_config"
	apply_theme
	exit 0
fi

if [ "$(uname)" != "Darwin" ]; then
	if [[ ${XDG_CURRENT_DESKTOP:-} == *"KDE"* ]]; then
		current_theme=$(kreadconfig6 --file kdeglobals --group KDE --key LookAndFeelPackage)
		if [ "$current_theme" == "org.kde.breezedark.desktop" ]; then
			plasma-apply-lookandfeel -a org.kde.breeze.desktop
		else
			plasma-apply-lookandfeel -a org.kde.breezedark.desktop
		fi
	else
		if [ "$(gsettings get org.gnome.desktop.interface color-scheme)" == "'prefer-light'" ]; then
			gsettings set org.gnome.desktop.interface color-scheme \'prefer-dark\'
			gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
			awww query && awww img $HOME/.wallpapers/dark.jpg
		elif [ "$(gsettings get org.gnome.desktop.interface color-scheme)" == "'prefer-dark'" ]; then
			gsettings set org.gnome.desktop.interface color-scheme \'prefer-light\'
			gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
			awww query && awww img $HOME/.wallpapers/light.png
		elif [ "$(gsettings get org.gnome.desktop.interface color-scheme)" == "'default'" ]; then
			gsettings set org.gnome.desktop.interface color-scheme \'prefer-light\'
			gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
			awww query && awww img $HOME/.wallpapers/light.png
		fi
		apply_theme \
			~/.config/sway/config \
			~/.config/sway/config.d/theme-colors \
			~/.config/niri/config.kdl \
			~/.config/niri/layout.kdl \
			~/.config/mako/config \
			~/.config/fuzzel/fuzzel.ini
		swaymsg reload
	fi
else
	osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
	apply_theme ~/.config/k9s/config.yaml
fi
