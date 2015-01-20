# KDE and i3 association
Tested in Fedora 21/KDE

In /usr/share/kde4/apps/ksmserver/windowmanagers/i3.desktop :

```
[Desktop Entry]
Encoding=UTF-8 
Name=i3
Comment=Highly configurable framework window manager 
Type=Application
Exec=i3
TryExec=i3
```

Then in `systemsettings` :

```
System Settings > Default Applications > Window Manager > Use a different window manager: > i3
```

We have to hide Plasma and KRunner :

```
mkdir ~/.config/autostart/
cp /usr/share/autostart/plasma-desktop.desktop ~/.config/autostart/
cp /usr/share/autostart/krunner.desktop ~/.config/autostart/
```

In these both files : add `Hidden=true` at the end.

Then, we have to remove the Status Notifier Manager to have notifications icons in i3 bar. In `systemsettings` :

```
System Settings > Startup and Shutdown > Service Manager > Uncheck "Status Notifier Manager"
```
