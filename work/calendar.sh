#!/bin/bash
me=$(gcalcli list | grep owner | grep '@' | awk '{ print $3 }')
calendar=$(zenity --list --hide-header --column="Calendar" Moi Absences)
topic=$(zenity --list --hide-header --height=600 --column="Topic" \
               "ISS Back" \
               "ISS API" \
               "Payment" \
               "Architecture" \
               "Internal Tools" \
               "Jenkins" \
               "Ansible" \
               "Terraform" \
               "Réunion"
     )
when=$(zenity --entry --text "When?")
duration=$(zenity --entry --text "Duration")
if [ "$calendar" == "Moi" ]; then
    calendar=$me
    if [[ ! -z $calendar && ! -z $topic && ! -z $duration ]]; then
        msg=$(gcalcli --calendar "$calendar" add --title "$topic" --when "$when" --duration "$duration" --noprompt 2>&1)
        ret=$?
    fi
fi
if [ "$calendar" == "Absences" ]; then
    if [[ ! -z $calendar && ! -z $topic && ! -z $duration ]]; then
        msg=$(gcalcli --calendar "$calendar" add --title "$topic" --when "$when" --duration "$duration" --allday --noprompt 2>&1)
        ret=$?
    fi
fi
if [[ $ret -gt 0 ]]; then
    zenity --error --width=600 --text "${msg##*$'\n'}"
fi
