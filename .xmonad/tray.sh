#!/bin/zsh
 
typeset -A DISKS

###
# Config
###
DATE_FORMAT="%a %d %b, %Y"
TIME_ZONES=("Pacific/Honolulu" "America/Los_Angeles" "America/New_York")
DISKS=(root / home /home)
SEPARATOR='^p(8)^r(3x3)^p(8)'
BAR_BG='#7DA926'
BAR_FG='#B9D56E'
BAR_HH=6
BAR_HW=40
BAR_VH=12
BAR_VW=3
BAR_ARGS="-bg $BAR_BG -fg $BAR_FG -w $BAR_HW -h $BAR_HH"
ICON_DIR="$HOME/.xmonad/dzen_bitmaps"
 
GLOBALIVAL=15
DATEIVAL=240
TIMEIVAL=4
DISKIVAL=4
CPUIVAL=1
CPUTEMPIVAL=1
 
 
###
# Functions
###
_date()
{
    date +${DATE_FORMAT}
}
 
_time()
{
    local zone
    print_sep=0
    for zone in $TIME_ZONES; do
        [[ $print_sep -eq 1 ]] && print -n ${SEPARATOR}
        print -n "${zone:t}: $(TZ=$zone date '+%H:%M')"
        print_sep=1
    done
}
 
#
# Format: label1 mountpoint1 label2 mountpoint2 ... labelN mountpointN
# Copied and modified from Rob
get_disk_usage() {
    local rstr; local tstr; local i; local sep
    for i in ${(k)DISKS}; do
        tstr=$(print `df -h $DISKS[$i]|sed -ne 's/^.* \([0-9]*\)% .*/\1/p'` 100 | \
            gdbar -h $BAR_HH -w $BAR_HW -fg $BAR_FG -bg $BAR_BG -l "${i}" -nonl | \
            sed 's/[0-9]\+%//g;s/  / /g')
        if [ ! -z "$rstr" ]; then
            sep=${SEPARATOR}
        fi
        rstr="${rstr}${sep}${tstr}"
    done
    print -n $rstr
}
 
cpu_temp()
{
    print -n ${(@)$(</proc/acpi/thermal_zone/THRM/temperature)[2,3]}
}

cpu()
{
    gcpubar -c 2 -bg $BAR_BG -fg $BAR_FG -w $BAR_HW -h $BAR_HH | tail -n1 | tr -d '\n'
}

###
# Main loop
###
 
DATEI=0
TIMEI=0
DISKI=0
CPUTEMPI=0
CPUI=0
 
date=$(_date)
times=$(_time)
disk_usage=$(get_disk_usage)
cpumeter=$(cpu)
#temp=$(cpu_temp)
 
while true; do
    [[ $DATEI -ge $DATEIVAL ]] && date=$(_date) && DATEI=0
    [[ $TIMEI -ge $TIMEIVAL ]] && times=$(_time) && TIMEI=0
    [[ $DISKI -ge $DISKIVAL ]] && disk_usage=$(get_disk_usage) && DISKI=0
    [[ $CPUI -ge $CPUIVAL ]] && cpumeter=$(cpu) && CPUI=0
    #[[ $CPUTEMPI -ge $CPUTEMPIVAL ]] && temp=$(cpu_temp) && CPUTEMPI=0

    # CPU usage
    echo -n "${cpumeter}${SEPARATOR}"

    # Disk usage
    echo -n "${disk_usage}${SEPARATOR}"

    # Time and date
    echo -n "${times}${SEPARATOR}"
    echo -n "${date}"
    echo
 
    DATEI=$(($DATEI+1))
    TIMEI=$(($TIMEI+1))
    DISKI=$(($DISKI+1))
    CPUI=$(($CPUI+1))
    #CPUTEMPI=$(($CPUTEMPI+1))
 
    sleep $GLOBALIVAL
done