#!/bin/bash
#
# ~/bin/dwm-statusbar
#
# Status bar for dwm. Expanded from:
# https://bitbucket.org/jasonwryan/eeepc/src/73dadb289dead8ef17ef29a9315ba8f1706927cb/Scripts/dwm-status

# Colour codes from dwm/config.h
GREY="\x01"
WHITE="\x02"
RED="\x03"
GREEN="\x04"
YELLOW="\x05"
BLUE="\x06"
CYAN="\x07"
MAGENTA="\x08"
ORANGE="\x09"

# Icon glyphs from ohsnap icon font
glyph_dl="Ú"
glyph_ul="Û"
glyph_vol_mute="ë"
glyph_vol_quiet="ì"
glyph_vol_loud="í"
glyph_tim="É"
glyph_batt_empty="ñ"
glyph_batt_partial="ò"
glyph_batt_full="ó"
glyph_no_internets="×"
glyph_ethernet="¡"
glyph_wireless_poor="¢"
glyph_wireless_good="£"
glyph_wireless_excellent="¤"

SEP="${GREY}\x19"

# Functions for printing taskbar information
print_internets() {
  if [[ $(ifconfig eth0 | grep -w inet) ]]; then  # Check for an Ethernet connection
    echo -ne "${GREEN}${glyph_ethernet} ${SEP} "
  elif [[ ! $(iwconfig wlan0 | grep 'Not-Associated') ]]; then  # Check for a Wireless connection
    ESSID=$(iwconfig wlan0 | grep ESSID | cut -d\" -f2)
    strength=$(iwconfig wlan0 | grep 'Link Quality' | awk '{ print $2}' | cut -d= -f2 | cut -d/ -f1)
    signal="${SEP} ${WHITE}${ESSID}: "

    # Determine signal strength
    if (( $strength < 40 )); then
      signal+="${GREEN}${glyph_wireless_poor} "
    elif (( $strength < 60 )); then
      signal+="${GREEN}${glyph_wireless_good} "
    else
      signal+="${GREEN}${glyph_wireless_excellent} "
    fi

    echo -ne "${signal} ${SEP} "
  else
    echo -ne "${RED}${glyph_no_internets} ${SEP} "
  fi
}

print_battery() {
  # Get the percentage of battery remaining
  perc+=$(acpi -b | cut -d' ' -f4 | sed 's/[%,]//g')
  time_remaining=$(acpi -b | cut -d' ' -f5)  # e.g. 02:34:02
  hour=$(echo ${time_remaining} | cut -d' ' -f5 | cut -d: -f1)
  hour=${hour:1} # Strip off trailing 0
  minute=$(echo ${time_remaining} | cut -d' ' -f5 | cut -d: -f2)

  if (( $perc < 15 )) ; then
    batt="${RED}${glyph_batt_empty}"
  elif (( $perc < 75 )) ; then
    batt="${YELLOW}${glyph_batt_partial}"
  else
    batt="${GREEN}${glyph_batt_full}"
  fi

  echo -e "${batt} ${WHITE}${perc}% ${hour}h ${minute}m ${SEP} "
}

print_volume() {
  volume="$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')"
  state=$(amixer get Master | tail -n 1 | cut -d' ' -f8 | sed -r 's/\[(.*)\]/\1/')

  if (( $volume == 0 )) || [[ $state == "off" ]] ; then
    vol="${RED}${glyph_vol_mute}"
  elif (( $volume < 50 )) ; then
    vol="${ORANGE}${glyph_vol_quiet}"
  else
    vol="${ORANGE}${glyph_vol_loud}"
  fi

  echo -ne "${vol} ${WHITE}${volume}% ${SEP} "
}

print_datetime() {
  datetime="$(date "+%a %d %b %H:%M")"
  echo -ne "${SEP} ${CYAN}${glyph_tim} ${WHITE}${datetime}"
}

while true
do
  # Get new rx/tx counts
  if [[ $(ifconfig eth0 | grep -w inet) ]]; then  # Check for an Ethernet connection
    rx_now=$(cat /sys/class/net/eth0/statistics/rx_bytes)
    tx_now=$(cat /sys/class/net/eth0/statistics/tx_bytes)
  elif [[ ! $(iwconfig wlan0 | grep 'Not-Associated') ]]; then  # Check for a Wireless connection
    rx_now=$(cat /sys/class/net/wlan0/statistics/rx_bytes)
    tx_now=$(cat /sys/class/net/wlan0/statistics/tx_bytes)
  fi

  # Calculate the rate (K)
  let rx_rate=($rx_now-$rx_old)/1024
  let tx_rate=($tx_now-$tx_old)/1024

  print_rx_rate() {
    # Prints download rate
    printf "%-11b" "${BLUE}${glyph_dl} ${WHITE}${rx_rate}K"
  }
  print_tx_rate() {
    # Prints upload rate
    printf "%-14b" "${SEP} ${MAGENTA}${glyph_ul} ${WHITE}${tx_rate}K"
  }

  # Pipe to status bar, not indented due to printing extra spaces/tabs
  xsetroot -name "$(print_internets)\
$(print_battery)\
$(print_volume)\
$(print_rx_rate)\
$(print_tx_rate)\
$(print_datetime)"

  # Reset old values
  rx_old=$rx_now
  tx_old=$tx_now

  sleep 1
done
