#!/bin/bash
#
# ~/bin/dwm-statusbar
#
# Status bar for dwm. Expanded from:
# https://bitbucket.org/jasonwryan/eeepc/src/73dadb289dead8ef17ef29a9315ba8f1706927cb/Scripts/dwm-status

# Colour codes from dwm/config.h
colour_gry="\x01" # grey on black
colour_wht="\x02" # white on black
colour_dgry="\x04" # darkgrey on black
colour_blk="\x05" # black on darkgrey
colour_red="\x06" # colour_red on black
colour_grn="\x07" # green on black
colour_dylw="\x08" # orange on black
colour_ylw="\x09" # yellow on black
colour_blu="\x0A" # colour_blue on darkgrey
colour_mag="\x0B" # colour_magenta on darkgrey
colour_cyn="\x0C" # cyan on darkgrey

# Icon glyphs from font xbmicons.pcf
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
sep_solid="\x19"
sep_line="\x19"


print_internets() {
  if [[ $(ifconfig eth0 | grep -w inet) ]]; then  # Check for an Ethernet connection
    echo -ne "${glyph_ethernet} ${sep_solid} "
  elif [[ ! $(iwconfig wlan0 | grep 'Not-Associated') ]]; then  # Check for a Wirelss connection
    ESSID=$(iwconfig wlan0 | grep ESSID | cut -d\" -f2)
    strength=$(iwconfig wlan0 | grep 'Link Quality' | awk '{ print $2}' | cut -d= -f2 | cut -d/ -f1)
    signal="${ESSID}: "

    if (( $strength < 40 )); then
      signal+="${glyph_wireless_poor} "
    elif (( $strength < 60 )); then
      signal+="${glyph_wireless_good} "
    else
      signal+="${glyph_wireless_excellent} "
    fi

    echo -ne "${signal} ${sep_solid} "
  else
    echo -ne "${glyph_no_internets} ${sep_solid} "
  fi
}

print_volume() {
  volume="$(amixer get Master | tail -n1 | sed -r 's/.*\[(.*)%\].*/\1/')"
  vol=${colour_wht}

  if (( $volume == 0 )) ; then
    vol+=${glyph_vol_mute}
  elif (( $volume < 50 )) ; then
    vol+=${glyph_vol_quiet}
  else
    vol+=${glyph_vol_loud}
  fi

  echo -ne "${vol} ${volume}% "
}

print_datetime() {
  datetime="$(date "+%a %d %b %H:%M")"
  echo -ne "${colour_wht}${sep_solid} ${glyph_tim} ${datetime}"
}

print_battery() {
  # Get the percentage of battery remaining
  perc+=$(acpi -b | cut -d' ' -f4 | sed 's/[%,]//g')
  batt="${colour_wht}"

  if (( $perc < 15 )) ; then
    batt+=${glyph_batt_empty}
  elif (( $perc < 75 )) ; then
    batt+=${glyph_batt_partial}
  else
    batt+=${glyph_batt_full}
  fi

  echo -e "${batt} ${perc}% ${sep_solid} "
}

while true
do
  # Get new rx/tx counts
  rx_now=$(cat /sys/class/net/eth0/statistics/rx_bytes)
  tx_now=$(cat /sys/class/net/eth0/statistics/tx_bytes)
  # Calculate the rate (K)
  let rx_rate=($rx_now-$rx_old)/1024
  let tx_rate=($tx_now-$tx_old)/1024

  print_rx_rate() {
    # Prints download rate
    printf "%-11b" "${colour_blk}${sep_solid}${colous_grn} ${glyph_dl} ${rx_rate}K"
  }
  print_tx_rate() {
    # Prints upload rate
    printf "%-12b" "${colour_gry}${sep_line}${colour_red} ${glyph_ul} ${tx_rate}K"
  }

  # Pipe to status bar, not indented due to printing extra spaces/tabs
  xsetroot -name "$(print_internets)\
$(print_battery)\
$(print_volume)\
$(print_rx_rate)$(print_tx_rate)\
$(print_datetime)"

  # Reset old values
  rx_old=$rx_now
  tx_old=$tx_now

  sleep 1
done
