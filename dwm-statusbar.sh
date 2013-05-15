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
glyph_dl="\uE011"
glyph_ul="\uE012"
glyph_vol_mute="ë"
glyph_vol_quiet="ì"
glyph_vol_loud="í"
glyph_tim="\uE016"
glyph_tor="\uE017"
sep_solid="\uE01A"
sep_line="\uE01B"

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

  echo -ne "${vol} ${volume}%"
}

print_datetime() {
  datetime="$(date "+%a %d %b ${sep_line} %H:%M")"
  echo -ne "${colour_blk}${sep_solid}${colour_wht} ${glyph_tim} ${datetime}"
}

battery_percentage_remaining() {
  state=$(acpi -a | cut -d' ' -f3)

  case $state in
    on-line)
      #acpi -b
      percentage=$(acpi -b | cut -d' ' -f4 | sed 's/[%,]//g')
      echo $percentage
      ;;
    off-line)
      #acpi -b
      percentage=$(acpi -b | cut -d' ' -f4 | sed 's/[%,]//g')
      echo $percentage
      ;;
    *)
      echo "Don't know what happened!"
      ;;
  esac
}

#perc=$(battery_percentage_remaining)
#
#if [[ $perc -lt 15 ]]; then
#  echo "Percentage is less than 5!"
#elif [[ $perc -lt 75 ]]; then
#  echo "Percentage is less than 75!"
#else
#  echo "Percentage is greater than 75!"
#fi

while true
do
  # Pipe to status bar, not indented due to printing extra spaces/tabs
  xsetroot -name "$(print_volume)"
#$(print_datetime)"

done
