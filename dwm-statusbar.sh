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
glyph_tim="É"
glyph_batt_empty="ñ"
glyph_batt_partial="ò"
glyph_batt_full="ó"
glyph_tor="\uE017"
sep_solid="\x19"
sep_line="\x19"

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
  echo -ne "${sep_solid} ${glyph_tim} ${datetime}"
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
  # Pipe to status bar, not indented due to printing extra spaces/tabs
  xsetroot -name "$(print_battery)\
$(print_volume)\
$(print_datetime)"

done
