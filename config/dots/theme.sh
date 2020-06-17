# this file simply has a register of some themes.

case "$CURRENT_THEME" in
  dracula)
    COL0='#282A36'
    COL1='#FF5555'
    COL2='#50FA7B'
    COL3='#F1FA8C'
    COL4='#BD93F9'
    COL5='#FF79C6'
    COL6='#8BE9FD'
    COL7='#BFBFBF'
    COL8='#4D4D4D'
    COL9='#FF6E67'
    COL10='#5AF78E'
    COL11='#F4F99D'
    COL12='#CAA9FA'
    COL13='#FF92D0'
    COL14='#9AEDFE'
    COL15='#F8F8F2'
    ;;

  gruvbox_dark)
    COL0='#282828'
    COL1='#CC241D'
    COL2='#98971A'
    COL3='#D79921'
    COL4='#458588'
    COL5='#B16286'
    COL6='#689D6A'
    COL7='#A89984'
    COL8='#928374'
    COL9='#FB4934'
    COL10='#B8BB26'
    COL11='#FABD2F'
    COL12='#83A598'
    COL13='#D3869B'
    COL14='#8EC07C'
    COL15='#EBDBB2'
    ;;

  gruvbox_dark2)
    COL0='#282828'
    COL1='#9D0006'
    COL2='#79740E'
    COL3='#B57614'
    COL4='#076678'
    COL5='#8F3F71'
    COL6='#427B58'
    COL7='#FBF1C7'
    COL8='#32302F'
    COL9='#FB4934'
    COL10='#B8BB26'
    COL11='#FABD2F'
    COL12='#83A598'
    COL13='#D3869B'
    COL14='#8EC07C'
    COL15='#FBF1C7'
    ;;

  gruvbox_light)
    COL0='#FBF1C7'
    COL1='#CC241D'
    COL2='#98971A'
    COL3='#D79921'
    COL4='#458588'
    COL5='#B16286'
    COL6='#689D6A'
    COL7='#7C6F64'
    COL8='#928374'
    COL9='#9D0006'
    COL10='#79740E'
    COL11='#B57614'
    COL12='#076678'
    COL13='#8F3F71'
    COL14='#427B58'
    COL15='#3C3836'
    ;;

  tomorrow_night)
    COL0='#1D1F21'
    COL1='#CC6666'
    COL2='#B5BD68'
    COL3='#F0C674'
    COL4='#81A2BE'
    COL5='#B294BB'
    COL6='#8ABEB7'
    COL7='#969896'
    COL8='#C5C8C6'
    COL9='#FF0000'
    COL10='#282A2E'
    COL11='#FF0000'
    COL12='#282A2E'
    COL13='#22AAAA'
    COL14='#22AAAA'
    COL15='#C5C8C6'
    ;;
esac

if [ "$EXPORT_THEME" ]; then
  export COL0 COL1 COL2 COL3 COL4 COL5 COL6 COL7 COL8 COL9 COL10 COL11 COL12 COL13 COL14 COL15
fi
