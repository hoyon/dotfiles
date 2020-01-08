#!/usr/bin/env bash
# Author: Brandon Mittman <brandonmittman@gmail.com>
# Dependencies: imagemagick, grim (optional)
set -o errexit -o noclobber -o nounset

hue=(-level "0%,100%,0.6")
effect=(-filter Gaussian -resize 20% -define "filter:sigma=1.5" -resize 500.5%)
# default system sans-serif font
font=$(convert -list font | awk "{ a[NR] = \$2 } /family: $(fc-match sans -f "%{family}\n")/ { print a[NR-1]; exit }")
desktop=""
swaylock_cmd=(swaylock)

# Need these to capture multiple monitors' screenshots
declare -a outputs
declare -a images

# parse the json swaymsg for outputs
poutputs=$(swaymsg -t get_outputs | grep name | sed 's/.*:.*\"\(.*\)\".*/\1/')
while read -r line; do
    outputs+=($line)
    images+=($(mktemp --suffix=.png))
done <<< "$poutputs"

options="Options:
    -h, --help       This help menu.

    -d, --desktop    Attempt to minimize all windows before locking.

    -g, --greyscale  Set background to greyscale instead of color.

    -p, --pixelate   Pixelate the background instead of blur, runs faster.

    -f <fontname>, --font <fontname>  Set a custom font.

    -t <text>, --text <text> Set a custom text prompt.

    -l, --listfonts  Display a list of possible fonts for use with -f/--font.
                     Note: this option will not lock the screen, it displays
                     the list and exits immediately."

# move pipefail down as for some reason "convert -list font" returns 1
set -o pipefail
trap 'rm -f "${images[*]}"' EXIT
temp="$(getopt -o :hdnpglt:f: -l desktop,help,listfonts,nofork,pixelate,greyscale,text:,font: --name "$0" -- "$@")"
eval set -- "$temp"

# l10n support
text="Type password to unlock"
case "${LANG:-}" in
    af_* ) text="Tipe wagwoord om te ontsluit" ;; # Afrikaans
    de_* ) text="Bitte Passwort eingeben" ;; # Deutsch
    da_* ) text="Indtast adgangskode" ;; # Danish
    en_* ) text="Type password to unlock" ;; # English
    es_* ) text="Ingrese su contraseña" ;; # Española
    fr_* ) text="Entrez votre mot de passe" ;; # Français
    he_* ) text="הליענה לטבל המסיס דלקה" ;; # Hebrew עברית (convert doesn't play bidi well)
    id_* ) text="Masukkan kata sandi Anda" ;; # Bahasa Indonesia
    it_* ) text="Inserisci la password" ;; # Italian
    ja_* ) text="パスワードを入力してください" ;; # Japanese
    lv_* ) text="Ievadi paroli" ;; # Latvian
    nb_* ) text="Skriv inn passord" ;; # Norwegian
    pl_* ) text="Podaj hasło" ;; # Polish
    pt_* ) text="Digite a senha para desbloquear" ;; # Português
    ru_* ) text="Введите пароль" ;; # Russian
    * ) text="Type password to unlock" ;; # Default to English
esac

while true ; do
    case "$1" in
        -h|--help)
            printf "Usage: %s [options]\n\n%s\n\n" "${0##*/}" "$options"; exit 1 ;;
        -d|--desktop) desktop=$(command -V wmctrl) ; shift ;;
        -g|--greyscale) hue=(-level "0%,100%,0.6" -set colorspace Gray -average) ; shift ;;
        -p|--pixelate) effect=(-scale 10% -scale 1000%) ; shift ;;
        -f|--font)
            case "$2" in
                "") shift 2 ;;
                *) font=$2 ; shift 2 ;;
            esac ;;
        -t|--text) text=$2 ; shift 2 ;;
        -l|--listfonts)
	    convert -list font | awk -F: '/Font: / { print $2 }' | sort -du | command -- ${PAGER:-less}
	    exit 0 ;;
	-n|--nofork) swaylock_cmd+=(--nofork) ; shift ;;
        --) shift; break ;;
        *) echo "error" ; exit 1 ;;
    esac
done

arraylen=${#outputs[@]}
for (( i=0; i<${arraylen}; i++ ));
do
    command -- "grim" -o "${outputs[$i]}" "${images[$i]}"

    value="60" #brightness value to compare to

    color=$(convert "${images[$i]}" -gravity center -crop 100x100+0+0 +repage -colorspace hsb \
    -resize 1x1 txt:- | awk -F '[%$]' 'NR==2{gsub(",",""); printf "%.0f\n", $(NF-1)}');

    if (( $i == 0 )); then
        if [[ $color -gt $value ]]; then #white background image and black text
            bw="black"
            icon="$HOME/.config/sway/lockdark.png"
        else #black
            bw="white"
            icon="$HOME/.config/sway/lock.png"
        fi
    fi

    param+=("-i" "${outputs[$i]}:${images[$i]}")

    convert "${images[$i]}" "${hue[@]}" "${effect[@]}" -font "$font" -pointsize 26 -fill "$bw" -gravity center \
        -annotate +0+160 "$text" "$icon" -gravity center -composite "${images[$i]}"
done

# If invoked with -d/--desktop, we'll attempt to minimize all windows (ie. show
# the desktop) before locking.
${desktop} ${desktop:+-k on}

# try to use swaylock with prepared parameters
if ! "${swaylock_cmd[@]}" "${param[@]}" >/dev/null 2>&1; then
    # We have failed, lets get back to stock one
    "${swaylock_cmd[@]}"
fi

# As above, if we were passed -d/--desktop, we'll attempt to restore all windows
# after unlocking.
${desktop} ${desktop:+-k off}
