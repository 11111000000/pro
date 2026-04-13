#!/bin/bash
pactl list short sources | while read -r line; do
    source_index=$(echo "$line" | awk '{print $1}')
    source_name=$(echo "$line" | awk '{print $2}')
    if [[ "$source_name" != *"monitor"* ]]; then
        pactl set-source-mute "$source_index" 1
    fi
done

# Mute all ALSA capture controls on all sound cards
for card in $(aplay -l | grep '^card' | awk '{print $2}' | sed 's/://'); do
    amixer -c "$card" controls | grep -E 'name=.*Capture' | while read -r ctl; do
        control=$(echo "$ctl" | sed 's/.*name='\''\(.*\)'\''/\1/')
        amixer -c "$card" set "$control" nocap > /dev/null
    done
done

# echo "Отключение всех микрофонов в ALSA..."
# for card in $(aplay -l | grep '^card' | awk '{print $2}' | sed 's/://'); do
#     for device in $(amixer -c "$card" scontrols | awk -F '[][:]' '{print $1}'); do
#         amixer -c "$card" set "$device" mute
#         echo "Выключен микрофон: $device на карте: $card"
#     done
# done
# amixer set Capture 0%

# # Отключение микрофонов в PulseAudio
# echo "Отключение всех микрофонов в PulseAudio..."
# for source in $(pactl list sources short | awk '{print $2}'); do
#     pactl set-source-mute "$source" 1
#     echo "Выключен микрофон: $source"
# done

# # echo "Все микрофоны отключены."
