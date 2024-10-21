# echo "Отключение всех микрофонов в ALSA..."
# for card in $(aplay -l | grep '^card' | awk '{print $2}' | sed 's/://'); do
#     for device in $(amixer -c "$card" scontrols | awk -F '[][:]' '{print $1}'); do
#         amixer -c "$card" set "$device" mute
#         echo "Выключен микрофон: $device на карте: $card"
#     done
# done
amixer set Capture 0%

# Отключение микрофонов в PulseAudio
echo "Отключение всех микрофонов в PulseAudio..."
for source in $(pactl list sources short | awk '{print $2}'); do
    pactl set-source-mute "$source" 1
    echo "Выключен микрофон: $source"
done

# echo "Все микрофоны отключены."
