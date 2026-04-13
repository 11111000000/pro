#!/bin/bash

# Имя вашего Bluetooth устройства
DEVICE_NAME="soundcore Space One"  
echo $DEVICE_NAME

# Получаем адрес устройства
DEVICE_MAC=$(bluetoothctl devices | grep "$DEVICE_NAME" | awk '{print $2}')
echo $DEVICE_MAC
if [ -z "$DEVICE_MAC" ]; then
    echo "Устройство не найдено"
    exit 1
fi

# Заменяем ":" на "_" в адресе устройства для использования в PulseAudio
DEVICE_MAC=${DEVICE_MAC//:/_}
echo $DEVICE_MAC 

# Включаем HFP профиль 
echo "Включаем HFP профиль"
pactl set-card-profile bluez_card.$DEVICE_MAC handsfree_head_unit

# Устанавливаем sink для output
echo "Устанавливаем sink для output"
pactl set-default-sink bluez_sink.$DEVICE_MAC.handsfree_head_unit

# Устанавливаем sink для mic
echo "Устанавливаем sink для mic"
pactl set-default-source bluez_source.$DEVICE_MAC.handsfree_head_unit

# Включаем основной микрофон гарнитуры
# Если требуется, можно настроить громкость микрофона
echo "Включаем микрофон гарнитуры"
pactl set-source-mute bluez_source.$DEVICE_MAC.handsfree_head_unit 0  # 0 - включить
pactl set-source-volume bluez_source.$DEVICE_MAC.handsfree_head_unit 100%  # Устанавливаем 100% громкость микрофона

