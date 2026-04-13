#!/bin/bash

# Путь к файлу изображения передан через первый аргумент скрипта
image_path="$1"

# Проверка существования файла
if [[ ! -f "$image_path" ]]; then
    echo "Файл не существует: $image_path"
    exit 1
fi

# Предположим, что у Postimages есть конечная точка API, подобная этой
url="https://api.postimages.org/1/upload"

# Загрузка изображения на Postimages
response=$(curl --silent --request POST --url "$url" --form "file=@$image_path")

echo "$response"

# Предполагая, что Postimages возвращает URL в формате plain text или JSON, 
# ниже пример обработки ответа в формате JSON:
link=$(echo $response | jq -r '.data.url')

# Вывод ссылки
echo "Ссылка на загруженное изображение: $link" $link
