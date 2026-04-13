# План исправления gptel/Ollama и тестов

## Состояние

- Проблема: `gptel` вызывает `curl: exit 35` для backend `Ollama`
- **Причина найдена**: gptel автоматически добавляет `https://` к URL!
- URL в gptel: `https://localhost:11434/v1/chat/completions`
- А Ollama слушает на: `http://localhost:11434`
- Результат: SSL connection error (curl 35)

## Headless тест подтвердил

```
gptel backend URL: https://localhost:11434/v1/chat/completions
gptel-use-curl: t
gptel-backend-protocol exists: nil
```

## Другие проблемы в nix gptel

| Функция | Статус |
|---------|--------|
| `gptel-get-backend` | fboundp = nil |
| `gptel-curl-get-response` | void-function |

## Варианты решения

1. ❌ Попробовать `gptel-use-curl = nil` (использовать url-retrieve вместо curl)
2. ❌ Найти способ переопределить URL в backend
3. ❌ Патчить gptel для HTTP для localhost
4. ❌ Использовать ellama/llm-ollama вместо gptel для Ollama