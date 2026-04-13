# Бесплатные AI провайдеры и модели

> Апрель 2026

## 1. OpenRouter (openrouter.ai) — РЕКОМЕНДУЕТСЯ

**Статус**: Работает без VPN
**Бесплатные модели**: 27+ моделей
**Лимиты**: 20 req/min, 200 req/day

### Лучшие бесплатные модели (2026)

| Модель | Контекст | Назначение |
|--------|----------|------------|
| `qwen/qwen3-coder:free` | 262K | Coding (лучшая) |
| `qwen/qwen3-next-80b-a3b-instruct:free` | 262K | Coding/Reasoning |
| `nvidia/nemotron-3-super-120b-a12b:free` | 262K | General |
| `google/gemma-4-31b-it:free` | 262K | Vision |
| `google/gemma-4-26b-a4b-it:free` | 262K | Vision/ Tools |
| `minimax/minimax-m2.5:free` | 197K | General |
| `openai/gpt-oss-120b:free` | 131K | Coding |
| `openrouter/free` | 200K | Auto-select |

### Лимиты
- 20 requests/minute
- 200 requests/day

### API Endpoint
```
https://openrouter.ai/api/v1/chat/completions
```

### Как использовать
- Не требует API ключа (или можно получить бесплатный)
- Бесплатные модели имеют `:free` суффикс

---

## 2. Ollama (ollama.com)

**Статус**: Локальный, работает везде
**Модели**: Llama, Qwen, Mistral и др.

### Как использовать
```bash
# Установка
curl -fsSL https://ollama.com/install.sh | sh

# Запуск
ollama serve

# Модели
ollama pull qwen2.5-coder:latest
ollama pull llama3:latest
```

### API
```
http://localhost:11434/v1/chat/completions
```

---

## 3. Российские провайдеры

### AITunnel (aitunnel.ru)
- Требует регистрацию
- Есть бесплатные лимиты для новых пользователей
- Модели: Qwen, DeepSeek, GPT

### Прочие
- SberCloud (GigaChat)
- Yandex Cloud (YandexGPT)
- VK (VK AI)

---

## 4. Другие бесплатные источники

### DeepSeek
- API: `https://api.deepseek.com`
- Бесплатные лимиты для новых аккаунтов

### Claude (Anthropic)
- Ограниченные бесплатные лимиты для новых пользователей

---

## 5. Где искать актуальные списки

### OpenRouter
- Список моделей: https://openrouter.ai/models?free=true
- API каталог: https://openrouter.ai/api/v1/models

### Обновляемые источники
- costgoat.com/pricing/openrouter-free-models
- openrouter-free-model.vercel.app

---

## 6. Настройка в ПРО

### Текущая реализация (про-ии-ядро.el)

```elisp
;; OpenRouter - обновление списка бесплатных
(pro-ai-gptel-openrouter-free-models t)
(pro-ai-gptel-refresh-openrouter-backend)

;; AITunnel - предпочитаемые модели
;; pro-ai-gptel-aitunnel-preferred-models

;; Qwen - вся линейка
```

### Функции
- `pro-ai-gptel-openrouter-free-models` — получить aktualьный список free моделей
- `pro-ai-gptel-refresh-openrouter-backend` — интерактивно обновить backend
- `pro/ai-switch-backend` — переключить backend
- `pro/ai-switch-model` — переключить модель
