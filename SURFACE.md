# SURFACE.md — публичные контракты ПРО

## FROZEN (стабильные контракты)

### Name: Healthcheck
Stability: [FROZEN]
Spec: Проверка работоспособности системы: инициализация без ошибок, загрузка ключевых модулей.
Proof: `emacs --batch -l tests/e2e/healthcheck.el`
Invariant: INV-Test-Coverage

---

### Name: Module.Load
Stability: [FROZEN]
Spec: Каждый *.el модуль загружается без ошибок через require/provide. Все 50+ модулей в директориях: инфраструктура/, интеграция/, интерфейс/, организация/, разработка/, среда/, язык/, навигация/, инструменты/, платформа/.
Proof: `emacs --batch -l tests/e2e/module-load.el`
Invariant: INV-Test-Coverage

---

### Name: Integration.Ai
Stability: [FROZEN]
Spec: ИИ-интеграция через GPTEL: chat buffer, множественные backends (OpenAI, Anthropic, Ollama, DeepSeek), подсчёт токенов и стоимости (рубли).
API: `pro-ai-gptel-start`, `gptel`, `gptel-send`
Proof: `emacs --batch -l tests/e2e/ai-integration.el`
Invariant: INV-Test-Coverage

---

### Name: Org.Roundtrip
Stability: [FROZEN]
Spec: Org-mode ↔ Emacs Lisp: конвертация org→el (org-element-parse-buffer) и обратно (org-element-interpret-data) без потери данных.
Proof: `emacs --batch -l tests/e2e/org-roundtrip.el`
Invariant: INV-Test-Coverage

---

## FLUID (изменяемые контракты)

### Name: Keybinding.Activation
Stability: [FLUID]
Spec: Переназначение клавиш: пользователь может изменить привязку в про-клавиши.org или через custom-set-keybindings.
API: `про-клавиши-set-key`, `про-клавиши-load`
Proof: -

---

### Name: Theme.Switching
Stability: [FLUID]
Spec: Переключение тем: динамическая смена визуальной темы без перезагрузки.
API: `про-тема-set-theme`, `load-theme`
Proof: -

---

### Name: Editor.Startup
Stability: [FLUID]
Spec: Запуск редактора: стартовый экран, буферы, окна настраиваются при старте.
API: `про-стартовый-экран-setup`
Proof: -

---

### Name: Package.Manager
Stability: [FLUID]
Spec: Управление пакетами: установка, обновление, удаление.
API: `pro/package-install`, `pro/package-update`
Proof: -

---

### Name: Terminal.Integration
Stability: [FLUID]
Spec: Интеграция терминалов: vterm, eshell, shell с настройками.
API: `про-терминалы-start`, `vterm`, `eshell`
Proof: -

---

### Name: Project.Navigation
Stability: [FLUID]
Spec: Навигация по проектам: dired, Projectile,(find-lisp).
API: `про-файлы-и-папки-open-project`
Proof: -

---

### Name: Code.Languages
Stability: [FLUID]
Spec: Поддержка языков: LISP, Python, JS, C, Rust, Haskell, Java, Flutter.
API: `про-код-на-{язык}-setup`
Proof: -

---

### Name: Testing.Cycle
Stability: [FLUID]
Spec: Цикл тестирования с уровнями: L1 (syntax), L2 (unit), L3 (e2e), L4 (integration).
Levels:
- L1: `emacs --batch -f batch-byte-compile` — проверка синтаксиса
- L2: `emacs --batch -l tests/unit/*.el` — изолированные юнит-тесты  
- L3: `emacs --batch -l tests/e2e/*.el` — e2e (требует devShell)
- L4: полная интеграция с network/GUI
API: `про-тест-run-level`, `про-тест-все-уровни`
Proof: `emacs --batch -f batch-byte-compile . --eval '(message "L1: OK")'`

---

## Test Infrastructure Requirements

### Levels

| Level | Type | What runs | Dependencies | Command |
|-------|------|-----------|---------------|---------|
| L1 | Syntax | byte-compile | none | `emacs --batch -f batch-byte-compile` |
| L2 | Unit | require/provide модулей | none (batch) | `emacs --batch -l tests/unit/*.el` |
| L3 | E2E | полная загрузка | devShell + network | `nix develop -c emacs --batch -l tests/e2e/*.el` |
| L4 | Integration | AI, org roundtrip | full Emacs + packages | interactive |

### Invariants

- INV-Test-Levels: Тесты разделены по уровням, каждый уровень запускается независимо
- INV-Test-Isolation: L1-L2 не требуют external packages, network, GUI
- INV-Test-Repeatability: Одинаковые входы ⇒ одинаковые результаты

---

## Test Infrastructure Requirements

## Payloads (публичные структуры)

### pro-конфиг (alist)
```elisp
(:модуль . "про-ии")
(:тема . "default")
(:языки . (lisp python javascript))
(:ии-провайдеры . (openai anthropic ollama))
```

### pro-org-структура (org-headings)
```org
* Заголовок
** Подзаголовок
Тело
#+begin_src emacs-lisp
код
#+end_src
```

### pro-ai-ответ (struct)
```elisp
(:модель . "gpt-4")
(:токены-вход . 1000)
(:токены-выход . 500)
(:стоимость-руб . 0.5))
```

## Operations (операции/команды)

| Операция | Успех | Ошибка |
|---------|-------|--------|
| `require 'про-ии` | feature loaded | file-missing |
| `про-ai-gptel-start` | буфер создан | network-error |
| `gptel` (M-x) | chat buffer opened | no-api-key |
| `org-element-parse-buffer` | tree returned | parse-error |
| `про-клавиши-set-key` | key bound | invalid-key |