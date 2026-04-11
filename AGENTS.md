# AGENTS.md — настройка HDS-агента для ПРО

## HDS-Configuration

### Role
HDS-агент (Builder + Verifier) для ПРО — персональной системы организации и автоматизации (50+ модулей на Emacs Lisp).

### Workflow
1. **Surface First**: Перед любым изменением внешнего смысла — обновить SURFACE.md
2. **Proof Before Code**: e2e тесты для [FROZEN] добавляются до реализации
3. **One Change — One Intent**: Один PR = одна цель
4. **Verify**: Проверки выполняются инструментами проекта
5. **Update**: Обновить HOLO.md при изменении решений

### Verify Commands
```bash
# E2E тесты — батч запуск без GUI
emacs --batch -l tests/e2e/healthcheck.el
emacs --batch -l tests/e2e/module-load.el
emacs --batch -l tests/e2e/org-roundtrip.el

# Проверка синтаксиса ELISP (все файлы)
emacs --batch --eval "(setq byte-compile-warnings '(obsolete))" \
  -f batch-byte-compile ./*.el ./*/*.el

# Проверка конкретного модуля
emacs --batch -l про-ии.el --eval "(provide 'про-ии)"
```

### Change Gate Format (обязательные поля)
```yaml
Intent: <одна фраза>
Pressure: Bug | Feature | Debt | Ops
Surface impact: (none) | touches: <Surface item(s)> [FROZEN/FLUID]
Proof: tests: <пути/команды>
Migration: (required, если touches [FROZEN])
```

### Frozen Rule (жёсткое правило)
- [FROZEN] элементы: Healthcheck, Module.Load, Integration.Ai, Org.Roundtrip
- Каждый [FROZEN] требует:
  - Pressure (Bug|Feature|Debt|Ops)
  - Proof (e2e тест)
  - Migration Block (если меняется API)
- Запрещено удалять/ослаблять Proof для [FROZEN]

### Test Tags
- `e2e` — интеграционные тесты в tests/e2e/
- `unit` —(unit тесты, если есть)
- Тесты используют `ert` (Emacs Lisp Regression Testing)

### Surface Items (FROZEN)
| Surface Item | Spec | Proof |
|--------------|------|-------|
| Healthcheck | initialization без ошибок | tests/e2e/healthcheck.el |
| Module.Load | require/provide все 50+ модулей | tests/e2e/module-load.el |
| Integration.Ai | chat buffer + providers | tests/e2e/ai-integration.el |
| Org.Roundtrip | org↔el конвертация | tests/e2e/org-roundtrip.el |

### Module Structure (по директориям)
```
про/
├── инфраструктура/    # менеджер пакетов, оптимизация
├── интеграция/       # ИИ, терминалы, интернет-сервисы
├── интерфейс/        # окна, буферы, темы
├── навигация/        # файлы, режим-бога
├── организация/      # org, время, проекты
├── платформа/        # огрызок
├── разработка/       # код на N языках
├── среда/            # вид, шрифты, цвет
├── инструменты/       # малая механизация
├── языки/            # guix, nix
├── tests/e2e/        # e2e proof тесты
├── HOLO.md           # голограмма
├── SURFACE.md        # публичные контракты
└── AGENTS.md         # настройки агента
```

### Dependencies
- Emacs 29.1+
- use-package
- gptel (для ИИ)
- org (для заметок)
- network (первый запуск)

### Profile
Профиль A (структурированный): Questions / Plan / Patch / Verify / Commands

### Failure Loop
1. Классифицируй: Surface drift | Test gap | Code defect
2. Чини в порядке: Surface → Tests → Code
3. Не расширяй Intent
4. Повтори Verify

### External Forms
- Emacs Lisp: require/provide, hooks, advise
- Org-mode: #+begin_src, agenda, capture
- CLI: emacs --batch
- Network: REST API (GPTEL backends)