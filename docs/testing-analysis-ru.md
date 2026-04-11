# Диалектический анализ тестирования ПРО

## 1. Тезис: Что имеем

Тесты существуют как файлы в `tests/e2e/`:
- `healthcheck.el` — Surface: Healthcheck, FROZEN
- `module-load.el` — Surface: Module.Load, FROZEN  
- `ai-integration.el` — Surface: Integration.Ai, FROZEN
- `org-roundtrip.el` — Surface: Org.Roundtrip, FROZEN
- `scenario-workflow.el` — вертикальный сценарий

## 2. Антитезис: Что наблюдаем

При запуске получаем системную ошибку:
```
emacs: /run/current-system/sw/share/nix-ld/lib/libmount.so.1: 
version `MOUNT_2_40' not found (required by ...glib-2.86.3/lib/libgio-2.0.so.0)
```

Это не ошибка теста — это ошибка окружения.

## 3. Диалектический синтез: Две проблемы

### Проблема A: Окружение (Nix-конфликт)
- `emacs` из PATH ссылается на glib версии, несовместимой с системной
- Nix-окружение (flake.nix) использует `emacs-nox`, но обёртка не работает с `--batch` флагами
- Решение: создать dedicated devShell с совместимой версией Emacs

### Проблема Б: Тесты не самодостаточны
- Тесты используют `(require 'про-ии)` и `(require 'про-организацию)`
- Эти модули требуют свою инфраструктуру (use-package, external dependencies)
- При batch-запуске нет GUI, часть пакетов может не загрузиться
- Тесты должны быть изолированы: либо мокировать зависимости, либо загружать только то, что можно загрузить без сети/GUI

## 4. Change Gate (текущее изменение)

```
Intent: Проанализировать тестирование и выработать качественный цикл
Pressure: Debt
Surface impact: touches: SURFACE.md (add: Testing.Cycle)
Proof: tests: Этот документ +работающие e2e тесты
Migration: (required — создание working test infrastructure)
```

## 5. Предлагаемый цикл тестирования

### 5.1 Уровни тестирования

| Level | Type | What | Run |
|-------|------|------|-----|
| 1 | Syntax | byte-compile всех .el | `emacs --batch --eval ... -f batch-byte-compile` |
| 2 | Unit | require/provide без зависимостей | `emacs --batch -l test.el` |
| 3 | E2E | Полная загрузка модулей |Requires network + devShell |
| 4 | Integration | Org↔el roundtrip, AI мок | Только в правильном окружении |

### 5.2 Verify Commands (обновлённые)

```bash
# Level 1: Syntax check (все файлы)
emacs --batch --eval "(setq byte-compile-warnings '(obsolete))" \
  -f batch-byte-compile ./*.el ./*/*.el

# Level 2: Unit load (без внешних зависимостей)
emacs --batch -l tests/e2e/healthcheck.el

# Level 3: Full e2e (требует nix devShell)
nix develop -c emacs --batch -l tests/e2e/healthcheck.el
```

### 5.3 Infrastructure requirements

Для Level 3+ необходим Nix-девшелл с:
- Emacs 29.1+ (emacs-nox для headless)
- network access (первый запуск пакетов)
- Возможность установки пакетов через `straight` или `use-package`

### 5.4 Test isolation principle

Тесты Level 2 не должны требовать:
- external packages (org, gptel)
- network
- GUI/display
- init.el загрузку

Если нужен полный тест — это Level 3, запускать через `nix develop`.

## 6. Action Items

- [ ] Создать `tests/unit/syntax.el` — syntax-only проверка
- [ ] Обновить flake.nix: добавить devShell с emacs и network
- [ ] Исправить e2e тесты для изолированного запуска (или пометить требования)
- [ ] Добавить в AGENTS.md: Verify Commands с учётом уровней

## 7. Инвариант нового цикла

```
INV-Test-Levels: Тесты разделены по уровням, каждый уровень 
имеет явные зависимости и может запускаться независимо:
- L1: синтаксис (любой Emacs)
- L2: unit load (изолированный batch)
- L3: e2e (требует devShell + network)
- L4: integration (полная среда)
```

Этот инвариант должен быть зафиксирован в SURFACE.md.