# План TTY-clean UI

## Цель

Сделать TTY-режим Emacs полностью текстовым: без иконок, без ligatures, без Unicode-замен и без GUI-специфичных украшений.

## Change Gate

- Intent: упростить и стабилизировать UI в TTY.
- Pressure: низкая видимость поломки, но постоянный шум в консоли.
- Surface impact: `про-текстовый-режим.el`, `про-внешний-вид.el`, языковые модули, Org-настройки.
- Proof: ручная проверка в TTY и batch-валидация загрузки модулей.

## Шаги

1. Ввести TTY-guard, который отключает icon-packages и posframe-UI.
2. Добавить явные выключатели для `prettify-symbols`, `org-pretty-entities`, `org-ellipsis` и symbol composition.
3. Проверить `tab-bar`, `mode-line` и `completion`-UI на ASCII-only поведение в TTY.
4. Убедиться, что GUI-ветка остаётся без изменений.
5. Проверить запуск в TTY и собрать список оставшихся проблем.

## Acceptance

- В TTY не появляются символы-заглушки и “битые” Unicode-значки.
- Все иконки и декоративные элементы отключены.
- GUI продолжает использовать прежний внешний вид.

## Verify

- `emacs --batch -l tests/e2e/healthcheck.el`
- Ручная проверка TTY с открытием Org, Lisp и completion-буферов.
