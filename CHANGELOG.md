# CHANGELOG

## [1.1.0] - 2026-04-17

### Added

- **Nix flake improvements**: Added new shell utilities for Emacs testing
  - `emacs-system-headless`: Run Emacs with system init.el in headless mode
  - `emacs-system-check`: Verify Emacs loads system config correctly
  - `emacs-tty-diagnose`: Diagnose TTY mode variable and hook states
  - `emacs-xvfb-smoke`: Full GUI/EXWM startup test with Xvfb
- **Org capture templates**: Added configurable capture templates (TODO, Journal, Note, Bookmarks, Link)
- **EXWM modularization**: Split графическую-среду into separate modules
  - про-графическую-среду-ядро.el: Core EXWM state and simulation keys
  - про-графическую-среду-ввод.el: XIM input methods and touchpad config
  - про-графическую-среду-мониторы.el: RandR monitor configuration
  - про-графическую-среду-окна.el: Window naming and hooks
  - про-графическую-среду-трей.el: System tray management
  - про-графическую-среду-старт.el: Single orchestration entry point
- **TTY support**: Added explicit keybindings for ESC sequences in org keybindings

### Fixed

- **GUI-only packages in TTY**: Added `:if (display-graphic-p)` guards
  - eldoc-box: Only load in graphical sessions
  - keyfreq: Only load in graphical sessions  
  - transient-posframe: Only load in graphical sessions
  - display-line-numbers: Only load in graphical sessions
- **corfu-terminal**: Now only activates in TTY mode, not GUI
- **установить-из.el**: Added error handling for package-vc-install failures
- **sample-init.el**: Updated to load про-графическую-среду-старт instead of про-графическую-среду
- **про-отладку.el**: Delayed file logging to after startup to avoid early init interference

### Changed

- **Startup behavior**: Removed automatic *Messages* buffer display on startup to avoid EXWM early initialization issues

## [1.0.0] - 2026-04-14

### Fixed

- **gptel-Ollama Integration**: Fixed curl exit 35 error when using gptel with local Ollama
  - Problem: gptel was forcing HTTPS protocol for Ollama backend
  - Root cause: Using `gptel-make-openai` instead of dedicated `gptel-make-ollama`
  - Solution: Migrated Ollama backend registration to use `gptel-make-ollama`
  - Result: Correct HTTP URL construction (`http://localhost:11434/api/chat`)
  - Added comprehensive headless tests to verify fix

### Added

- Headless Emacs tests for gptel-Ollama integration
- Comprehensive plan documentation for gptel-Ollama testing strategy

### Removed

- Obsolete org-mode documentation files from tests directory

### Notes

This release resolves a critical integration issue with Ollama in Emacs.
The fix ensures proper protocol handling and demonstrates our commitment to
stable, tested integrations with local LLM models.

Properly using the dedicated Ollama backend prevents protocol mismatch
due to the inappropriate use of OpenAI-compatible settings.