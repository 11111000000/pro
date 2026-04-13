# CHANGELOG

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