;;; искусственный-интеллект.el --- Искусственный Интеллект -*- lexical-binding: t -*-

;; Author: az
;; Maintainer: az
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Конфигурация нейросетевых сервисов

;;; Code:
;;;; LLAMA

(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "Russian")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama" :embedding-model "codellama")))

;;;; Codeium

;; (use-package codeium
;;   :init
;;   (установить-из :repo "Exafunction/codeium.el")
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   :config

;;   (setq use-dialog-box nil)

;;   (setq codeium/metadata/api_key "212600da-b787-4d45-91f0-5e9e98b94302")

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;          (lambda (api)
;;            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;   )

;; (use-package codeium-diagnose)
;;   :init
;;   (установить-из :repo "Exafunction/codeium-diagnose.el")
;;   :config
;;   (setq use-dialog-box nil)

(provide 'искусственный-интеллект)

;;; искусственный-интеллект.el ends here
