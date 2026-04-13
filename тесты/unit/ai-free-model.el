;;; ai-free-model.el --- Regression test for OpenRouter filtering -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Checks that string-valued pricing data does not break free-model detection.

;;; Code:

(require 'ert)

(eval-and-compile
  (let* ((source (or load-file-name buffer-file-name default-directory))
         (root (if (file-directory-p source)
                   (file-name-as-directory source)
                 (file-name-directory source))))
    (add-to-list 'load-path (expand-file-name "../../интеграция/" root))))

(require 'про-ии-ядро)

(ert-deftest pro-ai-openrouter-free-model-p-coerces-string-pricing ()
  (should (pro-ai-gptel--openrouter-free-model-p
           '((id . "qwen/qwen3-coder:free")
             (pricing . ((prompt . "0") (completion . "0")))))))

(ert-deftest pro-ai-openrouter-free-model-p-rejects-paid-model ()
  (should-not (pro-ai-gptel--openrouter-free-model-p
               '((id . "qwen/qwen3-coder:free")
                 (pricing . ((prompt . "0.0001") (completion . 0)))))))

(ert-run-tests-batch-and-exit)

;;; ai-free-model.el ends here
