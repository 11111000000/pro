(let ((keys-table '(("C-d" "nil") ("C-w" "nil") ("s-x" "nil") ("M-c" "nil") ("M-t" "nil") ("M-\\" "nil") ("M-f" "viper-forward-word") ("M-b" "viper-backward-word") ("M-j" "scroll-down") ("M-k" "scroll-up") ("M-y" "whole-line-or-region-kill-ring-save") ("C-y" "yank") ("C-d C-d" "whole-line-or-region-kill-region") ("C-M-d" "whole-line-or-region-kill-region") ("M-r" "replace-string") ("M-R" "replace-regexp") ("C-a" "к-идентации-или-началу-строки") ("C-S-r" "revert-buffer-quick") ("C-r" "revert-buffer") ("C-S-M-p" "shift-text-up") ("C-S-M-n" "shift-text-down") ("C-S-M-f" "shift-text-right") ("C-S-M-b" "shift-text-left") ("C--" "text-scale-decrease") ("C-+" "text-scale-increase") ("C-=" "text-scale-increase") ("C-M-=" "text-scale-set") ("C-c m" "popwin:messages") ("C-%" "forward-or-backward-sexp") ("C-a" "к-идентации-или-началу-строки") ("C-$" "toggle-truncate-lines") ("C-c f" "вставить-имя-файла") ("C-x b" "consult-buffer") ("C-x C-b" "consult-buffer") ("C-x M-b" "consult-buffer-other-window") ("C-c b" "consult-buffer-other-window") ("C-c C-b" "consult-buffer-other-window") ("s-<tab>" "tab-bar-switch-to-next-tab") ("C-x C-t" "выбрать-контакт-в-телеге") ("S-s-<iso-lefttab>" "tab-bar-switch-to-prev-tab") ("M-s s" "consult-line") ("M-s M-s" "consult-line-multi") ("M-s d" "consult-dash") ("C-x y" "consult-yank-from-kill-ring") ("<help> a" "consult-apropos") ("s-m" "consult-imenu-multi") ("C-j" "next-line") ("C-k" "previous-line") ("M-o" "viper-open-line") ("C-M-o" "viper-Open-line") ("M-O" "viper-Open-line") ("C-M-O" "viper-Open-line") ("C-d C-f" "delete-forward-char") ("C-d C-b" "backward-delete-char-untabify") ("C-d C-e" "удалить-до-конца-строки") ("C-d C-w" "delete-trailing-whitespace") ("C-d C-a" "delete-to-begin") ("C-M-s" "isearch-backward") ("C-c o p" "completion-at-point") ("C-c o t" "complete-tag") ("C-c o d" "cape-dabbrev") ("C-c o h" "cape-history") ("C-c o f" "cape-file") ("C-c o k" "cape-keyword") ("C-c o s" "cape-symbol") ("C-c o a" "cape-abbrev") ("C-c o i" "cape-ispell") ("C-c o l" "cape-line") ("C-c o w" "cape-dict") ("C-c o \\\\" "cape-tex") ("C-c o _" "cape-tex") ("C-c o ^" "cape-tex") ("C-c o &" "cape-sgml") ("C-c o r" "cape-rfc1345") ("C-c g" "magit-status") ("C-c p y" "prodigy") ("C-c d" "docker") ("C-c y y" "consult-yasnippet") ("C-c y n" "создать-новый-сниппет-со-шпаргалкой") ("C-c hl" "rainbow-identifiers-mode") ("C-c hi" "color-identifiers-mode") ("M-g g" "avy-goto-char") ("M-SPC" "er/expand-region") ("M-S-SPC" "er/contract-region") ("S-SPC" "mark-current-line") ("C-c SPC" "mc/mark-all-like-this") ("C-c C-SPC" "mc/unmark-all-like-this") ("C-c <mouse-1>" "mc/add-cursor-on-click") ("<backtab>" "hs-toggle-hiding") ("C-c <f2>" "eglot-reconnect") ("C-c v" "gt-do-translate") ("s-." "xref-find-definitions-other-window") ("C-M-." "xref-find-definitions-other-window") ("C-." "embark-act") ("C-c ir" "gptel-rewrite") ("C-c im" "gptel-menu") ("C-c in" "gptel-context-next") ("C-c i." "gptel-quick") ("C-c ia" "gptel-add") ("C-c if" "gptel-add-file") ("C-c ip" "gptel-system-prompt") ("C-c i RET" "gptel-send") ("C-c is" "gptel-send") ("C-c ik" "gptel-abort") ("C-c ig" "gptel-abort") ("C-c iq" "elysium-query") ("C-c iw" "whisper-run") ("C-c ic" "переключить-codeium") ("C-c e e" "evedel-create-directive") ("C-c e i" "evedel-modify-directive") ("C-c e D" "evedel-modify-directive-tag-query") ("C-c e P" "evedel-preview-directive-prompt") ("C-c e RET" "evedel-process-directives") ("C-c e TAB" "evedel-convert-instructions") ("C-c e r" "evedel-create-reference") ("C-c e k" "evedel-delete-instructions") ("C-c e C-'" "evedel-modify-reference-commentary") ("C-c e n" "evedel-next-instruction") ("C-c e p" "evedel-previous-instruction") ("C-c e s" "evedel-save-instructions") ("C-c e l" "evedel-load-instructions") ("C-c e t" "evedel-add-tags") ("C-c e T" "evedel-remove-tags") ("s-i" "nil") ("s-i s-i" "gptel") ("s-i i" "chatgpt-shell") ("s-i r" "gptel-rewrite") ("s-i m" "gptel-menu") ("s-i n" "gptel-context-next") ("s-i ." "gptel-quick") ("s-i a" "gptel-add") ("s-i f" "gptel-add-file") ("s-i p" "gptel-system-prompt") ("s-i RET" "gptel-send") ("s-i s" "gptel-send") ("s-i k" "gptel-abort") ("s-i g" "gptel-abort") ("s-i q" "elysium-query") ("s-i w" "whisper-run") ("s-i c" "переключить-codeium") ("s-e" "nil") ("s-e s-e" "evedel-create-directive") ("s-e e" "evedel-modify-directive") ("s-e i" "evedel-modify-directive") ("s-e D" "evedel-modify-directive-tag-query") ("s-e P" "evedel-preview-directive-prompt") ("s-e RET" "evedel-process-directives") ("s-e TAB" "evedel-convert-instructions") ("s-e r" "evedel-create-reference") ("s-e k" "evedel-delete-instructions") ("s-e C-'" "evedel-modify-reference-commentary") ("s-e n" "evedel-next-instruction") ("s-e p" "evedel-previous-instruction") ("s-e s" "evedel-save-instructions") ("s-e l" "evedel-load-instructions") ("s-e t" "evedel-add-tags") ("s-e T" "evedel-remove-tags") ("C-z" "nil")))
      (exwm-keys-table '(("s-q" "exwm-reset") ("s-\\" "toggle-input-method") ("C-\\" "toggle-input-method") ("s-SPC" "toggle-input-method") ("<XF86AudioMicMute>" "переключить-микрофон-alsa") ("<XF86AudioMute>" "выключить-звук") ("<XF86AudioRaiseVolume>" "увеличить-громкость") ("<XF86AudioLowerVolume>" "уменьшить-громкость") ("<XF86MonBrightnessUp>" "увеличить-яркость") ("<XF86MonBrightnessDown>" "уменьшить-яркость") ("<XF86TouchpadToggle>" "переключить-тачпад") ("C-c <f4>" "выключить-все-микрофоны") ("C-c M-<f4>" "включить-гарнитуру") ("s-b" "consult-buffer") ("C-s-d" "delete-window") ("s-h" "windmove-left") ("s-j" "windmove-down") ("s-k" "windmove-up") ("s-l" "windmove-right") ("s-H" "buf-move-left") ("s-J" "buf-move-down") ("s-K" "buf-move-up") ("s-L" "buf-move-right") ("s-R" "rename-buffer") ("s-t" "открыть-новую-вкладку") ("s-T" "tab-bar-undo-close-tab") ("s-r" "tab-bar-rename-tab") ("s-N" "tab-bar-move-tab") ("s-P" "tab-bar-move-tab-backward") ("s-n" "tab-bar-switch-to-next-tab") ("s-p" "tab-bar-switch-to-prev-tab") ("s-w" "tab-bar-close-tab") ("s-W" "закрыть-вкладку-и-буфер") ("s-d" "treemacs") ("s-x" "app-launcher-run-app") ("s-M-h" "split-window-horizontally") ("s-M-k" "split-window-vertically") ("C-x d" "consult-find") ("C-x C-d" "dired-jump") ("C-x m" "bookmark-jump") ("C-x C-m" "bookmark-set") ("<XF86Back>" "winner-undo") ("<XF86Forward>" "winner-redo") ("<print>" "скриншот-области") ("s-s" "consult-ag") ("s-<print>" "скриншот") ("s-S-s" "скриншот") ("s-<left>" "shrink-window-horizontally") ("s-<right>" "enlarge-window-horizontally") ("s-<down>" "shrink-window") ("s-<up>" "enlarge-window") ("s-z" "avy-goto-char") ("s-_" "winner-undo") ("s-M-_" "winner-redo") ("s-u" "tab-bar-history-back") ("s-U" "tab-bar-history-forward") ("C-<f3>" "battery") ("s-`" "eshell-toggle") ("s-~" "multi-vterm-dedicated-toggle") ("C-c tt" "eshell-here") ("C-c s" "scratch-pop") ("C-c l" "org-store-link") ("C-c a" "org-agenda") ("s-+" "golden-ratio") ("s-=" "balance-windows") ("s-_" "maximize-window") ("s--" "minimize-window") ("C-c pa" "projectile-add-known-project") ("C-c p C-p" "projectile-add-known-project") ("C-c pp" "projectile-switch-project") ("C-c C-p" "projectile-switch-project") ("C-c ps s" "consult-ag") ("C-x C-1" "delete-other-windows") ("C-x C-2" "split-window-below") ("C-x C-3" "split-window-right") ("C-x C-0" "delete-window") ("s-h" "windmove-left") ("s-j" "windmove-down") ("s-k" "windmove-up") ("s-l" "windmove-right") ("s-K" "buf-move-up") ("s-J" "buf-move-down") ("s-H" "buf-move-left") ("s-L" "buf-move-right") ("s-g" "delete-window") ("C-x +" "golden-ratio") ("C-x =" "balance-windows") ("C-x _" "maximize-window") ("C-x -" "minimize-window") ("s-f" "projectile-find-file-other-window") ("s-F" "ace-swap-window") ("C-<f6>" "profiler-start") ("C-<f7>" "profiler-stop") ("<f6>" "exwm-floating-toggle-floating") ("s-:" "chatgpt-shell-prompt") ("C-c i M-i" "chatgpt-shell") ("C-c ie" "chatgpt-shell") ("C-c il" "ellama-chat") ("C-c ii" "gptel") ("s-i s-i" "gptel") ("s-i i" "chatgpt-shell") ("s-a" "treemacs") ("C-c iv" "chatgpt-shell-send-and-review-region") ("C-c C-'" "exwm-edit--compose-minibuffer") ("C-c '" "exwm-edit--compose"))))
(mapcar
  (lambda (row)
   (cl-destructuring-bind (соч фун) row
     (global-set-key (kbd соч) (intern фун))))
 keys-table)

(mapcar
 (lambda (row)
   (cl-destructuring-bind (соч фун) row
     (global-set-key (kbd соч) (intern фун))))
 exwm-keys-table)

(if (and window-system (functionp 'exwm-input-set-key))
    (progn
	    (require 'exwm)
	    (mapcar
	     (lambda (row)
	       (cl-destructuring-bind (соч фун) row
	         (exwm-input-set-key (kbd соч) (intern фун))))
	     exwm-keys-table)))
)
