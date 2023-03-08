(let ((keys-table '(("C-a" "к-идентации-или-началу-строки") ("C--" "text-scale-decrease") ("C-+" "text-scale-increase") ("C-=" "text-scale-increase") ("C-M-=" "text-scale-set") ("C-c m" "popwin:messages") ("C-%" "forward-or-backward-sexp") ("C-a" "к-идентации-или-началу-строки") ("C-$" "toggle-truncate-lines") ("C-c f" "вставить-имя-файла") ("C-x b" "consult-buffer") ("C-x C-b" "consult-buffer-other-window") ("s-b" "consult-project-buffer") ("s-<tab>" "consult-buffer") ("M-s s" "consult-line") ("M-s M-s" "consult-line-multi") ("C-x y" "consult-yank-from-kill-ring") ("<help> a" "consult-apropos") ("s-m" "consult-imenu-multi")))
      (exwm-keys-table '(("s-q" "exwm-reset") ("s-\\" "toggle-input-method") ("s-e" "buffer-expose") ("s-T" "exwm-floating-toggle-floating") ("C-s-d" "delete-window") ("s-h" "windmove-left") ("s-j" "windmove-down") ("s-k" "windmove-up") ("s-l" "windmove-right") ("s-H" "buf-move-left") ("s-J" "buf-move-down") ("s-K" "buf-move-up") ("s-L" "buf-move-right") ("s-R" "rename-buffer") ("s-d" "delete-window") ("s-t" "tab-bar-new-tab") ("s-T" "tab-bar-undo-close-tab") ("s-r" "tab-bar-rename-tab") ("s-N" "tab-bar-move-tab") ("s-P" "tab-bar-move-tab-backward") ("s-n" "tab-bar-switch-to-next-tab") ("s-p" "tab-bar-switch-to-prev-tab") ("s-w" "tab-bar-close-tab") ("s-W" "закрыть-вкладку-и-буфер") ("s-x" "app-launcher-run-app") ("s-M-h" "split-window-horizontally") ("s-M-k" "split-window-vertically") ("<XF86Back>" "winner-undo") ("<XF86Forward>" "winner-redo") ("<print>" "скриншот-области") ("s-s" "скриншот-области") ("s-<print>" "скриншот") ("s-S-s" "скриншот") ("s-<left>" "shrink-window-horizontally") ("s-<right>" "enlarge-window-horizontally") ("s-<down>" "shrink-window") ("s-<up>" "enlarge-window") ("s-<tab>" "consult-buffer") ("s-f" "ace-window") ("s-z" "avy-goto-char") ("s-_" "winner-undo") ("s-M-_" "winner-redo") ("s-u" "winner-undo") ("s-U" "winner-redo") ("C-<f3>" "battery") ("s-`" "vterm-toggle-cd") ("s-~" "eshell-toggle") ("C-c s" "scratch-pop") ("s-a" "buffer-expose") ("s-SPC" "buffer-expose") ("s-*" "buffer-expose-stars") ("C-c l" "org-store-link") ("C-c a" "org-agenda"))))
(-map
 (lambda (row)
   (cl-destructuring-bind (соч фун) row
     (global-set-key (kbd соч) (intern фун))))
 keys-table)

(-map
 (lambda (row)
   (cl-destructuring-bind (соч фун) row
     (global-set-key (kbd соч) (intern фун))))
 exwm-keys-table)


(if (and window-system (functionp 'exwm-input-set-key))
    (progn
	    (require 'exwm)
	    (-map
	     (lambda (row)
	       (cl-destructuring-bind (соч фун) row
	         (exwm-input-set-key (kbd соч) (intern фун))))
	     exwm-keys-table)))

)
