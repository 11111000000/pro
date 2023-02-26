(let ((keys-table '(("C--" "text-scale-decrease") ("C-+" "text-scale-increase") ("C-=" "text-scale-increase") ("C-M-=" "text-scale-set") ("C-c m" "popwin:messages") ("C-%" "forward-or-backward-sexp") ("C-a" "к-идентации-или-началу-строки") ("C-$" "toggle-truncate-lines") ("C-c f" "вставить-имя-файла")))
      (exwm-keys-table '(("s-q" "exwm-reset") ("s-\\" "toggle-input-method") ("s-e" "buffer-expose") ("s-t" "exwm-floating-toggle-floating") ("C-s-r" "rename-buffer") ("C-s-d" "delete-window") ("s-h" "windmove-left") ("s-j" "windmove-down") ("s-k" "windmove-up") ("s-l" "windmove-right") ("s-H" "buf-move-left") ("s-J" "buf-move-down") ("s-K" "buf-move-up") ("s-L" "buf-move-right") ("s-t" "tab-bar-new-tab") ("s-r" "tab-bar-rename-tab") ("s-n" "tab-bar-switch-to-next-tab") ("s-p" "tab-bar-switch-to-prev-tab") ("s-d" "delete-window") ("s-D" "tab-bar-close-tab") ("C-s-n" "tabbar-forward") ("C-s-p" "tabbar-backward") ("s-x" "app-launcher-run-app") ("s-M-h" "split-window-horizontally") ("s-M-k" "split-window-vertically") ("<XF86Back>" "winner-undo") ("<XF86Forward>" "winner-redo") ("<print>" "скриншот-области") ("s-s" "скриншот-области") ("s-<print>" "скриншот") ("s-S-s" "скриншот") ("s-<left>" "shrink-window-horizontally") ("s-<right>" "enlarge-window-horizontally") ("s-<down>" "shrink-window") ("s-<up>" "enlarge-window") ("s-<tab>" "consult-buffer") ("s-f" "ace-window") ("s-z" "avy-goto-char") ("s-_" "winner-undo") ("s-M-_" "winner-redo") ("s-u" "winner-undo") ("s-U" "winner-redo") ("s-<f3>" "battery") ("s-`" "vterm-toggle-cd") ("C-`" "scratch-pop") ("s-a" "buffer-expose") ("s-SPC" "buffer-expose") ("s-*" "buffer-expose-stars"))))
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
