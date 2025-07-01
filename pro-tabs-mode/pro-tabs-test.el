;;; pro-tabs-test.el --- Tests for pro-tabs

(require 'ert)
(require 'pro-tabs)

(ert-deftest pro-tabs-enable-disable ()
  "Test enable/disable of `pro-tabs-mode' toggles tab-bar-mode."
  (pro-tabs-mode 1)
  (should (and pro-tabs-mode tab-bar-mode))
  (pro-tabs-mode 0)
  (should-not pro-tabs-mode)
  (should-not tab-bar-mode))

;; Можно добавить другие тесты следующим образом:
;; (ert-deftest pro-tabs-format-tab-bar-basic () ...)

(provide 'pro-tabs-test)
;;; pro-tabs-test.el ends here
