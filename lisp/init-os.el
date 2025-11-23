;; 在mac系统上交换command和option
(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta))

(provide 'init-os)
;;; init-os.el ends here
