;;;;; Config to make emacs auto complete my C++ code.
;;;;; I will show them you can be super efficient C++ coder too (not just Javascript!)
;;;;;


(defun tc-recompile()
  " recompile and don't show the compilation buffer"
  (interactive)
  (let ((orig-win (selected-window))
        (compilation-win (get-buffer-window "*compilation*" t)))
  (when compilation-win
      (select-window compilation-win)
      (bury-buffer)
      (select-window orig-win))
    (recompile)
  )
)

;; C-c C-% will set a buffer local hook to use mode-compile after saving
(global-set-key '[(ctrl c) (ctrl %)]
                (lambda ()
                  (interactive)
                  (if (member 'tc-recompile after-save-hook)
                      (progn
                        (setq after-save-hook
                            (remove 'tc-recompile after-save-hook))
                        (message "No longer compiling after saving."))
                    (progn
                      (add-to-list 'after-save-hook 'tc-recompile)
                      (message "Compiling after saving.")))))
