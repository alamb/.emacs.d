;;;;; Config to make emacs auto complete my C++ code. 
;;;;; I will show them you can be super efficient C++ coder too (not just Javascript!)
;;;;;

(semantic-mode)

(require 'semantic/ia)
(require 'semantic/bovine/gcc)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(global-ede-mode t)

(ede-cpp-root-project "Nutonian"
                :name "Nutonian Source Code"
                :file "~/Nutonian/source-code/all.pro"
                :include-path '("/"
                                "/third-party/install/mkspecs/linux-g++"
                                "/ncloud"
                                "/nu"
                                "/third-party/install/include"
                                "/tparty/cpp-netlib"
                                "/tparty/zlib"
                                "/tparty/eureqa-api"
                                "/tparty"
                                "/Libs"
                                )
                :system-include-path '("~/exp/include")
                :spp-table '(("isUnix" . "")
                             ("BOOST_TEST_DYN_LINK" . "")
                             ("BOOST_THREAD_USE_LIB " . "")
                             ("BOOST_NETWORK_ENABLE_HTTPS" . "")
                             ("BOOST_IOSTREAMS_NO_LIB" . "")
                             ("_DEBUG" . "")
                             )
)

;; Note that I didn't have great luck with the auto-complete package

;;;;;;;;;;;;;;;;; Key bindings to get behavior similar to QT-Creator ;;;;;;;;;;;;;


;; f1 to find documentation
(global-set-key [f1] 'semantic-ia-show-doc)
;; f2 to find symbol's definition
(global-set-key [f2] 'semantic-ia-fast-jump)
;; f4 to open correpsonding header
(global-set-key [f4] 'ff-find-other-file)
;; alt-left goes back to previous cursor position
(global-set-key (kbd "M-<left>") 'pop-global-mark)



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


 
