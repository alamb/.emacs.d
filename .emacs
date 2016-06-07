;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Andrew Lamb's .emacs  file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/emacs-bash-completion")

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; Open .h files in C++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; recompile via f12
(global-set-key [f11] 'recompile)
(global-set-key [f12] 'compile)

;; Turn off the annoying tool bar w/ icons
(cond ((>= emacs-major-version 21)
       (tool-bar-mode nil)))
;;(toggle-uniquify-buffer-names)
(setq visible-bell t)
(setq font-lock-maximum-decoration t)

;; Stuff from the emacs "standards" by Bill Mann
(if (fboundp 'show-paren-mode)
    (show-paren-mode 1))			; 20.1 highlite the matching paren
(setq tags-revert-without-query t)	        ; reread changed TAGS tables

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; C coding style parameters
(c-add-style '"nutonian"
             '((c-basic-offset . 4)
               (c-comment-only-line-offset . 0)
               (c-offsets-alist . ((statement-block-intro . +)
                                   (knr-argdecl-intro . +)
                                   (substatement . *)
                                   (substatement-open . 0)
                                   (label . 0)
                                   (case-label . *)
                                   (statement-cont . *)
                                   (statement-case-intro . *)
                                   (func-decl-cont . 0)
                                   (inline-open . 0)
                                   ))))

(defun c-hooks ()
  (interactive)
  (c-set-style "nutonian")
  (c-set-offset 'innamespace nil)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq default-tab-width 4)
  (setq comment-start "// ")
  (setq comment-end "")
  (setq fill-column 79))

(add-hook 'c-mode-hook 'c-hooks)
(add-hook 'c++-mode-hook 'c-hooks)

;; TODO: make some reasonable key bindings

;; Ensure that  we don't get  asked about following symlinks into CVS
;; controlled sources (I don't care!)
(setq vc-follow-symlinks t)

;; Add extra highlighting for selected words
(defvar c-extra-keywords
  '(("[^A-Z_]\\(FIX\\(ME\\)?\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("[^A-Z_]\\(TRICKY\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("[^A-Z_]\\(LEAK\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("[^A-Z_]\\(TEMP\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("[^A-Z_]\\(HACK\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("[^A-Z_]\\(TODO\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("[^A-Z_]\\(XXXX*\\)[^A-Z_]" 1 font-lock-warning-face t)
    ("\\(\\+\\+\\+[A-Za-z]*\\)" 1 font-lock-warning-face t)))
(font-lock-add-keywords 'c-mode c-extra-keywords)
(font-lock-add-keywords 'c++-mode c-extra-keywords)
(font-lock-add-keywords 'java-mode c-extra-keywords)
(font-lock-add-keywords 'perl-mode c-extra-keywords)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GDB stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gud-hooks ()
  (interactive)
  (gud-def gud-nexti "nexti %p" nil)
  (local-set-key "\M-s" 'gud-step)
  (local-set-key "\M-n" 'gud-next)
  (local-set-key "\M-i" 'gud-stepi)
  (local-set-key "\M-j" 'gud-nexti)  ;; j for "jump"??
  (local-set-key "\M-c" 'gud-cont)
  (local-set-key "\M-u" 'gud-up)
  (local-set-key "\M-d" 'gud-down)
  (local-set-key "\M-f" 'gud-finish))

(add-hook 'gud-gdb-mode-hook 'gud-hooks)

;; Bind Ctrl+x Space to setting breakpoints
(global-set-key (kbd "C-x SPC") 'gud-break)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From Ryan Betts: FileCache/ido mode (is awesome)
;;
;; I like ido-mode .. which this random Martin fellow wrote some elisp
;; to bind file-cache mode to.  Without this, I think you have to do
;; file-cache-minibuffer-complete or something ..
;;
;; But with ido-mode and file-cache, I can find any file in the source
;; tree by typing any ordered sequence of letters from the file name.
;; This rocks with java's silly package layouts.
;;
;; To populate the cache from $SOURCE:
;;   M-x aal-load-filecache
;; To open a file,
;;   C-c C-f filename (this being the binding provided below)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ido)
(ido-mode t)
;;(setq ido-enable-flex-matching t)

;; file name cache
(require 'filecache)

;; nifty tricks from http://www.credmp.org/category/emacs/
;; Prevent subversion files,sql,dal form polluting the cache
(add-to-list 'file-cache-filter-regexps "\\.svn-base$")
(add-to-list 'file-cache-filter-regexps "\\.sql$")
(add-to-list 'file-cache-filter-regexps "\\.out$")
(add-to-list 'file-cache-filter-regexps "\\.dat$")

;; load filecache w/ SOURCE files
(defun aal-load-filecache ()
  (interactive)
   (file-cache-add-directory-recursively(getenv "SOURCE")))

;; magic lisp from MartinNordholts via emacswiki.org
(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in ile-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))

(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
      (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))

;;C-C C-f to find via cache
(global-set-key "\C-c\C-f" 'file-cache-ido-find-file)
(global-set-key "\C-cf" 'file-cache-ido-find-file)


(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; no stupid toolbar
(tool-bar-mode -1)

;; Bash auto completion mode
(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)


;; All C++ auto complete stuff
(load "~/.emacs.d/aal-cyborg.el")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-mode t)
 '(tool-bar-mode nil))

;;; Indulge coworker OCD and remove trailing whitespace on text file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; Indent xml/html by 4 spaces rather than default 2
(setq nxml-child-indent 4)


;; Colorize in compile mode
;; http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


;; Add NodeJS error format line number parsing
;; https://benhollis.net/blog/2015/12/20/nodejs-stack-traces-in-emacs-compilation-mode/
(setq compilation-error-regexp-alist-alist
      (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
                         1 ;; file
                         2 ;; line
                         3 ;; column
                         )
            compilation-error-regexp-alist-alist))
(setq compilation-error-regexp-alist
      (cons 'node compilation-error-regexp-alist))


;; Rust mode in emacs
(add-to-list 'load-path "~/.emacs.d/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
