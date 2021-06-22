;;; package --- Summary
;;; Commentary:
;;; Code:

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (save-place-mode 1))

;; display el-doc
(use-package eldoc
  :diminish eldoc-mode
  :hook ((emacs-lisp-mode . eldoc-mode)
         (lisp-interaction-mode . eldoc-mode)
         (ielm-mode . eldoc-mode)))

;; save regexp search queries every 3 minutes
(use-package savehist
  :init
  (setq savehist-additional-variables '(search ring regexp-search-ring)
        savehist-autosave-interval 180)
  :config
  (savehist-mode 1))

;; auto-saves your buffers, when certain events happen -
;; e.g. you switch between buffers, an Emacs frame loses focus, etc.
(use-package super-save
  :diminish super-save-mode
  :init
  (setq super-save-auto-save-when-idle t
        auto-save-default nil)
  :config
  (super-save-mode 1))

;; macos: get system $PATH correctly in GUI startup
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil
          exec-path-from-shell-variables '("PATH")
          exec-path-from-shell-arguments '("-l"))
    :config
    (exec-path-from-shell-initialize)))

;; A quick major mode help with discover-my-major
(use-package discover-my-major
  :config
  (global-unset-key (kbd "C-h h"))        ; original "C-h h" displays "hello world" in different languages
  (define-key 'help-command (kbd "h m") 'discover-my-major))


;; display key complete candidates in minibuffer
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode t)
  (which-key-setup-minibuffer))

;; calculate package loading time
(use-package esup)

;; backup files
(defvar backup-directory (expand-file-name "backups/" user-emacs-directory))
(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))
(setq
 make-backup-files t        ; backup a file the first time it is saved
 backup-directory-alist `((".*" . ,backup-directory)) ; save backup files
 backup-by-copying t     ; copy the current file into backup directory
 version-control t   ; version numbers for backup files
 delete-old-versions t   ; delete unnecessary versions
 kept-old-versions 6     ; oldest versions to keep when a new numbered backup is made (default: 2)
 kept-new-versions 9 ; newest versions to keep when a new numbered backup is made (default: 2)
 auto-save-default t ; auto-save every buffer that visits a file
 auto-save-timeout 20 ; number of seconds idle time before auto-save (default: 30)
 auto-save-interval 200 ; number of keystrokes between auto-saves (default: 300)
 )

;; highlight and click-able  addresses like URL or email-address
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)

(provide 'setup-others)
;;; setup-others ends here
