;;; package --- Summary
;;; Commentary:
;;; Code:

;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold 100000000)
(setq large-file-warning-threshold 100000000) ;; size in bytes
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;;only maximize the first frame
;; (add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; maximize all frames

;;;;;;;;;;;;;;;;;;;;;;;;
;; config straight.el ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; check OS type, if is macOS, bind meta-key to command-key
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq mac-option-key-is-meta t)
    (setq mac-command-key-is-meta nil)
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))

;; run minor mode in background and not show in modeline
(use-package diminish)

;; Always as "y or n", not that annoying "yes or no".
(defalias 'yes-or-no-p 'y-or-n-p)

;; user info
(setq user-full-name "neit"
      user-mail-address "kamikaze129@gmail.com")

;; default directory
(setq default-directory "~/")

;; org directory
(setq org-directory "~/OneDrive/work/org")

;; add your modules path
;; (add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;; load your modules
;;(require 'setup-faces)
;;(require 'setup-others)
;;(require 'setup-editing)
;;(require 'setup-convenience)
;;(require 'setup-programming)

(defvar config-directory (expand-file-name "config" user-emacs-directory))

(setq config-packages '(faces others editing convenience programming))

(dolist (pack config-packages)
  (setq pack-directory (expand-file-name pack config-directory))
  (use-package '(pack :local-repo pack-directory)))

(provide 'init)
;;; init ends here
