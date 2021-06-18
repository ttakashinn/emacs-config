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

;; run minor mode in background and not show in modeline
(use-package diminish)

;; add your modules path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; load your modules
(require 'setup-faces)
(require 'setup-others)
(require 'setup-editing)
(require 'setup-convenience)
(require 'setup-programming)
