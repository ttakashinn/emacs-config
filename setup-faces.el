;;; package --- Summary
;;; Commentary:
;;; Code:

;; you won't need any of the bar thingies
;; turn it off to save screen estate
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; show buffer size in modeline
(size-indication-mode t)

;; show number of column
(column-number-mode 1)

(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(setq inhibit-splash-screen t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; Scrolling with the mouse.
(setq mouse-wheel-scroll-amount '(1))    ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't)       ;; scroll window under mouse

;;;;;;;;;;;;;;;;;
;; DOOM THEMES ;;
;;;;;;;;;;;;;;;;;
(use-package doom-themes :defer t)
(load-theme 'doom-zenburn t)

;; icons packages
(use-package all-the-icons)

;; auto-resize window to easily work with the current one
(use-package golden-ratio
  :diminish golden-ratio-mode
  :config
  (add-to-list 'golden-ratio-exclude-modes "ediff-mode")
  (add-to-list 'golden-ratio-exclude-modes "helm-mode")
  (add-to-list 'golden-ratio-exclude-modes "dired-mode")
  (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
  (golden-ratio-mode 1))

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

;; do not enable golden-raio in these modes
(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "gud-mode"
                                   "gdb-locals-mode"
                                   "gdb-registers-mode"
                                   "gdb-breakpoints-mode"
                                   "gdb-threads-mode"
                                   "gdb-frames-mode"
                                   "gdb-inferior-io-mode"
                                   "gud-mode"
                                   "gdb-inferior-io-mode"
                                   "gdb-disassembly-mode"
                                   "gdb-memory-mode"
                                   "magit-log-mode"
                                   "magit-reflog-mode"
                                   "magit-status-mode"
                                   "IELM"
                                   "eshell-mode"
                                   "dired-mode"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: DASHBOARD ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package page-break-lines)
(use-package dashboard
  :config
  (setq dashboard-projects-backend 'projectile)
  ;; Set the title
  ;; (setq dashboard-banner-logo-title "")
  ;; Set the banner
  (setq dashboard-startup-banner (expand-file-name "config/ttakashinn.txt" user-emacs-directory))
  ;; Value can be
  ;; 'official which displays the official emacs logo
  ;; 'logo which displays an alternative emacs logo
  ;; 1, 2 or 3 which displays one of the text banners
  ;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content nil)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)
                          (bookmarks . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-set-navigator nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info "??? ??? ???")

  ;;(setq dashboard-set-footer t)
  ;;(setq dashboard-footer-messages '("Dashboard is pretty cool!"))
  (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))
  (dashboard-setup-startup-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: doom-modeline ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-modeline
  :diminish doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-unicode-fallback t))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: treemacs   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs-all-the-icons)

(use-package treemacs
  :after projectile
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-load-theme "all-the-icons")
    (treemacs-resize-icons 12)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :after (treemacs lsp-mode)
  :config
  (lsp-treemacs-sync-mode 1))

(provide 'setup-faces)
;;; setup-faces ends here
