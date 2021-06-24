;;; package --- Summary
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: yasnippet* ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  ;; :init
  ;; if put snippets at the custom directories, need to define yas-snippet-dirs explicitly
  ;; (add-to-list 'load-path (expand-file-name "snippets" user-emacs-directory))
  ;; (setq yas-snippet-dirs '((expand-file-name "snippets" user-emacs-directory)) ;; personal snippets
  ;; )
  :commands (yas-reload-all)
  :hook ((prog-mode . yas-minor-mode)
         (snippet-mode . yas-minor-mode))
  :config
  (yas-reload-all))
(use-package yasnippet-snippets
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: company              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :after yasnippet
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.5)
  (setq company-transformers '(company-sort-prefer-same-case-prefix))
  :config
  (global-company-mode)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
;; (setq company-backends (append company-backends '(company-yasnippet)))

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setf (alist-get 'min-height company-box-frame-parameters) 6)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
        company-box-backends-colors nil

        ;; These are the Doom Emacs defaults
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))

  ;; Add a space after the icon
  (dolist (elt company-box-icons-all-the-icons)
    (setcdr elt (concat (cdr elt) " "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; config helm* packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :demand t
  :diminish helm-mode
  :init
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-apropos-fuzzy-match t
        helm-recentf-fuzzy-match t)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-recentf)
         ("C-h a" . helm-apropos)
         :map helm-map
         ("TAB" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :bind-keymap ("C-c h" . helm-command-prefix)
  :config
  (setq helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
        helm-split-window-in-side-p t ;;open helm buffer inside current window, not occupy whole other window
        helm-ff-filename-history-use-recentf t
        helm-ff-search-library-in-sexp t
        helm-move-to-line-cycle-in-source t) ;;move to end or beginning of source when reaching top or bottom of source.
  (helm-mode 1)
  (helm-autoresize-mode 1)
  (global-unset-key (kbd "C-x c")))

;; search package
(use-package helm-ag
  :init
  (setq helm-ag-base-command "rg --no-heading" ;; change to use rg
        helm-ag-success-exit-status '(0 2))
  :bind (("C-c g" . helm-ag)
         ("C-c d" . helm-do-ag)
         ("C-c r" . helm-do-ag-project-root)))

;; TODO: config search engine to rg

;; display icon of items in helm windows
(use-package helm-icons
  :config
  (helm-icons-enable))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES: projectile             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :after helm
  :diminish projectile-mode
  :init
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'helm)
  (projectile-mode 1))

(use-package helm-projectile
  :after (helm projectile)
  :init (setq helm-projectile-fuzzy-match t)
  :config
  (helm-projectile-on)
  (setq projectile-switch-project-action 'helm-projectile)

  ;; https://2kvn.com/helm-ripgrep-va-emacs-dung-helm-ag-p5f31323237
  ;; custom helm-projectile-ag to work with rg
  (defun helm-projectile-ag (&optional options) "Helm version of projectile-ag."
         (interactive
          (if current-prefix-arg
              (list (read-string "option: " "" 'helm-ag--extra-options-history))))
         (if (require 'helm-ag nil 'noerror)
             (if (projectile-project-p)
                 (let ((helm-ag-command-option options)
                       (current-prefix-arg nil))
                   (helm-do-ag (projectile-project-root)
                               (car (projectile-parse-dirconfig-file))))
               (error "You're not in a project"))
           (error "helm-ag not available"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: magit                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :defer t
  :diminish auto-revert-mode
  :hook (magit-mode . magit-load-config-extensions)
  :config
  (setq-default magit-stage-all-confirm nil)
  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: flycheck                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: languages  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock\\'")
  :bind (:map elixir-mode-map
              ("C-c C-f" . elixir-format))
  )

(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook ((elixir-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-completion-provider :none) ;; important - prevent lsp-mode add company-capf to the head of company-backends
  ;; (setenv "PATH" (concat (expand-file-name "elixir-ls/release:" user-emacs-directory) (getenv "PATH")))
  (add-to-list 'exec-path (expand-file-name "elixir-ls/release" user-emacs-directory)))

(use-package lsp-ui
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init
  (setq lsp-ui-doc-enable nil))

(use-package helm-lsp
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . helm-lsp-workspace-symbol)))

(use-package web-mode
  :mode (("\\.html\\.erb\\'" . web-mode)
         ("\\.eex\\'" . web-mode)
         ("\\.leex\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package js
  :init
  (setq js-indent-level 2))

(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (setq css-indent-offset 2))

(provide 'setup-programming)
;;; setup-programming.el ends here
