;;; package --- Summary
;;; Commentary:
;;; Code:

;; hippie-expand is a better version of dabbrev-expand.
;; While dabbrev-expand searches for words you already types, in current;; buffers and other buffers, hippie-expand includes more sources,
;; such as filenames, klll ring...
(use-package hippie-exp
  :bind ("M-/" . hippie-expand)
  :config
  (setq
   hippie-expand-try-functions-list
   '(try-expand-dabbrev ;; Try to expand word "dynamically", searching the current buffer.
     try-expand-dabbrev-all-buffers ;; Try to expand word "dynamically", searching all other buffers.
     try-expand-dabbrev-from-kill ;; Try to expand word "dynamically", searching the kill ring.
     try-complete-file-name-partially ;; Try to complete text as a file name, as many characters as unique.
     try-complete-file-name ;; Try to complete text as a file name.
     try-expand-all-abbrevs ;; Try to expand word before point according to all abbrev tables.
     try-expand-list ;; Try to complete the current line to an entire line in the buffer.
     try-expand-line ;; Try to complete the current line to an entire line in the buffer.
     try-complete-lisp-symbol-partially ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
     try-complete-lisp-symbol) ;; Try to complete word as an Emacs Lisp symbol.
   ))

;; config Ibuffer
(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))

  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq ibuffer-use-other-window t) ;; always display ibuffer in another window
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process))))

;; highlight the current line
(use-package hl-line
  :diminish hl-line-mode
  :config
  (global-hl-line-mode)
  (set-face-background hl-line-face "gray10"))

;; highlight number
(use-package highlight-numbers
  :diminish highlight-numbers-mode
  :hook (prog-mode . highlight-numbers-mode))

;; highlight symbol
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :hook ((prog-mode . highlight-symbol-mode)
         (org-mode . highlight-symbol-mode))
  :config
  (highlight-symbol-nav-mode)
  (setq highlight-symbol-idle-delay 0.2
        highlight-symbol-on-navigation-p t)
  (global-set-key [(control shift mouse-1)]
                  (lambda (event)
                    (interactive "e")
                    (goto-char (posn-point (event-start event)))
                    (highlight-symbol-at-point)))
  (global-set-key (kbd "M-n") 'highlight-symbol-next)
  (global-set-key (kbd "M-p") 'highlight-symbol-prev))

;; brings visual feedback to some operations by highlighting portions relating to the operations.
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; Pair-wise colored parens.
(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

;; Scale the text of all the windows/frames at the same time.
(use-package default-text-scale
  :bind (("s-=" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)))

;; This package highlights the cursor every time it jumps abruptedly from a
;; place to another (e.g. when changing windows and so on).
(use-package beacon
  :diminish beacon-mode
  :config
  (beacon-mode 1))

;; clean auto-indent and backspace unindent
(use-package clean-aindent-mode
  :hook (prog-mode . clean-aindent-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of multiple cursors config
(use-package multiple-cursors
  :bind (("s-d" . mc/mark-next-like-this)
         ("s-D" . mc/unmark-next-like-this)
         ("M-s-D" . mc/skip-to-next-like-this)
         ("S-s-<mouse-1>" . mc/add-cursor-on-click)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of multiple cursors config

;; update any change made on file to the current buffer
(global-auto-revert-mode)

;; display line number
(add-hook 'prog-mode-hook 'linum-mode)

;; enable electric-pair-mode
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Show trailing whitespace on programming modes.
(add-hook 'prog-mode-hook
          '(lambda () (setq-default show-trailing-whitespace t)))

;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; highlight paired parenthesis
(show-paren-mode 1)

;; easier window navigation
;; move point from window to window using Shift and the arrow keys.
(windmove-default-keybindings)

;; show important whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                            (setq-local whitespace-style
                                        '(face
                                          tabs
                                          tab-mark
                                          spaces
                                          space-mark
                                          trailing
                                          indentation::space
                                          indentation::tab
                                          newline
                                          newline-mark))
                            (whitespace-mode 1)))

(provide 'setup-convenience)
;;; setup-convenience ends here
