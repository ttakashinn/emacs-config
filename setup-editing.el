;;; package --- Summary
;;; Commentary:
;;; Code:

;; increase *-ring-max
(setq global-mark-ring-max 5000
      mark-ring-max 5000
      kill-ring-max 5000)

;; add new line at the end of file, in certain major modes
(setq mode-require-final-newline t)

;; default to 2 visible spaces to display a tab
(setq-default tab-width 2
              indent-tabs-mode nil)

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; delete selected region when type
(delete-selection-mode)

;; disable region selection using shift key
(setq shitf-select-mode nil)

;; org
(use-package org
  :init
  (setq org-directory "~/OneDrive/work/org/")
  (setq org-startup-indented t)
  :bind (("C-c l" . org-stored-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook ((org-mode . visual-line-mode)
         (org-mode . toggle-word-wrap)
         (org-mode . variable-pitch-mode)))

;; C-w kills entire line when no active regions
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :config
  (whole-line-or-region-global-mode 1))

;; duplicate line or region
(use-package duplicate-thing
  :bind ("M-c" . duplicate-thing))

;; moves selected region around.
(use-package drag-stuff
  :diminish drag-stuff-mode
  :bind (("s-<down>" . drag-stuff-down)
         ("s-<up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode))

;; increases the selected region by semantic units
(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

;; when called interactively with no active region, copy a single line instead
;; bind to M-w by default
(defadvice kill-ring-save (before slick-copy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized functions               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-a") 'nt/smarter-beginning-of-line)
(global-set-key (kbd "C-x k") 'nt/kill-default-buffer)
(global-set-key (kbd "<C-return>") 'nt/open-line-below)
(global-set-key (kbd "<C-S-return>") 'nt/open-line-above)
(global-set-key (kbd "C-c i") 'nt/indent-region-or-buffer)
(global-set-key (kbd "M-]") 'xah-forward-right-bracket)
(global-set-key (kbd "M-[") 'xah-backward-left-bracket)
(global-set-key (kbd "M-1") 'xah-select-text-in-quote)
(global-set-key (kbd "M-2") 'xah-select-line)
(global-set-key (kbd "M-3") 'xah-select-block)

(global-set-key (kbd "<f8>") 'nt/edit-init-file)
(global-set-key (kbd "C-x \\") 'nt/split-window-horizontally-and-focus-new)
(global-set-key (kbd "C-x -") 'nt/split-window-vertically-and-focus-new)
(global-set-key (kbd "C-x p") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x O") 'other-frame)
(global-set-key (kbd "C-c e") 'nt/elixir-new-exs-buffer)
(global-set-key (kbd "s-b") 'nt/switch-to-previous-buffer)
(global-set-key (kbd "s-J") 'nt/join-line-below)
(global-set-key (kbd "s-<") 'previous-buffer)
(global-set-key (kbd "s->") 'next-buffer)

(defun nt/smarter-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun nt/kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill"
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))


(defun nt/open-line-below ()
  "Open a line below the current one (like vim's 'o')."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun nt/open-line-above ()
  "Open a line above the current one (like vim's 'O')."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line)
  (indent-for-tab-command))

(defun nt/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun nt/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (nt/indent-buffer)
        (message "Indented buffer.")))
    (whitespace-cleanup)))

(defun nt/edit-init-file ()
  "Edit the init file, usually ~/.emacs.d/init.el."
  (interactive)
  (find-file (or user-init-file "")))

(defun nt/newline-aqnd-indent-like-previous-line ()
  "Create a newline and indent at the same level of the previous line."
  (interactive)
  (newline)
  (indent-relative-maybe))

(defun nt/split-window-horizontally-and-focus-new ()
  "Splits the window horizontally and focus the new one."
  (interactive)
  (nt/--split-window-and-focus-new 'split-window-horizontally))

(defun nt/split-window-vertically-and-focus-new ()
  "Splits the window vertically and focus the new one."
  (interactive)
  (nt/--split-window-and-focus-new 'split-window-vertically))

(defun nt/--split-window-and-focus-new (splitting-fun)
  (funcall splitting-fun)
  (other-window 1))

(defun nt/elixir-new-exs-buffer ()
  "Create a scratch buffer in Elixir mode."
  (interactive)
  (switch-to-buffer "*elixir scratch*")
  (elixir-mode))

(defun nt/switch-to-previous-buffer ()
  "Switch to the previously visited buffer.
If called multiple times, basically alternate between two buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun nt/join-line-below ()
  "Joins the line below the current one into the current one."
  (interactive)
  (delete-indentation 1))

;; Move Cursor to Bracket/Quote
(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  ;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "List of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

;; Select Text between Quotes/Brackets
(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: '\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command select between any bracket chars, not the inner text of a bracket. For example, if text is

 (a(b)c▮)

 the selected char is “c”, not “a(b)c”.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2020-03-11"
  (interactive)
  (let (
        ($skipChars "^'\"`<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）〘〙")
        $p1
        )
    (skip-chars-backward $skipChars)
    (setq $p1 (point))
    (skip-chars-forward $skipChars)
    (set-mark $p1)))

;; Select current line
(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-11-01"
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (progn
      (end-of-line)
      (set-mark (line-beginning-position)))))

;; Select current block
(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2019-12-26"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move"))))

(provide 'setup-editing)
;;; setup-editing ends here
