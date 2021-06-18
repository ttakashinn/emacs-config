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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized functions               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
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

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)


(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill"
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

;; Move cursor to end of current line
;; Insert new line below current line
;; it will also indent newline
(global-set-key (kbd "<C-return>") (lambda ()
                                     (interactive)
                                     (end-of-line)
                                     (newline-and-indent)))

;; Move cursor to previous line
;; Go to end of the line
;; Insert new line below current line (So it actually insert new line above with indentation)
;; it will also indent newline
(global-set-key (kbd "<C-S-return>") (lambda ()
                                       (interactive)
                                       (previous-line)
                                       (end-of-line)
                                       (newline-and-indent)
                                       ))

;; when called interactively with no active region, copy a single line instead
;; bind to M-w by default
(defadvice kill-ring-save (before slick-copy activate compile)
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Indent a region if selected, otherwise the whole buffer.
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indented selected region."))
        (progn
          (indent-buffer)
          (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

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
  "list of right bracket chars.")
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

(global-set-key (kbd "M-]") 'xah-forward-right-bracket)
(global-set-key (kbd "M-[") 'xah-backward-left-bracket)

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

(global-set-key (kbd "M-1") 'xah-select-text-in-quote)

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

(global-set-key (kbd "M-2") 'xah-select-line)

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

(global-set-key (kbd "M-3") 'xah-select-block)

(provide 'setup-editing)
;;; setup-editing ends here
