(setq package-enable-at-startup nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
;; taken from prelude-ui.el
(setq frame-title-format
      '("TTAKASHINN - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                 "%b"))))

;; config default frame
(setq default-frame-alist
       '((fullscreen . fullheight)
         (width . 100)
         (left . 0)
         (vertical-scroll-bars . nil)
         (horizontal-scroll-bars . nil)
         (tool-bar-lines . 0)
         (font . "Source Code Pro for Powerline-13")))

;; set italic font for italic face, since Emacs does not set italic
;; face automatically
(set-face-attribute 'italic nil
                    :family "Source Code Pro for Powerline-Italic")
