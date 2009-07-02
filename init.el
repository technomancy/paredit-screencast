;; Startup script for paredit screencast demo



(load "/home/phil/src/elisp/paredit/paredit-beta")
(load "/home/phil/src/elisp/starter-kit/elpa-to-submit/clojure-mode")

(fringe-mode 0)
(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-19-*-*-*-m-0-*-*")
(set-frame-size (caadr (current-frame-configuration)) 99 30)

(ido-mode t)
(show-paren-mode t)
(tooltip-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq start-dir (file-name-directory
		 (or (buffer-file-name) load-file-name)))

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(defface paren-face
  '((((class color) (background light))
     (:foreground "grey15"))) "for parens")

(defun intro ()
  (interactive)
  (find-file (concat start-dir "intro"))
  (set-default-font "-unknown-Inconsolata-normal-normal-normal-*-60-*-*-*-m-0-*-*")
  (set-frame-size (caadr (current-frame-configuration)) 33 12)
  (font-lock-add-keywords nil '(("(\\|)" . 'paren-face)))
  (dotimes (n 70)
    (set-face-foreground 'paren-face (concat "grey" (number-to-string (- 80 n))))
    (sit-for 0.01)))