;;; Emacs configuration of Jorgen Schäfer

;; I don't use XEmacs.  This file does not work with XEmacs.
(when (featurep 'xemacs)
  (error "This .emacs file does not work with XEmacs."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global configuration

(when (window-system)
  (set-default-font "-bitstream-bitstream vera sans mono-*-r-*-*-17-*-*-*-*-*-*-*")
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq x-select-enable-primary t
        x-select-enable-clipboard t))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "utf-8")
(prefer-coding-system 'latin-1)
(prefer-coding-system 'utf-8)

(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-font-lock-mode 1)
(auto-compression-mode 1)
(column-number-mode 1)
(line-number-mode 1)
(display-time)
(random t)
(epa-file-enable)

;; I like seeing the region
(transient-mark-mode 1)
;; But I also like working with the region when I don't see it
(setq mark-even-if-inactive t)

(setq show-paren-delay 0
      show-paren-style 'parenthesis)
(show-paren-mode 1)

;; Selecting with the mouse is nice
(when (fboundp 'mouse-sel-mode)
  (mouse-sel-mode 1))

;; get rid of yes-or-no questions - y or n is enough
(defalias 'yes-or-no-p 'y-or-n-p)

;; I use this for some file
(add-hook 'before-save-hook 'time-stamp)

;; Use aspell when available
(when (executable-find "aspell")
  (setq-default ispell-program-name "aspell"))

;; I dislike the flyspell word completion stuff.
(when (load "flyspell" t t)
  (define-key flyspell-mode-map (kbd "M-TAB") 'ispell-complete-word)
  )

;; Put backup files neatly away
(let ((backup-dir "~/tmp/emacs/backups")
      (auto-saves-dir "~/tmp/emacs/auto-saves/")
      (auto-save-list-dir "~/tmp/emacs/auto-save-list/")
      (tramp-autosaves-dir "~/tmp/emacs/tramp-auto-saves/")
      (tramp-backup-dir "~/tmp/emacs/tramp-backups/"))
  (dolist (dir (list backup-dir
                     auto-saves-dir
                     auto-save-list-dir
                     tramp-autosaves-dir
                     tramp-backup-dir))
    (when (not (file-directory-p dir))
      (make-directory dir t)))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
        auto-save-list-file-prefix (concat auto-save-list-dir ".saves-")
        tramp-backup-directory-alist `((".*" . ,tramp-backup-dir))
        tramp-auto-save-directory tramp-autosaves-dir))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(setq-default
 ;; we usually want a final newline...
 require-final-newline 'ask
 ;; No, please, no tabs in my programs!
 indent-tabs-mode nil
 ;; If you don't know, just give me text-mode
 major-mode 'text-mode
 ;; I don't like emacs destroying my window setup
 even-window-heights nil
 ;; Same here
 resize-mini-windows nil
 ;; Two spaces after a colon is wrong[tm] in german.  There's even a
 ;; DIN for that... (DIN 5008)
 sentence-end-double-space nil
 ;; No am/pm here
 display-time-24hr-format t
 ;; A tab is 8 spaces is 8 spaces is 8 spaces
 tab-width 8
 ;; Scrolling is moving the document, not moving my eyes
 scroll-preserve-screen-position 'keep
 ;; My email address
 user-mail-address (with-temp-buffer
                     (insert-file "~/.email")
                     (goto-char (point-min))
                     (buffer-substring-no-properties
                      (point) (point-at-eol)))
 ;; I kinda know my emacs
 inhibit-startup-message t
 ;; unified diff for the masses
 diff-switches "-u"
 ;; nice comment format
 comment-style 'extra-line
 ;; case insensitivity for the masses!
 case-fold-search t
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 ;; Don't show a cursor in other windows
 cursor-in-non-selected-windows nil
 ;; A wide characters ask for a wide cursor
 x-stretch-cursor t
 ;; i want a mouse yank to be inserted where the point is, not where i click
 mouse-yank-at-point t
 ;; Don't highlight stuff that I can click on all the time. I don't click
 ;; anyways.
 mouse-highlight 1
 ;; I prefer bash-style to zsh-style
 pcomplete-cycle-completions nil
 ;; > may show up in prompts (my default prompt)
 shell-prompt-pattern "^[^#$%\n]*[#$%>] *"
 ;; C-h a should check for more stuff
 apropos-do-all t
 ;; Single dash starts a paragraph
 paragraph-start "^[#;]* *-\\|\f\\|[      ]*$"
 ;; Some nice times for M-x display-time-world
 display-time-world-list '(("America/New_York" "Sarasota")
                           ("Europe/Berlin" "Hamburg")
                           ("Europe/Helsinki" "Helsinki")
                           ("Etc/GMT+12" "Wellington")
                           )
 )

;; Auto-filling in prog mode is great for comments.
(setq comment-auto-fill-only-comments t)
(add-hook 'prog-mode-hook 'auto-fill-mode)

;; Full-line completion is *annoying*
(setq hippie-expand-try-functions-list
      (delq 'try-expand-line
            hippie-expand-try-functions-list))

(setq delete-trailing-lines t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Fix some keyboard sequences for various terminals
(mapc (lambda (map)
        (define-key function-key-map
          (read-kbd-macro (cadr map))
          (read-kbd-macro (car map))))
      '(("<S-up>"     "ESC [1;2A")
        ("<S-down>"   "ESC [1;2B")
        ("<S-right>"  "ESC [1;2C")
        ("<S-left>"   "ESC [1;2D")

        ("<M-up>"     "ESC [1;3A")
        ("<M-down>"   "ESC [1;3B")
        ("<M-right>"  "ESC [1;3C")
        ("<M-left>"   "ESC [1;3D")

        ("<M-S-up>"     "ESC [1;4A")
        ("<M-S-down>"   "ESC [1;4B")
        ("<M-S-right>"  "ESC [1;4C")
        ("<M-S-left>"   "ESC [1;4D")

        ("<C-up>"     "ESC [1;5A")
        ("<C-down>"   "ESC [1;5B")
        ("<C-right>"  "ESC [1;5C")
        ("<C-left>"   "ESC [1;5D")

        ("<C-prior>"  "ESC [5;5~")
        ("<C-next>"   "ESC [6;5~")
        ("<C-delete>" "ESC [3;5~")
        ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Custom commands

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "<f5>") 'recompile)

(global-set-key (kbd "C-x r a") 'fc/add-rectangle)
(defun fc/add-rectangle (start end)
  "Add all the lines in the region-rectangle and put the result in the
kill ring."
  (interactive "r")
  (let ((sum 0))
    (mapc (lambda (line)
            (string-match "-?[0-9.]+" line)
            (setq sum (+ sum (string-to-number (match-string 0 line)))))
          (extract-rectangle start end))
    (kill-new (number-to-string sum))
    (message "%s" sum)))

(global-set-key (kbd "C-c e") 'fc/eval-and-replace)
(defun fc/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (prin1 (eval (read (current-kill 0)))
         (current-buffer)))

(global-set-key (kbd "C-c m") 'fc/calculate-region)
(defun fc/calculate-region (start end &optional prefix)
  "Evaluate the mathematical expression within the region, and
replace it with its result.

With a prefix arg, do not replace the region, but instead put the
result into the kill ring."
  (interactive "r\nP")
  (let* ((expr (buffer-substring start end))
         (result (fc/bc-calculate-expression expr))
         (ends-with-newline (string-match "\n$" expr)))
    (if prefix
        (progn
          (kill-new result)
          (message "%s" result))
      (kill-region start end)
      (insert result)
      (when ends-with-newline
        (insert "\n")))))

(defun fc/bc-calculate-expression (expr)
  "Evaluate `expr' as a mathematical expression, and return its result.

This actually pipes `expr' through bc(1), replacing newlines with
spaces first. If bc(1) encounters an error, an error is
signalled."
  (with-temp-buffer
    (insert expr)
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match " " nil t))
    (goto-char (point-max))
    (insert "\n")
    (call-process-region (point-min)
                          (point-max)
                         "bc" t t nil "-lq")
    (goto-char (point-min))
    (when (search-forward "error" nil t)
      (error "Bad expression"))
    (while (search-forward "\n" nil t)
      (replace-match "" nil t))
    (buffer-string)))

(global-set-key (kbd "C-c d") 'fc/insert-date)
(defun fc/insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%d.%m.%Y")
                 (t "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c C-u") 'fc/kill-to-beginning-of-line)
(defun fc/kill-to-beginning-of-line ()
  "Kill from the beginning of the line to point."
  (interactive)
  (kill-region (point-at-bol)
               (point)))

(global-set-key (kbd "C-x 8 p") 'fc/unicode-info-at-point)
(defun fc/unicode-info-at-point (&optional do-kill)
  "Display the unicode name of the character at point."
  (interactive "P")
  (let ((char-code (elt (thing-at-point 'char) 0))
        name)
    (setq name (get-char-code-property char-code 'name))
    (when (or (not name)
              (= ?< (elt name 0)))
      (setq name (get-char-code-property char-code 'old-name)))
    (when do-kill
      (kill-new name))
    (message "%s" name)))

(defun rename-buffer-and-file (new-file-name)
  "Rename the current buffer's file to NEW-FILE-NAME.

Also, rename the buffer and attach it to the new file."
  (interactive
   (list (read-file-name "Rename to: "
                         nil buffer-file-name
                         nil buffer-file-name)))
  (rename-file buffer-file-name
               new-file-name)
  (rename-buffer (file-name-nondirectory new-file-name)
                 t)
  (setq buffer-file-name new-file-name)
  (normal-mode))

;; By Stefan Monnier It is the opposite of fill-paragraph: Takes a
;; multi-line paragraph and makes it into a single line of text.
(global-set-key (kbd "C-c u") 'unfill-paragraph)
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, "
          "sed do eiusmod tempor incididunt ut labore et dolore "
          "magnaaliqua. Ut enim ad minim veniam, quis nostrud "
          "exercitation ullamco laboris nisi ut aliquip ex ea commodo "
          "consequat. Duis aute irure dolor in reprehenderit in "
          "voluptate velit esse cillum dolore eu fugiat nulla pariatur. "
          "Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun google (what)
  "Use google to search for WHAT."
  (interactive "sSearch: ")
  (browse-url (format "http://www.google.de/search?q=%s" what)))

(defun leo (word)
  (interactive "sWord: ")
  (browse-url (format "http://dict.leo.org/?search=%s" word)))

(global-set-key (kbd "C-c l") 'leo-at-point)
(defun leo-at-point ()
  "Open the Leo dictionary for the word at point."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (if (not word)
        (error "No word found at point")
      (browse-url (format "http://dict.leo.org/?search=%s#results"
                          word)))))

(defun fc/htmlfontify-buffer-or-region (for-blog-p)
  "Show the current buffer or region if active as HTML in a temporary buffer.

This uses `htmlfontify'."
  (interactive "P")
  (let ((hfy-page-footer (lambda (filename)
                           ""))
        (hfy-page-header (lambda (filename stylesheet)
                           (if for-blog-p
                               "<link href=\"http://www.jorgenschaefer.de/css/elisp.css\" rel=\"stylesheet\" type=\"text/css\">\n"
                               ""))))
    (if (region-active-p)
        (let ((text (buffer-substring (region-beginning)
                                      (region-end))))
          (with-temp-buffer
            (insert text)
            (switch-to-buffer (htmlfontify-buffer))))
      (switch-to-buffer (htmlfontify-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various major modes shipped with Emacs

;;;;;;;;;;;;;;;
;;; Comint mode

(defun fc/init-comint ()
  ;; Don't jump around when output in a buffer happens
  (set (make-local-variable 'scroll-conservatively) 1000))

(add-hook 'comint-mode-hook 'fc/init-comint)

;;;;;;;;;;;;
;;; CUA mode

;; Those are terribly annoying, as they make it difficult to use a lot
;; of Emacs commands that operate on the active region. Including
;; user-defined stuff under C-c *.
(setq cua-enable-cua-keys nil)
;; Can't hit <C-return> in terminals
(setq cua-rectangle-mark-key (kbd "C-x r RET"))

(cua-mode 1)
;; Using customize, because this has a non-trivial :set method.
(customize-set-variable 'cua-rectangle-mark-key (kbd "C-x r RET"))

;;;;;;;;;
;;; Dired

(load-library "dired")
(define-key dired-mode-map (kbd "a") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "RET") 'dired-find-file)

;;;;;;;;;;;;;;
;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(define-key emacs-lisp-mode-map (kbd "M-.") 'dabbrev-expand)
(define-key lisp-interaction-mode-map (kbd "M-.") 'dabbrev-expand)

;;;;;;;;;;;;;;;
;;; Python Mode

;; Uses elpy, see below

(load "python" nil t)

(define-key python-mode-map (kbd "C-c S") 'fc/python-insert-super)
(defun fc/python-insert-super ()
  "Insert a Python super() statement for the current class."
  (interactive)
  (let (name split)
    (setq name (save-excursion
                 ;; For some silly reason, p-i-c-d does not work if
                 ;; the method has no text in it?!
                 (insert "x")
                 (prog1 (python-info-current-defun)
                   (delete-char -1))))
    (when (not name)
      (error "Can't find class and method names"))
    (setq split (split-string name "\\."))
    (when (not (cadr split))
      (error "Not in a method"))
    (insert (format "super(%s, self).%s()\n"
                    (car split)
                    (cadr split)))
    (indent-for-tab-command)))

;;;;;;;;;;
;;; Scheme

;; Make parens less visible
(font-lock-add-keywords 'scheme-mode '(("[()]" . 'paren-face)))
(defface paren-face
  '((t (:foreground "gray60")))
  "The face used for parenthesises.")

;;;;;;;;;;
;;; C Mode
(add-hook 'c-mode-hook 'fc/c-setup)
(defun fc/c-setup ()
  "Set up C mode for my needs."
  (c-set-style "k&r")
  (setq c-basic-offset 4)
  (c-set-offset 'case-label '+)
  (c-set-offset 'inextern-lang 0))

;;;;;;;;
;;; Perl
(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))

;;;;;;;;;;;;
;;; Uniquify
(when (load "uniquify" t t)
  (setq-default uniquify-buffer-name-style 'post-forward))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Version Control Mode
(setq vc-diff-switches diff-switches
      vc-initial-comment t)

;; Git extension
(when (load "gitty" t t)
  (gitty-mode))

;;;;;;;;;;;;;;;
;;; Comint Mode
(add-to-list 'comint-output-filter-functions
             'comint-watch-for-password-prompt)
(ansi-color-for-comint-mode-on)

(when (load "comint-scroll-to-bottom" t t)
  (add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom))

;;;;;;;;;;;;
;;; ido-mode
(ido-mode 1)
(setq ido-everywhere t
      ido-case-fold t
      ido-use-filename-at-point nil
      ido-use-url-at-point nil
      ido-confirm-unique-completion t
      ;; This is cute. Except when you want to open a new file, then
      ;; it's annoying as hell.
      ido-auto-merge-work-directories-length -1
      ido-enable-flex-matching t)

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-common-completion-map (kbd "C-c")
              (make-sparse-keymap))
            (define-key ido-common-completion-map (kbd "C-c C-u")
              'fc/ido-copy-selection)
            (define-key ido-file-dir-completion-map (kbd "<up>")
              'ido-prev-work-directory)
            (define-key ido-file-dir-completion-map (kbd "<down>")
              'ido-next-work-directory)))

(defun fc/ido-copy-selection ()
  "Copy the current ido selection to the kill ring."
  (interactive)
  (kill-new (abbreviate-file-name (concat ido-current-directory
                                          ido-text))))

;;;;;;;;;;;;;;;;
;;; isearch-mode

(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch, but it only looks for the
rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward)
                isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))

;;;;;;;;;;;;;;
;;; shell-mode

(global-set-key (kbd "C-c s") 'fc/toggle-shell)
(defun fc/toggle-shell ()
  "Switch between the last active buffer and the shell."
  (interactive)
  (if (eq major-mode 'shell-mode)
      (let ((buf (catch 'return
                   (dolist (buf (cdr (buffer-list)))
                     (when (not (string-prefix-p " " (buffer-name buf)))
                       (throw 'return buf)))
                   nil)))
        (when buf
          (switch-to-buffer buf)))
    (shell)))

(when (load "shell" t t)
  (define-key shell-mode-map (kbd "C-c C-y") 'fc/shell-switch-dir)
  (defun fc/shell-switch-dir ()
    "Switch `shell-mode' to the `default-directory' of the last buffer."
    (interactive)
    (when (eq major-mode 'shell-mode)
      (let* ((dir (catch 'return
                    (dolist (buf (buffer-list))
                      (with-current-buffer buf
                        (when buffer-file-name
                          (throw 'return default-directory)))))))
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (format "cd %s" (shell-quote-argument dir)))
        (let ((comint-eol-on-send nil))
          (comint-send-input))))))

;;;;;;;;;;;;;;;;
;;; Emacs Server
(load "server" t t)
(when (not (server-running-p server-name))
  (server-start))

;;;;;;;;;;;;;
;;; html-mode
(load "sgml-mode" t t)

(define-key html-mode-map (kbd "C-c RET") 'fc/html-toggle-paragraph)
(define-key text-mode-map (kbd "C-c p") 'fc/html-toggle-paragraph)
(defun fc/html-toggle-paragraph ()
  "Add or remove HTML paragraph tags from the current paragraph"
  (interactive)
  (save-excursion
    (backward-paragraph)
    (when (looking-at "^\\s-*$")
      (forward-char 1))
    (if (looking-at "<p>")
        (replace-match "")
      (insert "<p>"))
    (forward-paragraph)
    (when (looking-at "^\\s-*$")
      (backward-char 1))
    (if (looking-back "</p>")
        (replace-match "")
      (insert "</p>"))))

(define-key html-mode-map (kbd "&") 'fc/html-insert-quoted)
(defun fc/html-insert-quoted (char)
  "Insert a & character.

Depending on the subsequent character, insert an appropriate HTML
glyph."
  (interactive "cInsert: ")
  (case char
    ((?&)
     (insert "&amp;"))
    ((?<)
     (insert "&lt;"))
    ((?>)
     (insert "&gt;"))
    ((?\s)
     (insert "&nbsp;"))
    ((34) ; "
     (insert "&quot;"))
    (t
     (insert (format "&#x%02x;" char)))))

;;;;;;;;;;;;
;;; Org Mode
(when (load "org" t t)
  (define-key org-mode-map (kbd "C-c a") 'fc/org-agenda)
  (defun fc/org-agenda ()
    (interactive)
    (when (get-buffer "google-calendar.org")
      (kill-buffer "google-calendar.org"))
    (org-agenda nil (caar org-agenda-custom-commands)))

  (setq org-fontify-emphasized-text nil
        org-tags-column 40
        org-agenda-files nil
        org-descriptive-links nil
        org-agenda-include-diary nil
        org-agenda-start-on-weekday nil
        org-todo-keywords '((sequence "TODO"
                                      "DONE"
                                      "WAITING"))
        org-agenda-custom-commands '(("t" "General TODO agenda"
                                      ((todo "TODO")
                                       (agenda "")
                                       (todo "WAITING")))))
  (dolist (filename '("~/Documents/Notes/Todo"
                      "~/Documents/Notes/EVE"
                      "~/Documents/Notes/google-calendar.org"))
    (when (file-exists-p filename)
      (add-to-list 'org-agenda-files filename t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Third party extensions
(when (file-directory-p "~/.emacs.d/site-lisp")
  (add-to-list 'load-path "~/.emacs.d/site-lisp")
  (dolist (dirname (directory-files "~/.emacs.d/site-lisp" t "^[^.]"))
    (when (file-directory-p dirname)
      (add-to-list 'load-path dirname))))

(when (load "package" t t)
  (add-to-list 'package-archives
               '("marmalade" .
                 "http://marmalade-repo.org/packages/"))
  (package-initialize))

;;;;;;;;
;;; Elpy
(when (load "elpy" t t)
  (setq nose-use-verbose nil)
  (elpy-enable)
  (elpy-clean-modeline))

;;;;;;;;;
;;; iedit
(when (load "iedit" t t)
  (dolist (map (list global-map isearch-mode-map
                     esc-map help-map))
    (define-key map (kbd "M-,") 'iedit-mode)))

;;;;;;;;;;;;
;;; fci mode

;; (when (and window-system
;;            (load "fill-column-indicator" t t))
;;   (setq-default fci-rule-column 80)
;;   (add-hook 'prog-mode-hook 'fci-mode))

;;;;;;;;;;;;;
;;; Typo Mode
(when (load "typo" t t)
  (dolist (hook '(markdown-mode-hook html-mode-hook))
    (add-hook hook 'typo-mode)))

;;;;;;;;;;;;;;;;;;
;;; Win Point Mode

;; Is this obsoleted by `switch-to-buffer-preserve-window-point'?
(when (load "winpoint" t t)
  (window-point-remember-mode 1))

;;;;;;;;;;;;
;;; Legalese
(load "legalese" t t)

;;;;;;;;;
;;; Bitly
(when (load "bitly" t t)
  (global-set-key (kbd "C-c U") 'bitly-url-at-point))

;;;;;;;;;;;;;;;;
;;; Paredit mode
(when (load "paredit" t t)
  (define-key paredit-mode-map (kbd "RET") 'newline)
  (define-key paredit-mode-map (kbd "C-j") 'paredit-newline)
  (define-key paredit-mode-map (kbd "<C-left>") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "<C-right>") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-q") 'paredit-reindent-defun)

  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode))

;;;;;;;;;;;;;;
;;; JavaScript

(when (load "js2-mode" t t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'auto-complete-mode)
  (setq js2-highlight-level 3
        js2-mode-indent-ignore-first-tab t
        js2-mode-indent-inhibit-undo t
        js2-global-externs '("$")
        )
  (define-key js2-mode-map (kbd "C-c C-n") 'js2-next-error)

  (defadvice js2-mode-toggle-element (before ad-move-to-toggle-element
                                             activate)
    "Move to a sensible location first"
    (interactive)
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (invisible (next-single-property-change start 'invisible nil end))
           (brace (save-excursion
                    (goto-char start)
                    (re-search-forward "{" end t))))
      (cond
       (invisible (goto-char invisible))
       (brace (goto-char brace))))))

;;;;;;;;;
;;; magit
(when (load "magit" t t)
  (global-set-key (kbd "C-x v g") 'magit-status)
  (global-set-key (kbd "C-x v a") 'vc-annotate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edit-server (for Chrome)
(when (load "edit-server" t t)
  (setq edit-server-new-frame nil)
  (edit-server-start))

;;;;;;;;;;;;;;;;;
;;; markdown-mode

(when (load "markdown-mode" t t)
  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (setq markdown-command "markdown_py"
        markdown-italic-underscore t
        markdown-indent-on-enter nil
        markdown-follow-wiki-link-on-enter nil
        )
  (add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
  (define-key markdown-mode-map (kbd "TAB") 'markdown-cycle)
  (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
  (define-key markdown-mode-map (kbd "C-M-f") 'forward-sexp)
  (define-key markdown-mode-map (kbd "C-M-b") 'backward-sexp)

  (define-key markdown-mode-map (kbd "C-c C-x") 'fc/markdown-code-block)
  (defun fc/markdown-code-block (beg end)
    "Wrap the current region into a code block."
    (interactive "r")
    (save-excursion
      (goto-char end)
      (when (not (bolp))
        (insert "\n"))
      (insert "```\n")
      (goto-char beg)
      (forward-line 0)
      (insert "```\n")))

  (define-key markdown-mode-map (kbd "C-c C-n") 'fc/markdown-next-header)
  (defun fc/markdown-next-header ()
    "Go to the next header in the file."
    (interactive)
    (let ((next-header (save-excursion
                         (forward-line 1)
                         (re-search-forward "^#" nil t))))
      (if (not next-header)
          (error "No next header")
        (goto-char next-header)
        (goto-char (point-at-bol)))))

  (define-key markdown-mode-map (kbd "C-c C-p") 'fc/markdown-previous-header)
  (defun fc/markdown-previous-header ()
    "Go to the previous header in the file."
    (interactive)
    (let ((previous-header (save-excursion
                             (forward-line -1)
                             (re-search-backward "^#" nil t))))
      (if (not previous-header)
          (error "No previous header")
        (goto-char previous-header)
        (goto-char (point-at-bol)))))

  (defun markdown-check-change-for-wiki-link (&rest ignored)
    "Do nothing.

The default markdown implementation exhibits a bug. You can
reproduce it using the following:

M-: (when (looking-at \"\") (replace-match \"abc\"))

This will insert \"abc\" at the current point, but move point
down one line. Removing `markdown-check-change-for-wiki-link'
from `after-change-functions' fixes that."
    nil)
  )

;;;;;;;;;;;;;
;;; emacs-w3m
(when (load "w3m" t t)
  (setq-default browse-url-browser-function 'w3m-browse-url)

  ;; This might help in saving cookies
  (add-hook 'kill-emacs-hook
            (lambda ()
              (w3m-quit t)))

  (setq w3m-use-cookies t
        w3m-cookie-accept-bad-cookies t
        w3m-use-tab nil
        w3m-use-tab-menubar nil
        w3m-auto-show nil)

  (define-key w3m-mode-map (kbd "C-c C-@") 'lui-track-next-buffer)
  (define-key w3m-mode-map (kbd "C-c c") 'fc/copy-url)
  (define-key w3m-mode-map (kbd "<down>") 'next-line)
  (define-key w3m-mode-map (kbd "<up>") 'previous-line)
  (define-key w3m-mode-map (kbd "<right>") 'forward-char)
  (define-key w3m-mode-map (kbd "<left>") 'backward-char)
  (define-key w3m-mode-map (kbd "C-x b") 'ido-switch-buffer)

  ;; Change w3m buffers to the url they show
  ;; Thanks to marienz (#emacs) on .emacs for the idea
  (add-hook 'w3m-display-hook 'fc/w3m-rename-buffer)
  (defun fc/w3m-rename-buffer (url)
    (rename-buffer url t))

  (add-hook 'w3m-form-input-textarea-mode-hook 'fc/remove-cr)
  (defun fc/remove-cr ()
    "Remove all occurrences of ^M in the current buffer."
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\r" nil t)
        (replace-match ""))))

  (defun fc/copy-url (n)
    "Copy the current URL to the kill ring, or the current anchor URL if
a prefix argument is given."
    (interactive "p")
    (let ((url (if (= n 1)
                   w3m-current-url
                 (w3m-anchor))))
      (if url
          (kill-new url)
        (error "No url.")))))

;;;;;;;;;;;;;;;;;
;;; Expand Region
(when (load "expand-region" t t)
  (global-set-key (kbd "C-@") 'fc/set-mark-command)
  (global-set-key (kbd "C-SPC") 'fc/set-mark-command)
  (defun fc/set-mark-command (arg)
    "Call `set-mark-command' on first call, else `er/expand-region'."
    (interactive "P")
    (if (and (not arg)
             (eq last-command 'fc/set-mark-command))
        (er/expand-region 1)
      (if cua-mode
          (cua-set-mark arg)
        (set-mark-command arg)))))

;;;;;;;;;;;
;;; Keyfreq

(when (load "keyfreq" t t)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;;;;;;;;;
;;; Circe
(when (load "circe" t t)
  (defun irc ()
    "Connect to IRC."
    (interactive)
    (circe "Freenode")
    (circe "IRCnet")
    (circe "Coldfront")
    (circe "Bitlbee")
    )

  (setq freenode-passwd nil
        bitlbee-passwd nil
        coldfront-passwd nil
        bitly-pass nil)
  (when (file-exists-p "~/.private.el")
    (load-file "~/.private.el"))

  (setq circe-default-realname "http://www.jorgenschaefer.de/"
        circe-server-killed-confirmation 'ask-and-kill-all
        circe-format-server-topic "*** Topic change by {origin}: {topic-diff}"
        circe-lagmon-mode-line-format-string "l:%.1f "
        circe-lagmon-mode-line-unknown-lag-string "l:? "
        circe-reduce-lurker-spam t
        circe-network-options
        `(("Freenode"
           :host "istinn.electusmatari.com" :port 7778 :tls t
           :pass ,freenode-pass
           :nick "forcer"
           :channels ("#emacs" "#emacs-circe"
                      :after-auth "#emacs-ops")
           :nickserv-password ,freenode-password
           :nickserv-ghost-style immediate
           )
          ("IRCnet"
           :host "istinn.electusmatari.com" :port 7778 :tls t
           :pass ,ircnet-pass
           :nick "forcer"
           :channels ("#kollektiv")
           )
          ("Coldfront"
           :host "istinn.electusmatari.com" :port 7778 :tls t
           :pass ,coldfront-pass
           :nick "Arkady"
           :nickserv-password ,coldfront-password
           :channels ("#grd" "#electusmatari" "#em-private")
           )
          ("Bitlbee"
           :port 6667
           :nick "Jorgen"
           :nickserv-password ,bitlbee-password
           :lagmon-disabled t
           )))

  (load "circe-lagmon")
  (circe-lagmon-mode)

  (setq lui-max-buffer-size 30000
        lui-flyspell-p t
        lui-scroll-behavior 'post-scroll
        lui-flyspell-alist '(("#kollektiv" "german8")
                             ("" "american"))
        )

  (add-hook 'lui-post-output-hook 'fc/lui-save-highlights)
  (defun fc/lui-save-highlights ()
    (when (and (time-less-p '(0 300 0 0)
                            (or (current-idle-time)
                                '(0 0 0 0)))
               (memq 'circe-highlight-nick-face
                     (lui-faces-in-region (point-min)
                                          (point-max))))
      (let ((buffer (buffer-name))
            (target circe-chat-target)
            (network (with-circe-server-buffer
                       circe-server-network))
            ;; We're narrowed
            (text (buffer-string)))
        (with-current-buffer (get-buffer-create "*Highlights*")
          (goto-char (point-max))
          (insert (format-time-string "%Y-%m-%d %H:%M") " in "
                  (or target buffer) "@" network ":\n"
                  text
                  "\n")
          (display-buffer (current-buffer))))))

  (load "lui-autopaste")
  (add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)
  (defvar fc/lui-autopaste-service-gist-url nil
    "The URL for the last gist.")
  (defun fc/lui-autopaste-service-gist (text)
    "Paste TEXT to github using gist.el."
    ;; It's so great gist works with callbacks! It's so much fun to
    ;; work around that!
    (setq fc/lui-autopaste-service-gist-url 'waiting)
    (with-temp-buffer
      (insert text)
      (gist-region (point-min) (point-max) nil
                   (lambda (gist)
                     (when (eq fc/lui-autopaste-service-gist-url
                               'waiting)
                       (setq fc/lui-autopaste-service-gist-url
                             (oref gist :html-url))))))
    (let ((wait 0))
      (while (and (< wait 10)
                  (eq fc/lui-autopaste-service-gist-url
                      'waiting))
        (sit-for 0.1 )
        (setq wait (1+ wait))))
    (if (eq fc/lui-autopaste-service-gist-url 'waiting)
        (progn
          (setq fc/lui-autopaste-service-gist-url nil)
          (error "Couldn't create gist"))
      (prog1 fc/lui-autopaste-service-gist-url
        (setq fc/lui-autopaste-service-gist-url nil))))

  (add-hook 'lui-pre-input-hook 'fc/there-is-no-wether)
  (defun fc/there-is-no-wether ()
    "Throw an error when the buffer contains \"wether\"
Or other words I used repeatedly"
    (goto-char (point-min))
    (when (re-search-forward (regexp-opt '("wether"
                                           "occurence" "occurrance"
                                           "occurance"
                                           )
                                         'words)
                             nil t)
      (error "It's \"whether\" and \"occurrence\"!")))

  (setq bitly-access-token bitly-pass))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Startup code

(when (file-exists-p "~/Documents/Notes/Todo")
  (find-file "~/Documents/Notes/Todo")
  (org-mode)
  (org-content 1)
  (org-agenda nil "t")
  (other-window 1)
  (setq default-directory "~/"))

;; Faces

(face-spec-set 'font-lock-comment-face
               '((((class color)) (:foreground "red"))))
(face-spec-set 'highlight-indent-face
               '((((min-colors 64)) (:background "#f5f5f5"))
                 (t (:background "white"))))
(face-spec-set 'flymake-errline
               '((((min-colors 64)) (:background "#ffafaf" :inherit nil))
                 (t (:foreground "red" :bold t))))
(face-spec-set 'org-tag '((t nil)))
(face-spec-set 'rst-level-2 '((t (:foreground "cyan"))))
(face-spec-set 'rst-level-3 '((t (:foreground "cyan"))))
(face-spec-set 'rst-level-4 '((t (:foreground "cyan"))))

(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
