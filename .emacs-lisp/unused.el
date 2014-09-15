;;
;; A place for storing useful .emacs code I no longer use,
;; but might want some day.
;;


(defun flashcard () "Create flashcard lines from ed2go vocab"
  (interactive)
  ;; series of separate save-excursions because each replace
  ;; needs to run from point to the end of the file.

  ;; eliminate whitespace at BOL
  (save-excursion
    (replace-regexp "^\s*" ""))

  ;; combine any multiple lines:
  ;; another way to do this would be to loop over lines,
  ;; do some sort of looking-at trick then call join-lines.
  (save-excursion
    (let ((save-fill-column fill-column))
      (set-fill-column 1000000)
      (fill-individual-paragraphs (point) (point-max))
      (delete-matching-lines "^$")
      (set-fill-column save-fill-column)))

  (save-excursion
    (delete-matching-lines "^$"))

  ;; add python-ish stuff
  (save-excursion
    (replace-regexp "^\\([^]].*?\\) *= *\\(.*\\) *" "    \[ \"\\1\", \"\\2\" \],"))

  ;; Leave in "Dialog #" as comments
  (save-excursion
    (replace-regexp "^[\s-]*Dialog" "    # Dialog"))
)

;; Call lxr on an identifier
;; Select an identifier as the region, then C-c C-l i
(defun lxr-ident (start end) (interactive "r")
  (let ((ident (buffer-substring start end)))
    (shell-command (concat "mozilla-remote --newwin http://lxr.mozilla.org/seamonkey/ident\?i=" ident))))
(global-set-key "\C-c\C-li" 'lxr-ident)

(defun wrap-html-tag (tagName)
  "Add a tag to beginning and ending of current word or text selection."
  (interactive "sEnter tag name: ")
  (let (p1 p2 inputText)
    (if (use-region-p)
        (progn
          (setq p1 (region-beginning) )
          (setq p2 (region-end) )
          )
      (let ((bds (bounds-of-thing-at-point 'symbol)))
        (setq p1 (car bds) )
        (setq p2 (cdr bds) ) ) )

    (goto-char p2)
    (insert "</" tagName ">")
    (goto-char p1)
    (insert "<" tagName ">")
    ))

;; Some modes, like Python, override my "\C-c\C-r" 'revert-buffer binding
;; (for older emacsen, try py-mode-map instead of python- )
;(defun reset-revert-buffer ()
;  (define-key python-mode-map "\C-c\C-r" 'revert-buffer)
; )
; (setq python-mode-hook 'reset-revert-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Long lines mode: emacs doesn't have a way comparable to
;; vim's "set linebreak" to show long lines as wrapped visually,
;; without changing the file.  longlines-mode adds a minor mode
;; to change the file upon reading it in, then change it back
;; when it's saved.  Ick, but that's apparently all emacs can do.
;; http://www.emacswiki.org/elisp/longlines.el
;(autoload 'longlines-mode
;  "longlines.el"
;  "Minor mode for automatically wrapping long lines." t)

;(setq longlines-wrap-follows-window-size t)
;(setq longlines-show-hard-newlines t)

;; make command completion complete as far as possible, not just first word
;; unfortunately, this doesn't work
;(define-key minibuffer-local-must-match-map " " minibuffer-complete)

;; Recent Emacsen have a bug where the block cursor disappears sometimes,
;; like if you go to the beginning of an existing line and delete backward.
;; Changing to a bar cursor fixes it.  Hope you like a bar cursor!
;; See http://6v8.gamboni.org/Emacs-and-OS-X.html (happens on linux too).
;; (setq initial-frame-alist
;;   (cons '(cursor-type . bar)
;;         (copy-alist initial-frame-alist)
;;    )
;; )
;; (setq default-frame-alist
;;    (cons '(cursor-type . bar)
;;          (copy-alist default-frame-alist)
;;     )
;; )

; (autoload 'pmodify "ptools" "ptools utilities" t)

; Load a better man package
(autoload 'manual-entry "man-background"
	  "Run UNIX man in the background.  When it's finished, a man entry window pops up." t)


;; Balancing parens:
;; <josteink> akk: my favourite way of getting that sorted out is (indent-whole-buffer)
;; <josteink> makes it obvious where the error is introduced when scanning from top to bottom
;; <mr_snowf1ake> akk: rainbow-delimeters also can help
;; <mr_snowf1ake> akk: and i think it might have been auto-highlight-symbols that highlights the matching paren?
;; <josteink> akk: http://pastebin.com/KBa6zhhX
;; <mr_snowf1ake> i have so many plugins haha.. can't keep track of them...
;; <JordiGH> ,plugin
;; <fsbot> Emacs doesn't have plugins, it has elisp packages
;; <mr_snowf1ake> sorry
;; <mr_snowf1ake> i meant packages
;; <c3w> there is show-paren-mode
;; <c3w> I use paredit to balance parens, it's very proactive
;; <c3w> akk: also hl-sexp-mode

; (load-library "paren")

;;; KEYPAD BINDINGS:
;;; Argh!  The names of the keypad keys change with every emacs release!!
;;; Put this at END of .emacs so that if it bombs, at least it won't
;;; put us in error-debug mode.
;;;
;;; Update: I think the most common reason for it bombing is changing
;;; keyboard maps.  Judicious control over the keymap used at boot
;;; time helps this quite a bit.
;;;

;; Is this really required any more? Apparently not.
;;(require 'mwheel)
;;(mwheel-install)

;; Disable obnoxious "Electric" re-indenting in c- and java-modes.
;; http://www.gnu.org/software/emacs/manual/html_mono/ccmode.html
;; XXX But really we probably want this in C mode;
;; it's just in java mode where it doesn't work right,
;; because java and javascript modes are so aggressive about
;; indenting whenever you type a comma, semicolon etc.
;; Sadly, no: it's aggressive in c-mode too. Try putting a //
;; in front of a brace, for example.
;; But if you turn it off, C mode is unusable: when you type a { or }
;; it doesn't go to the right place. Sigh!
;; (setq-default c-electric-flag nil)

;;(defun my-c-mode-hook ()
;;  (no-electric c-mode-map))
;;(defun my-js-mode-hook ()
;;  (no-electric java-mode-map))

;;
;;     ;;(setq font-lock-keywords 
;;     ;;      (append moz-font-lock-keywords 
;;     ;;              c-font-lock-keywords-2))
;;     ;; Turn off obnoxious electric-indent modes:
;;     (define-key c-mode-map ";" 'self-insert-command)
;;     (define-key c-mode-map "/" 'self-insert-command)
;;     (define-key c-mode-map "*" 'self-insert-command)
;;     (define-key c-mode-map "{" 'self-insert-command)
;; ;    (define-key c-mode-map "}" 'self-insert-command)
;;     (define-key c-mode-map "," 'self-insert-command)
;;  ))

;(setq c-mode-hook 'my-c-mode-hook)
;(setq c++-mode-hook 'my-c-mode-hook)


;; Automatically make scripts executable on save.
;; http://www.emacswiki.org/emacs/MakingScriptsExecutableOnSave
;; XXX

;; ;(defun recentf-open-files-compl ()
;; (defun foorecent ()
;;   "Show a menu of recent files ... or something"
;;   (interactive)
;;   (let* ((all-files recentf-list)
;;          (tocpl (mapcar (function
;;                          (lambda (x) (cons (file-name-nondirectory x) x)))
;;                         all-files))
;;          (prompt (append ‘(“File name: “) tocpl))
;;          (fname (completing-read (car prompt) (cdr prompt) nil nil)))
;;     (find-file (cdr (assoc-string fname tocpl)))))
;; ;(global-set-key [(control x)(control r)] ‘recentf-open-files-compl)
;; ;(global-set-key "C-xC-r" ‘recentf-open-files-compl)
;; (global-set-key "C-xC-r" ‘foorecent)

;; ;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
;; ;; get rid of `find-file-read-only' and replace it with something
;; ;; more useful.
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; ;; enable recent files mode.
;; (recentf-mode t)

;; ; 50 files ought to be enough.
;; (setq recentf-max-saved-items 50)

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

; (require 'icicles)
; (icy-mode 1)

;; http://www.emacswiki.org/emacs/AutoEncryption

;; (defvar pgg-gpg-user-id "akkana@shallowsky.com")
;; (autoload 'pgg-make-temp-file "pgg" "PGG")
;; (autoload 'pgg-gpg-decrypt-region "pgg-gpg" "PGG GnuPG")
;; (define-generic-mode 'gpg-file-mode
;;   (list ?#) 
;;   nil nil
;;   '(".gpg\\'" ".gpg-encrypted\\'")
;;   (list (lambda ()
;; 	    (add-hook 'before-save-hook
;;                       (lambda () 
;;                         (let ((pgg-output-buffer (current-buffer)))
;;                           (pgg-gpg-encrypt-region (point-min) (point-max)
;;                                                   (list pgg-gpg-user-id))))
;;                       nil t)
;; 	    (add-hook 'after-save-hook 
;; 		      (lambda ()
;;                         (let ((pgg-output-buffer (current-buffer)))
;;                           (pgg-gpg-decrypt-region (point-min) (point-max)))
;; 			(set-buffer-modified-p nil)
;; 			(auto-save-mode nil))
;; 		      nil t)
;;             (let ((pgg-output-buffer (current-buffer)))
;;               (pgg-gpg-decrypt-region (point-min) (point-max)))
;; 	    (auto-save-mode nil)
;; 	    (set-buffer-modified-p nil)))
;;   "Mode for gpg encrypted files")

