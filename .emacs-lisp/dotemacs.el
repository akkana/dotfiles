;;
;; Akkana's old and grizzled GNU Emacs initialization file
;;

;; Show errors in this file:
(setq debug-on-error t)
(setq stack-trace-on-error t)

(setq load-path (cons "~/.emacs-lisp/" load-path))

;; Automatically uncompress .gz files
;; -- this seems to have stopped working unless I do it by hand.
(auto-compression-mode 1)

;; Disable all version control handling
(setq vc-handled-backends nil)

;; Don't prompt for y-e-s-\n
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; Basic key bindings
;;
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-v" 'find-file-other-window)
(global-set-key "\C-xs" 'save-buffer)
(global-set-key "\M-w" 'kill-region)
(global-set-key "\C-m" 'newline-and-indent)
(global-set-key (kbd "<S-return>") 'newline)
;(global-set-key [C-return] 'newline-and-indent)
(global-set-key "\M-n" 'goto-line)
(global-set-key "\M-N" 'what-line)
(global-set-key "\M-?" 'help-for-help)
(global-set-key "\C-x\C-k" 'kill-buffer)
;; (global-set-key "\M-/" 'apropos)
(global-set-key "\C-c\C-c" 'kill-emacs)
(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key "\C-x\C-i" 'indent-region)
(global-set-key "\C-c\C-i" 'indent-region)

;; Use home/end to go to beginning/end of file, not line;
;; because ^A/^E are easy to hit but M-<> are not
;; (especially on a mini laptop).
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; I keep hitting ^Z accidentally, and at the same time I can
;; never remember what undo is normally bound to.
;; Unfortunately this will suck if I run emacs from the shell,
;; so I'd really like to run this only if we're in GUI mode.
(global-set-key "\C-z" 'undo)

;; Redo mode allows real undo/redo: http://www.emacswiki.org/emacs/RedoMode
(load "redo.el")
(global-set-key "\M-z" 'redo)

;; Make space do what tab does when autocompleting,
;; NOT stopping at punctuation:
(define-key minibuffer-local-completion-map " " 'minibuffer-complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color themes: search for color-theme.el
;(load "color-theme-akk.el")
;(color-theme-akk)
  ;;  C-u C-x = or customize-face will let you customize whatever's at point,
  ;; and will add whatever it is to your .emacs.
  ;; However, it adds it with this caveat:
  ;;   If there is more than one, they won't work right.
  ;; so I don't yet know what happens if you need to change more than one.
  ;; It may be that it's okay as long as you put all your face customizations
  ;; inside this one custom-set-faces call.
  ;;
;(set-background-color "grey90")
; Some decent colors: grey90, Alice Blue, light cyan, mint cream
(set-background-color "light cyan")
(custom-set-faces
 '(flyspell-duplicate ((((class color))
                        (:foreground "red" :underline t :weight bold))))
 '(font-lock-comment-face ((((class color) (min-colors 88)
                             (background light)) (:foreground "blue"))))
 )

(set-face-foreground 'modeline "yellow")
(set-face-background 'modeline "purple")
(set-face-background 'modeline-inactive "light blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turning off annoyances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-progressive-speed nil)

;; I'd rather not force this, since I occasionally edit binary files,
;; but it's just too annoying how emacs asks, then doesn't actually
;; add the newline so I have to go and do it myself.
;; Ideally I should do this only for text and Fundamental modes.
;; But -- sigh -- this doesn't work anyway.
(setq require-final-newline t)

;; My KVM switch uses scroll lock, and emacs complains about it.
;; So silence that. 'noop worked in emacs21 but is gone in emacs22.
;; and this has stopped working again in emacs23.
;(global-set-key [scroll-lock] 'ignore)
(global-set-key (kbd "<Scroll_Lock>") 'ignore)

;; Get rid of keys I hit accidentally:
(global-unset-key "\M-c")    ; don't want the capitalize thing
;; undefine the keys that do narrow-to-page and narrow-to-region,
;; because who would ever want such a stupid function??
(global-unset-key "\C-xp")
(global-unset-key "\C-xn")

;; Recent versions of emacs always blink the cursor.  Yuck!
;(blink-cursor-mode nil)
;(customize-set-variable 'blink-cursor nil) ;; thanks to sachac!
; But that no longer works in emacs 22, so let's try this:
(if (fboundp 'blink-cursor-mode) (blink-cursor-mode 0))

;; Turn off that big useless toolbar
(tool-bar-mode 0)
(menu-bar-mode 0)
;; and the irritating scrollbar that claims to have somewhere to
;; scroll even on new 1-line files:
(toggle-scroll-bar -1)

;; and the mousewheel progressive speed:
(setq mouse-wheel-progressive-speed nil)

;; Emacs's html mode always asks for email address.  Why??
(setq query-user-mail-address nil)

;; xemacs v21 is forever leaving .saves- files with long names
;; which screw up my directory listings.  Make it put them
;; somewhere else:
(setq auto-save-list-file-prefix "~/.emacs-saves/.saves-")
;; or not do it at all (I can ^X^S frequently):
(setq auto-save-default nil)

;; don't paste syntax highlight color into buffers where it's meaningless:
(setq yank-excluded-properties t)

;; make sure the tab width is right:
;(set-variable "tab-width" 8)
;(set-variable "default-tab-width" 8)
;(set-variable "indent-tabs-mode" nil)
(setq-default indent-tabs-mode nil)
(setq tabify nil)

;; Emacs 23 changed up/down behavior, so it goes to the next screen
;; line instead of the next buffer line (on lines long enough to wrap).
;; Revert to old behavior:
(setq line-move-visual nil)

;; Try to tell emacs not to put killed text into the X selection
;(setq interprogram-cut-function nil)
; Unfortunately that also prevents it from selecting highlighted text!
; Need a way to turn off just killed text.

;; Emacs seems to have lost the ability to do middlemouse paste of
;; PRIMARY, apparently because it's sometimes (but not always) taking
;; CLIPBOARD instead.
;; See also http://www.oreillynet.com/onlamp/blog/2005/01/quick_tip_for_linux_users_havi.html
;(setq x-select-enable-clipboard nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End annoyances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq default-case-fold-search nil)
(setq delete-auto-save-files t)
(setq make-backup-files nil)
;; and some extensions for SGI's additions to c++-mode
(setq c++-hanging-member-init-colon t)

;; stop prompting me when I try to edit a link to an svn file
(setq vc-follow-symlinks t)

;(setq default-major-mode 'text-mode)
(setq fill-column 75)

;;; Set the mode format at the bottom
;;; Used to be "%*%*%* " emacs-version " %b  %M %[(%m)%] line=%5l %3p %-"
;(setq default-modeline-format 
;      (list "%*%*%* %b  %M %[(%m)%] line=%5l %3p %-"))
(setq line-number-mode t)
;(setq mode-line-format default-mode-line-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This one works -- thanks to ggole on #emacs
(defun unhtml (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      )))

;; These two don't work -- suggestions from other #emacs folks.
(defun unhtml2 ()
  "Eliminate HTML special characters < > & within the region."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (goto-char (region-beginning))
  (replace-string "&" "&amp;")
  (goto-char (region-beginning))
  (replace-string "<" "&lt;")
  (goto-char (region-beginning))
  (replace-string ">" "&gt;")
  (widen)
)

(defun unhtml3 ()
  "Eliminate HTML special characters < > & within the region."
  (interactive)
  (goto-char (region-beginning))
  ;(while (search-forward "&" (region-end))
  ;  (replace-match "&amp;"))
  (while (search-forward "<" (region-end))
    (replace-match "&lt;"))
  (goto-char (region-beginning))
  (while (search-forward ">" (region-end))
    (replace-match "&gt;"))
  (goto-char (region-beginning))
)

;; Create a basic HTML page template -- I get tired of typing this all the time.
;; Strangely, you have to have some text in the file already --
;; I'm not sure why.
(defun newhtml ()
  "Insert a template for an empty HTML page"
  (interactive)
  (insert "<html>\n"
          "<head>\n"
          "<title></title>\n"
          "</head>\n\n"
          "<body>\n\n"
          "<h1></h1>\n\n"
          "<p>\n\n"
          "</body>\n"
          "</html>\n")
  )

;; Replace Windows/Mac newslines in current buffer.
;; Thanks to slamm for the original!
;; Rewritten by Akkana to handle Mac newlines as well.
;; Note: Mike Tsao says in his weblog that C-x [ENTER] f unix [ENTER]
;; converts DOS line endings to Unix, but it doesn't get rid of the ^M here.
(defun fixm ()
  "Delete ^M from buffer."

  (interactive)
  ;; Remember where we were
  (let ((old-point (point)))

    ;; Move to top
    (set-window-point (get-buffer-window (current-buffer)) 0)

    (replace-string "\r\n" "\n")
    (replace-string "\r" "\n")
    ;; Replace ^M's
    ;; (while (search-forward "\r" nil t)
    ;;   (replace-match "\r\n" "\n")
    ;;   (replace-match "\r" "\n")
    ;;   )

    ;; Move back to the old point
    (set-window-point (get-buffer-window (current-buffer)) old-point)
    )
  )

;; For mac files
(defun showmac () "Show the file with mac newline encoding" (interactive)
  (let ((coding-system-for-read 'mac)) (revert-buffer nil t)))

;; Call lxr on an identifier
;;Select an identifier as the region, then C-c C-l i
(defun lxr-ident (start end) (interactive "r")
  (let ((ident (buffer-substring start end)))
    (shell-command (concat "mozilla-remote --newwin http://lxr.mozilla.org/seamonkey/ident\?i=" ident))))
(global-set-key "\C-c\C-li" 'lxr-ident)

;; Kill All Buffers without prompting.
;; Modified from kill-some-buffers in files.el, which prompts too much.
(defun kill-all-buffers ()
  "Kill all buffers without prompting."
  (interactive)
  (let ((list (buffer-list)))
    (while list
      (let* ((buffer (car list))
             (name (buffer-name buffer)))
        (kill-buffer buffer))
      (setq list (cdr list)))))

;;
;; For composing in emacs then pasting into a word processor,
;; this un-fills all the paragraphs (i.e. turns each paragraph
;; into one very long line) and removes any blank lines that
;; previously separated paragraphs.
;;
(defun wp-munge () "un-fill paragraphs and remove blank lines" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    ;(delete-matching-lines "^$")
    (set-fill-column save-fill-column)
    ))

(defun wp-unmunge () "fill paragraphs and separate them with blank lines"
  (interactive)
  (mark-whole-buffer)
  (replace-regexp "\(.\)$" "\1\n")
  (fill-individual-paragraphs (point-min) (point-max))
  ;;(delete-matching-lines "^$")
  ;;(replace-regexp "^$" "\n")
  )

(defun unfill () "un-fill paragraphs" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    (set-fill-column save-fill-column)
    ))

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

;;
;; Derived C modes, setting different styles for different files.
;;
(define-derived-mode gnu-c-mode c-mode "GNU C mode"
  (c-set-style "gnu"))
(define-derived-mode linux-c-mode c-mode "GNU C mode"
  (c-set-style "linux"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special code for html and text files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Want auto-fill-mode for some text and html files, but not all.
;; So define two derived modes for that, and we'll use auto-mode-alist
;; to choose them based on filename.
(define-derived-mode html-wrap-mode html-mode "HTML wrap mode"
  (auto-fill-mode))
(define-derived-mode text-wrap-mode text-mode "Text wrap mode"
  (auto-fill-mode))

;; Don't fill when hitting return, only on space:
(set-char-table-range auto-fill-chars 10 nil)

;; In text mode, I don't want it auto-indenting for the first
;; line in the file, or lines following blank lines.
;; Everywhere else is okay.
;; XXX This works fine except that it stomps the X selection.
;; XXX Maybe because of (kill-line 0) ?
(defun newline-and-text-indent ()
  "Insert a newline, then indent the next line sensibly for text"
  (interactive)
  (cond
   ;; Beginning of buffer, or beginning of an existing line, don't indent:
   ((or (bobp) (bolp)) (newline))

   ;; If we're on a whitespace-only line,
   ((and (eolp)
         (save-excursion (re-search-backward "^\\(\\s \\)*$"
                                             (line-beginning-position) t)))
    ;; ... delete the whitespace, then add another newline.
    ;; Don't use (kill-line 0) here because that saves the whitespace
    ;; to the kill ring and stomps the X selection.
    (delete-region (line-beginning-position) (line-end-position))
    (newline))

   ;; Else (not on whitespace-only) insert a newline,
   ;; then add the appropriate indent:
   (t (newline-and-indent))
   ;; If the previous set-char-table-range doesn't work to prevent Return
   ;; from indenting the current line, use this instead of previous line:
   ;;(t (insert "\n")
   ;;   (indent-according-to-mode))
   ))

(defun text-indent-hook ()
  (local-set-key "\C-m" 'newline-and-text-indent)
  (flyspell-mode 1)
  (flyspell-buffer)
  )
(setq text-mode-hook 'text-indent-hook)

;;
;; Define keys for inserting tags in HTML mode:
;;
(defun html-hook ()
  (local-set-key "\C-cb" (lambda () (interactive) (sgml-tag "b")))
  (local-set-key "\C-ci" (lambda () (interactive) (sgml-tag "i")))
  (local-set-key "\C-cp" (lambda () (interactive) (sgml-tag "pre")))
  (local-set-key "\C-m" (lambda () (interactive) (insert "\n")))
  (flyspell-mode 1)
  )
(setq sgml-mode-hook 'html-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mixed text and image mode using iimage -- this is so cool!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode text-img-mode text-mode "Image display mode"
  (auto-fill-mode)
  (turn-on-iimage-mode)
  (iimage-mode-buffer t)
  ;(local-set-key "\C-ci" (lambda () (interactive) (iimage-mode-buffer t)))
  (local-set-key "\C-ci" 'refresh-iimages)
  (local-set-key "\C-cs" 'screenshot)
  (local-set-key "\C-cr" 'repeat-screenshot)
  )

;; Call up mypaint to insert a new image
(defun img ()
  "Prompt for a filename, then call up mypaint to create an image"
  (interactive)
  (let ((imgfile (read-string "Filename? " "xxx.jpg" 'my-history)))
    (insert "\nfile://" imgfile "\n" )
    (start-process "mypaint" nil "/usr/bin/mypaint" imgfile)
  ))

;; Call up scrot to insert a new screenshot
;; It would be nice to have autocompletion on this, or some sort of
;; warning preventing replacing existing files.
(defun screenshot ()
 "Prompt for a filename, then call up scrot to create an interactive screenshot"
  (interactive)
  (let* ((imgfile (read-string "Filename? " "img/" 'my-history))
         (imgfile (if (string-match "\\." imgfile)
                      imgfile
                      ;; (mapconcat 'identity '(imgfile "jpg") ".")
                      (concat imgfile ".jpg")
                      ))
         )
    (insert "file://" imgfile "\n" )
    (start-process "scrot" nil "/usr/bin/scrot" "-s" imgfile)
  ))

;; Re-do the current screenshot
(defun repeat-screenshot ()
  "Re-do the screenshot following point"
  (interactive)
  (beginning-of-line)
  (let* ((url (thing-at-point 'line)) ;; original file:// url
         (filename (replace-regexp-in-string "\n" ""
                           (replace-regexp-in-string ".*file://" "" url)))
             ;; filename, with file:// and terminal newline removed
         )
    (start-process "scrot" nil "/usr/bin/scrot" "-s" filename)
    (message "Take new screenshot")
    )
  ;; Would be nice to refresh images automatically,
  ;; but we'd have to wait for the scrot process to exit.
  ;(refresh-iimages)
  )

;; Re-load the cached images.
;; http://vwood.github.com/emacs-images-in-buffer.html
(defun refresh-iimages ()
  "Only way I've found to refresh iimages (without also recentering)"
  (interactive)
  (clear-image-cache nil)
  (iimage-mode nil)
  (iimage-mode t)
  (message "Refreshed images")
  )

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

;; A mode for editing tab-separated tables, or any other file
;; where you want TAB to insert a tab. (You wouldn't think that
;; would be difficult, but it is! indent-tabs-mode doesn't do it,
;; lines inserted at the beginning of the file are still untabified.)
(define-derived-mode tabbed-mode text-mode "Tab separated mode"
  (local-set-key (kbd "TAB") 'self-insert-command)
  )

;; Some modes, like Python, override my "\C-c\C-r" 'revert-buffer binding
;; (for older emacsen, try py-mode-map instead of python- )
(defun reset-revert-buffer ()
  (define-key python-mode-map "\C-c\C-r" 'revert-buffer)
 )
(setq python-mode-hook 'reset-revert-buffer)

; Ruby
(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
(defun ruby-stuff-hook ()
  (local-set-key "\C-m" 'newline-and-text-indent)
  (turn-on-font-lock)
  )
(add-hook 'ruby-mode-hook 'ruby-stuff-hook)

(setq auto-mode-alist
      (cons '("\\.pde$" . c-mode)
      (cons '("\\.ino$" . c-mode)
      (cons '("\\.py$" . python-mode)
      (cons '("\\.rb$" . ruby-mode)
      (cons '("\\.R$" . R-mode)
      (cons '("\\.m$" . octave-mode)
      (cons '("\\.scm$" . scheme-mode)
      (cons '("\\.blx$" . html-wrap-mode)
      (cons '("\\.html$" . html-wrap-mode)
      (cons '("\\.js$" . javascript-mode)
      (cons '("\\.r$" . r-mode)
      (cons '("blogstuff/" . html-wrap-mode)
      (cons '("Docs/gimp/book/notes" . text-wrap-mode)
      (cons '("README" . text-wrap-mode)
;; Book used to be longlines mode, but that was too flaky.
      (cons '("Docs/gimp/book/" . text-wrap-mode)
      (cons '("Docs/classes/" . text-img-mode)
      (cons '("Docs/" . text-wrap-mode)
      (cons '("linux-.*/" . linux-c-mode)
            auto-mode-alist) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq completion-ignored-extensions
      '(".xpt" ".a" ".so" ".o" ".d" ".elc" ".class" "~" ".ckp" ".bak" ".imp" ".lpt" ".bin" ".otl" ".err" ".lib" ".x9700" ".aux" ".elf" ))

;; make command completion complete as far as possible, not just first word
;; unfortunately, this doesn't work
;(define-key minibuffer-local-must-match-map " " minibuffer-complete)

;; This is supposed to prevent the excessive making of local backup files.
;; http://jamesthornton.com/emacs/chapter/emacs_16.html#SEC150
(setq vc-cvs-stay-local nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and related modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TAB in C-mode should always insert a tab
(setq c-tab-always-indent nil)

;; C-mode variables for Britton-Lee coding standards (See bcx's paper)
(setq c-indent-level 4)
(setq c-continued-statement-offset 4)
(setq c-brace-offset -4)
(setq c-brace-imaginary-offset 0)
(setq c-argdecl-indent 0)
(setq c-label-offset -2)

;; C++-mode variables for Netscape and cc-mode.el:
;; see /tools/ns/share/emacs/19.33/lisp/cc-mode.el for more details.
(autoload 'c-add-style "cc-mode" "C++ Editing Mode" t)
(autoload 'c++-mode    "cc-mode" "C++ Editing Mode" t)
(autoload 'c-mode      "cc-mode" "C Editing Mode" t)
(autoload 'objc-mode   "cc-mode" "Objective-C Editing Mode" t)
(autoload 'java-mode   "cc-mode" "Java Editing Mode" t)
(autoload 'archive-mode "arc-mode" "Zip and Jar Mode" t)
(autoload 'iimage-mode "iimage-mode" "Iimage Mode" t)

;; Style stuff for the old c-mode, now outmoded:
;(c-add-style "akkana"
;             '((c-basic-offset . 4)
;               (c-comment-only-line-offset . 0)
;               (c-offsets-alist . ((statement-block-intro . +)
;                                   (knr-argdecl-intro . +)
;                                   (substatement-open . 0)
;                                   (label . 0)
;                                   (statement-cont . +)
;                                   (case-label . 2)
;                                   ))))
;(c-set-style "akkana")

; Here's the new way, for cc-mode:
(setq c-default-style '((java-mode . "java") (other . "stroustrup")))

;;
;; Ah, glorious linux C-mode!  (from val@mnt.edu)
;;
(defun linux-c-mode ()
    "C mode with adjusted defaults for use with the Linux kernel."
   (interactive)
;   (setq tabify t)
   (c-mode)
   (c-set-style "K&R")
   (setq c-basic-offset 8)
   (setq indent-tabs-mode t)
)

(setq octave-block-offset 4)

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

;; adapted from
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)    ; window-system
  (progn
    ;; Always use a width of 80
    (add-to-list 'default-frame-alist (cons 'width 80))
           
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
                 ; was    (- (x-display-pixel-height) 100)
                 ; That produces an int, but with * we must convert
                 ; from float to int with floor.
         (cons 'height (+ 2 (floor (/ (* (x-display-pixel-height) 0.85)
                                      (frame-char-height)))))))))
;; To set initial window position too:
;; (set-frame-position (selected-frame) 10 30)

(set-frame-size-according-to-resolution)

(put 'eval-expression 'disabled nil)

; (autoload 'pmodify "ptools" "ptools utilities" t)

; Load a better man package
(autoload 'manual-entry "man-background"
	  "Run UNIX man in the background.  When it's finished,
a man entry window pops up." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn debugging back off.  Put any questionable code after these lines!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error nil)
(setq stack-trace-on-error nil)

; (load-library "paren")

;;;
;;; KEYPAD BINDINGS:
;;; Argh!  The names of the keypad keys change with every emacs release!!
;;; Put this at END of .emacs so that if it bombs, at least it won't
;;; put us in error-debug mode.
;;;
;;; I think the most common reason for it bombing is changing
;;; keyboard maps.  Judicious control over the keymap used at boot
;;; time helps this quite a bit.
;;;

;;
;; How this works:
;; The keypad arrow keys let you move in the four directions.
;; The middle key (5) is used as a prefix, like ESC.
;; Arrow alone moves one character/line in the indicated direction.
;; Prefix-Arrow moves one "unit": word or page.
;; Prefix-Prefix-Arrow moves as far as possible (end of line, end of buffer).
;;

(defvar middle-key-map nil "")
(defvar double-middle-key-map nil "")

(cond
 ((or (string-match "XEmacs" emacs-version)
      (and (boundp 'emacs-major-version)
	   (or (and
		(= emacs-major-version 19)
		(>= emacs-minor-version 14))
	       (>= emacs-major-version 20))))
;;       (fboundp 'load-options-file))

  ; Arrow keys are already set, no need to set them again.
  ;; Oops, in debian they're not set because all the keypad
  ;; keys have different names.  I love emacs and keyboard maps.
  (global-set-key [kp-8] 'previous-line)
  (global-set-key [kp-2] 'next-line)
  (global-set-key [kp-4] 'previous-character)
  (global-set-key [kp-6] 'next-character)

;;  (global-set-key [kp-next] 're-search-forward)
;;  (global-set-key [kp-prior] 're-search-backward)
;;  (global-set-key [kp-end] 'query-replace)

  (setq middle-key-map (make-sparse-keymap))
  (suppress-keymap middle-key-map t)
;  (global-set-key [kp-5] nil)
  ;(global-set-key (append [kp-4] [kp-5]) 'backward-word)
  (define-key global-map [begin] 'middle-key-prefix)
  (define-key global-map [kp-5] 'middle-key-prefix)
;  (global-set-key [kp-5] 'middle-key-map)
  (fset 'middle-key-prefix middle-key-map)
  (define-key middle-key-map [kp-left] 'backward-word)
  (define-key middle-key-map [kp-right] 'forward-word)
  (define-key middle-key-map [kp-up] 'scroll-down)
  (define-key middle-key-map [kp-down] 'scroll-up)
;;  (define-key middle-key-map [kp-next] 'replace-regexp)
  (define-key middle-key-map [kp-4] 'backward-word)
  (define-key middle-key-map [kp-6] 'forward-word)
  (define-key middle-key-map [kp-8] 'scroll-down)
  (define-key middle-key-map [kp-2] 'scroll-up)

  (setq double-middle-key-map (make-sparse-keymap))
  (suppress-keymap double-middle-key-map t)
  (define-key middle-key-map [kp-begin] 'double-middle-key-prefix)
  (define-key middle-key-map [kp-5] 'double-middle-key-prefix)
  (fset 'double-middle-key-prefix double-middle-key-map)

  (define-key double-middle-key-map [kp-left] 'beginning-of-line)
  (define-key double-middle-key-map [kp-right] 'end-of-line)
  (define-key double-middle-key-map [kp-up] 'beginning-of-buffer)
  (define-key double-middle-key-map [kp-down] 'end-of-buffer)
  (define-key double-middle-key-map [kp-4] 'beginning-of-line)
  (define-key double-middle-key-map [kp-6] 'end-of-line)
  (define-key double-middle-key-map [kp-8] 'beginning-of-buffer)
  (define-key double-middle-key-map [kp-2] 'end-of-buffer)
  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End keypad bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq-default c-electric-flag nil)

;; Earlier attempt to do this:
;; (defun no-electric (keymap)
;;   (progn
;;     (define-key keymap ";" 'self-insert-command)
;;     (define-key keymap "/" 'self-insert-command)
;;     (define-key keymap "*" 'self-insert-command)
;;     (define-key keymap "{" 'self-insert-command)
;; ;    (define-key keymap "}" 'self-insert-command)
;;     (define-key keymap "," 'self-insert-command)
;;  ))

;; But these stopped working, maybe because the names for the maps
;; are wrong. thunk on #emacs points to:
;; ,,df current-local-map and ,,df local-unset-key
;; (add-hook 'c-mode-hook (lambda () (no-electric c-mode-map)))
;; (add-hook 'c++-mode-hook (lambda () (no-electric c-mode-map)))
;; (add-hook 'java-mode-hook (lambda () (no-electric java-mode-map)))

(add-hook 'js-mode-hook (lambda ()
  (define-key js-mode-map "," 'self-insert-command)
  (define-key js-mode-map ";" 'self-insert-command)
 ))

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

;; Match paren.  from http://grok2.tripod.com/
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))
(global-set-key "\C-x%" 'match-paren)

;;
;; tramp-mode is lovely for remote editing, but it sure does take a
;; lot of hand-holding. Thanks, Val.
;;

(setq tramp-debug-buffer t)

;(setq tramp-default-method "scp")
(setq tramp-rcp-program "scp") 

;; Orig nonworking pattern
;;(setq tramp-shell-prompt-pattern
;;  "^[^#$%>\n]*[#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*" )
;;
;; Works, kinda
;;
(setq tramp-shell-prompt-pattern
        "^\e\[[0-9]*[a-z]([a-z\.\')-\e\[[0-9]*[a-z] " )
;;
;; For some reason, sshd on rainbow puts in this extra "Response:"
;line
;;
;;(setq tramp-password-prompt-regexp
;;        "^.*\\([pP]assword\\|Response\\|passphrase.*\\):\^@? *" )

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((flyspell-mode) (encoding . utf-8) (auto-fill-mode) (wrap-mode)))))
(put 'upcase-region 'disabled nil)

;;
;; Some useful tips on testing/evaluating elisp:
;; http://www.masteringemacs.org/articles/2010/11/29/evaluating-elisp-emacs/
;; eval-buffer  eval-region   C-x C-e does eval-last-sexp
;; eval-defun (with point anywhere inside the defun)
;; typing a C-u first invokes edebug which lets you step through, etc.
;; M-; is eval-expression
;; In the scratch buffer, put point at the end of an expression and C-j
;; M-x ielm -- interactive emacs lisp mode
;; which even has some command completion with TAB
(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;;
;; A few other useful elisp tutorials:
;; http://ergoemacs.org/emacs/elisp_basics.html
;; http://cjohansen.no/an-introduction-to-elisp
;; http://ergoemacs.org/emacs/elisp.html
;;

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
