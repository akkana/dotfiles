;;
;; Akkana's ancient and grizzled GNU Emacs initialization file
;;

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Show errors in this file:
(setq debug-on-error t)
(setq stack-trace-on-error t)

; (setq load-path (cons "~/.emacs.d/lisp/" load-path))

;; Automatically uncompress .gz files
;; -- this seems to have stopped working unless I do it by hand.
(auto-compression-mode 1)

;; Disable all version control handling
(setq vc-handled-backends nil)

;; Don't prompt all the time for y e s \n
(fset 'yes-or-no-p 'y-or-n-p)

;; Change window size on smaller screens. Adapted from
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)    ; window-system
  (progn
    ;; Emacs can't accept some fonts via Xdefaults. Here's how to set them here,
    ;; and we'd want to do it differently based on screen size:
    ;(message (x-display-pixel-height))
    ;; (if (<= (x-display-pixel-height) 768)
    ;;     ; Good on a small monitor
    ;;     ; This works but isn't bold
    ;;     ;(set-default-font "Inconsolata-12:bold")
    ;;     ; This isn't found at all, though it works for xterm in Xdefaults:
    ;;     (set-default-font "-*-clean-bold-r-*-*-13-*-*-*-c-*-*-*")
    ;;     ; Prettier on a larger monitor:
    ;;     (set-default-font "-misc-fixed-bold-r-normal-*-14-*-*-*-*-*-*-*")
         (set-default-font "-misc-fixed-bold-r-normal-*-14-*-*-*-*-*-*-*")
    ;;     )

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
         (cons 'height (+ 4 (floor (/ (* (x-display-pixel-height) 0.85)
                                      (frame-char-height)))))))))
;; To set initial window position too:
;; (set-frame-position (selected-frame) 10 30)

(set-frame-size-according-to-resolution)

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
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cC" 'uncomment-region)

;; Lately exchange-point-and-mark selects everything in between the two points,
;; clobbering anything in the primary selection.
;; But you can turn that off by passing t to activate "Transient Mark mode".
(global-set-key "\C-x\C-x" (lambda () (interactive) (exchange-point-and-mark t)))

;; I am forever hitting this by accident, when my finger slips off
;; Ctrl-z and grazes C-x at the same time. Disable it:
(global-unset-key "\C-x\C-z")

;; I seem to spend half my free time chasing after various broken
;; electric indents in emacs. And really, the only time I ever want
;; electric indent is for }. So maybe the answer is to turn off
;; electrics everywhere, then rebind } to indent the current line
;; after inserting.
(setq electric-indent-mode nil)

;; Electric mode has recently taken over (newline), so we have to do this:
(if (fboundp 'electric-indent-just-newline)
    (global-set-key (kbd "<S-return>") 'electric-indent-just-newline)
    (global-set-key (kbd "<S-return>") 'newline))

(global-set-key "\M-n" 'goto-line)
(global-set-key "\M-N" 'what-line)
(global-set-key "\M-?" 'help-for-help)
(global-set-key "\C-x\C-k" 'kill-buffer)
;; (global-set-key "\M-/" 'apropos)
(global-set-key "\C-c\C-c" 'kill-emacs)
(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-x\C-i" 'indent-region)

(global-set-key "\C-x%" 'match-paren)

;; Use home/end to go to beginning/end of file, not line;
;; because ^A/^E are easy to hit but M-<> are not
;; (especially on a mini laptop).
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set a few key bindings that override everything and can't get
;; overridden by minor modes.
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
;; but that doesn't have all the details.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar global-keys-minor-mode-map (make-sparse-keymap)
  "global-keys-minor-mode keymap.")

(define-key global-keys-minor-mode-map "\C-c\C-r" 'revert-buffer)
(define-key global-keys-minor-mode-map (kbd "C-;") 'insert-date)
(define-key global-keys-minor-mode-map (kbd "C-:") 'insert-yesterday-date)

;; Try to disable electric mode in python, but this doesn't work:
;(define-key global-keys-minor-mode-map (kbd "C-:") 'self-insert)

(define-minor-mode global-keys-minor-mode
  "A minor mode so that global key settings override annoying major modes."
  t "" 'global-keys-minor-mode-map)
;; name is set to "" since it gets shoved into the modeline for everything.

(global-keys-minor-mode 1)

;; A keymap that's supposed to be consulted before the first
;; minor-mode-map-alist.
(defconst global-minor-mode-alist (list (cons 'global-keys-minor-mode
                                              global-keys-minor-mode-map)))
;; Next line used to use setf but emacs23 doesn't have setf.
(setq emulation-mode-map-alists '(global-minor-mode-alist))

;; Not sure if this part is actually needed.
;; It might depend on what key bindings are in global-keys-minor-mode-map.
(defun my-minibuffer-setup-hook ()
  (global-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;; end global-keys code ;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(if (>= emacs-major-version 24)
;    (progn
      (require 'package)
      (add-to-list 'package-archives
                   '("melpa-stable" . "https://stable.melpa.org/packages/"))
      (package-initialize)
;      ))

;;;;;;;;;;;;;;;;;;;;;;;;;; Undo handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I keep hitting ^Z accidentally, and at the same time I can
;; never remember what undo is normally bound to.
;; Unfortunately this will suck if I run emacs from the shell,
;; so I'd really like to run this only if we're in GUI mode.
;; (if (display-graphic-p)
;;   (global-set-key "\C-z" 'undo))

;; Redo mode allows real undo/redo: http://www.emacswiki.org/emacs/RedoMode
;(load "redo.el")
;(global-set-key "\M-z" 'redo)
;(global-set-key "\C-z" 'undo)
;(global-set-key "\M-z" 'redo)

;; Supposedly undo-tree is better.
;; To use it, install it using emacs' built-in package system,
;; since it's not shipped with emacs nor available in debian:
;; M-x package-install undo-tree
;; Then use package-initialize to enable it: require doesn't work
;; with emacs-installed packages. Then you have to read the comments
;; at the beginning of undo-tree.el to figure out how to use it since
;; there's no online documentation. *eyeroll*
(if (>= emacs-major-version 24)
    (progn
        ; (package-initialize)  ; Already done earlier
        (undo-tree-mode 1)
        (global-set-key "\C-z" 'undo-tree-undo)
        (global-set-key "\M-z" 'undo-tree-redo)
        )
    (progn
        (load "redo.el")
        (global-set-key "\C-z" 'undo)
        (global-set-key "\M-z" 'redo)
        ) )
;;;;;;;;;;;;;;;;;;;;;;;; end Undo ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make space do what tab does when autocompleting,
;; NOT stopping at punctuation:
(define-key minibuffer-local-completion-map " " 'minibuffer-complete)

;; better handling of duplicate filenames
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;(setq default-major-mode 'text-mode)
(setq fill-column 75)

;;; Set the mode format at the bottom
;;; Used to be "%*%*%* " emacs-version " %b  %M %[(%m)%] line=%5l %3p %-"
;(setq default-modeline-format
;      (list "%*%*%* %b  %M %[(%m)%] line=%5l %3p %-"))
(setq line-number-mode t)
;(setq mode-line-format default-mode-line-format)

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
; Some decent colors: grey90, Alice Blue, light cyan, mint cream, Honeydew
;(set-background-color "mint cream")
(set-background-color "#e9fffa")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((((class color)) (:foreground "red" :underline t :weight bold))))
 '(font-lock-comment-face ((((class color) (min-colors 88) (background light)) (:foreground "blue"))))
 '(whitespace-trailing ((t (:background "cyan" :foreground "yellow" :weight bold)))))

(set-face-foreground 'mode-line "yellow")
(set-face-background 'mode-line "purple")
(set-face-background 'mode-line-inactive "light blue")

(set-face-background 'trailing-whitespace "yellow")

(set-face-attribute 'region nil :background "#8df" :foreground "black")

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
;; And they're easy to hit accidentally.
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

;; make sure the tab width is right:
;(set-variable "tab-width" 8)
;(set-variable "default-tab-width" 8)
;(set-variable "indent-tabs-mode" nil)
(setq-default indent-tabs-mode nil)
(setq tabify nil)

;; Highlight trailing whitespace.
;; This may be too annoying on files edited by mac/win people.
(setq-default show-trailing-whitespace t)

;; Also show tabs. https://www.emacswiki.org/emacs/ShowWhiteSpace
(defface extra-whitespace-face
  '((t (:background "honeydew2")))
  "Used for tabs and such.")
(defvar bad-whitespace
  '(("\t" . 'extra-whitespace-face)))

;; Emacs 23 changed up/down behavior, so it goes to the next screen
;; line instead of the next buffer line (on lines long enough to wrap).
;; Revert to old behavior:
(setq line-move-visual nil)
;;;;;;;;;;;;; X Selection / Clipboard behavior

;; This may be helpful: http://www.emacswiki.org/emacs/Comments_on_CopyAndPaste

;; ;; With these two:
;; ;(setq x-select-enable-clipboard t)
;; ;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; ;; text in PRIMARY can be pasted into emacs with middleclick but not with ^Y.

;; ;; With these three:
;; ;(setq x-select-enable-clipboard t)
;; ;(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; ;(setq x-select-enable-primary t)
;; ;; C-y will also paste PRIMARY.

;; ;; Copy selected text into X CLIPBOARD selection as well as PRIMARY
;; ;; so clueless new apps (like GIMP now, sigh, bug 730315) can use it.
;; (setq x-select-enable-clipboard t)
;; ;; I'm not clear what this one does, but maybe it'll help emacs be less flaky:
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; ;; or maybe this will -- except that would also put it in the kill ring:
;; ;; (setq mouse-drag-copy-region t)

;; ;; Non-nil means cutting and pasting uses the primary selection.
;; ;; But unfortunately it disables x-select-enable-clipboard.
;; (setq x-select-enable-primary t)

;; ;; Save only temporarily active regions to the primary selection
;; (setq select-active-regions 'only)

;; ;; Try to tell emacs not to put killed text into the X selection
;; ;(setq interprogram-cut-function nil)
;; ; Unfortunately that also prevents it from selecting highlighted text!
;; ; Need a way to turn off just killed text.

;; ;; Emacs seems to have lost the ability to do middlemouse paste of
;; ;; PRIMARY, apparently because it's sometimes (but not always) taking
;; ;; CLIPBOARD instead.
;; ;; See also http://www.oreillynet.com/onlamp/blog/2005/01/quick_tip_for_linux_users_havi.html
;; ;(setq x-select-enable-clipboard nil)

(if (and (boundp 'emacs-major-version) (< emacs-major-version 25))

    ;; emacs24: What wgreenhouse on #emacs uses.
    (progn
      (setq x-select-enable-primary t)
      (setq x-select-enable-clipboard t)
      (setq save-interprogram-paste-before-kill t)
    )

    ;; emacs25:
    ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Clipboard.html
    (progn
      (setq select-enable-clipboard nil)
      (setq select-enable-primary t)
      (setq mouse-drag-copy-region t)
    )
)

;; Around 24.5, emacs developed the annoying habit that every time I
;; switch into a buffer, it primary-selects whatever region is active.
;; Nobody seems to know how to turn this off, so instead, bind C-x b
;; to something that eliminates any active region before switching out.
;; This is a much better alternative to the more drastic solution:
;; (transient-mark-mode 0)
;;
;; Unfortunately we need to do this for every way of switching buffers:
;;     C-x b   (switch-to-buffer)
;;     C-x C-f (find-file)

(defun delesect-then (what-then)
  "Deselect any region in the current buffer, then call something else"
  (deactivate-mark)
  ;; deactivate-mark leaves us at the region's end.
  ;; It might be nicer to end up at the region's beginning.
  ;; This doesn't work -- sets it to the end (why?) --
  (if (region-active-p) (push-mark (region-beginning)))
  ;;(if (region-active-p) (push-mark (region-end)))
  (call-interactively what-then)
  )

(global-set-key "\C-xb"
                (lambda () (interactive) (delesect-then 'switch-to-buffer)))
(global-set-key "\C-x\C-f"
                (lambda () (interactive) (delesect-then 'find-file)))

;; don't paste syntax highlight color into buffers where it's meaningless.
;; In emacs 25 this seems to work, but it doesn't work in emacs 24.
(add-to-list 'yank-excluded-properties 'font)
(add-to-list 'yank-excluded-properties 'font-lock-face)

;; In emacs24, yank-excluded-properties doesn't work.
;; However, this works for Ctrl-Y:
(if (and (boundp 'emacs-major-version)
         (< emacs-major-version 25))

    (progn
      (defun yank-without-colors () (interactive)
             (insert (substring-no-properties
                      (x-selection 'PRIMARY))))
      (global-set-key "\C-y" 'yank-without-colors)

      ;; Unfortunately there seems to be no way in emacs24 to get
      ;; middlemouse to paste without colors.
      ;; You can try things like this:
      ;;
      ;; (defun mouse-paste-without-colors (e)
      ;;   (interactive "e")
      ;;   (goto-char (posn-point (event-start e)))
      ;;   (insert (substring-no-properties (x-selection 'PRIMARY)))
      ;;   )
      ;; (global-set-key (kbd "<mouse-2>") 'mouse-paste-without-colors)
      ;;
      ;; but then something happens after the paste that stomps the
      ;; primary X selection and replaces it with a bunch of other crap.
      ;; That happens in emacs25 too, but fortunately emacs25 does
      ;; the right thing with yank-excluded-properties
      ;; so we don't need these rebindings.
      ;; Nobody seems to know why this is happening.
      ;; There's something called <down-mouse-2> but it happens
      ;; before <mouse-2>, not after, and binding it to 'nop doesn't help.
      ;; There's no <up-mouse-2>.
    )
)

; You can use this to turn off colors on a block of text:
(defun decolorize () (interactive)
  (set-text-properties (point) (mark) nil))

(defun decolorize2 () (interactive)
  (remove-text-properties (point) (mark)))

;;;;;;;;;;;;; End X Selection / Clipboard behavior

;(setq default-case-fold-search nil)
(setq delete-auto-save-files t)
(setq make-backup-files nil)
;; and some extensions for SGI's additions to c++-mode
(setq c++-hanging-member-init-colon t)

;; stop prompting me when I try to edit a link to an svn file
(setq vc-follow-symlinks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; End annoyances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-insert for various modes
;;
;; auto-insert is cool but the documentation is terrible.
;; An auto-insert stanza goes like this:
;; (add-to-list 'auto-insert-alist
;;              '(("\\.c$" . "C Program")
;;                "Parameter"
;;                "strings"
;; ))
;; If "Parameter" is non-nil, it will be used as a prompt, and whatever
;; you type in response to the prompt will be inserted wherever you
;; have "str" in the auto-insert data.
;; An underscore _ says to put the cursor there after inserting.
;; > indents to the current indent level according to the mode.
;; Some other variables and functions that might be useful to include:
;; (file-name-nondirectory (file-name-sans-extension buffer-file-name))
;; (substring (current-time-string) -4)
;; (user-full-name)
;;
;; You can also define autoinsert using define-auto-insert
;; but the syntax is a bit more opaque.
;;
;; https://www.emacswiki.org/emacs/AutoInsertMode
;; https://www.gnu.org/software/emacs/manual/html_node/autotype/Skeleton-Language.html#Skeleton-Language
;; https://www.emacswiki.org/emacs/SkeletonMode

(auto-insert-mode)

;; Auto-insert mode has tons of defaults and it's hard to get a list
;; of even what they are. I don't want auto-insert unless I've defined it.
(setq auto-insert-alist '())

;; Don't prompt before every auto-insertion:
(setq auto-insert-query nil)

(add-to-list 'auto-insert-alist
             '(python-mode
               nil
               "#!/usr/bin/env python3\n"
               "\n"
               _ "\n"
               "\n"
               "if __name__ == '__main__':\n"
               > "\n\n"
))

(add-to-list 'auto-insert-alist
             '(("\\.html$" . "HTML file")
               "Title: "
               "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"
               "<html>\n"
               "<head>\n"
               "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n"
               "<title>" str "</title>\n"
               "</head>\n\n"
               "<body>\n\n"
               "<h1>" str "</h1>\n\n"
               "<p>\n"
               _ "\n\n"
               "</body>\n"
               "</html>\n"
))

(add-to-list 'auto-insert-alist
             '((".*lwvweb.*\\.html$" . "LWVNM HTML file")
               "Title: "
               "<?php\n"
               "  $title = \"" str "\";\n"
               "\n"
               "  require ($_SERVER['DOCUMENT_ROOT'] . \"/php/header.php\");\n"
               "?>\n"
               "\n"
               "<p>\n"
               _ "\n"
               "\n"
               "<?php\n"
               "require ($_SERVER['DOCUMENT_ROOT'] . \"/php/footer.php\");\n"
               "?>\n"
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Match paren.  from http://grok2.tripod.com/
;; bind to C-x %
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
	((looking-at "\\s\)") (forward-char 1) (backward-list 1))
	(t (self-insert-command (or arg 1)))))

(defun fixm ()
  "Change line breaks to Unix style. This no longer works because emacs is all tolerant now of DOS line endings."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t)
  (replace-string "\r" "")
  ;(message "fixm")
)

;; A fix for DOuble CApitals from a slow left pinky.
;; Emacs is wonderful. :-)
;; https://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type/13975
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to Single Capitals as you type."
  :init-value nil
  :lighter (" dubcaps")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

;; Kill all Buffers without prompting.
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
  (if (not (use-region-p)) (mark-whole-buffer))
  (let ((save-fill-column fill-column)
        (rstart           (region-beginning))
        (rend             (region-end)))

    ;(message (format "beginning: %s, end: %s" rstart rend))
    ;(sleep-for 3)

    ; For all short lines, add an extra line break after them.
    ; This should get a lot of things like list items, titles etc.
    (replace-regexp "^\\(.\\{1,45\\}\\)$" "\\1\n" nil rstart rend)

    ; Lines that start with - or * or something like 1. or a.
    ; followed by a space followed by more content are list lines, and
    ; have to be separated by blank lines, otherwise fill-* will merge them.
    (replace-regexp "^\\s-*\\(-\\|\\*\\|[1-9a-zA-z]+\\.\\)\\s-+\\(.+\\)$"
                    "\n\\1 \\2" nil rstart rend)

    ; Fill all paragraphs
    (set-fill-column 1000000)
    (fill-individual-paragraphs rstart rend)

    ;; Now try to eliminate some of those extra lines we added earlier.

    ; Remove those blank lines we added after short lines.
    (replace-regexp "^\\(.\\{1,45\\}\\)\n\n" "\\1\n" nil rstart rend)

    ; Replace runs of more than one blank line with a single one
    (replace-regexp "\n\\{3,\\}" "\n\n" nil rstart rend)

    ; Remove extra newline after long list lines:
    (replace-regexp "^\\s-*\\(-\\|\\*\\|[1-9a-zA-z]+\\.\\)\\s-+\\(.\\{46,\\}\\)\n\n"
                    "\\1 \\2\n" nil rstart rend)

    ; Deletion of blank lines currently disabled
    ;(delete-matching-lines "^$")

    ; restore the previous fill column
    (set-fill-column save-fill-column)
))

(defun wp-unmunge () "fill paragraphs and separate them with blank lines"
  (interactive)
  (if (not (use-region-p)) (mark-whole-buffer))
  (replace-regexp "\\(.$\\)" "\\1\n" nil (region-beginning) (region-end))
  (fill-individual-paragraphs (region-beginning) (region-end))

  ;; This sometimes ends up with doubled blank lines, so:
  (goto-char 1)
  (replace-regexp "\n\n+" "\n\n")
  )

(defun unfill () "un-fill paragraphs" (interactive)
  (let ((save-fill-column fill-column))
    (set-fill-column 1000000)
    (mark-whole-buffer)
    (fill-individual-paragraphs (point-min) (point-max))
    (set-fill-column save-fill-column)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special code for html and text files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq sentence-end-double-space nil)

;; Want auto-fill-mode for some text and html files, but not all.
;; So define two derived modes for that, and we'll use auto-mode-alist
;; to choose them based on filename.
(define-derived-mode html-wrap-mode html-mode "HTML wrap mode"
  "HTML mode, plus autofill and flyspell"

  (message "HTML wrap mode")

  (auto-fill-mode)
  (flyspell-mode 1)
  (flyspell-buffer)

  ;; New annoyance in emacs24: every time you save an html file,
  ;; it calls a browser on it, replacing whatever's in your current
  ;; browser window.
  (html-autoview-mode -1)
  )

;;  (if (string-match (buffer-local-value 'major-mode (current-buffer))
;;                    "html-mode")
;;  (if (eq (buffer-local-value 'major-mode (current-buffer)) 'html-mode)

(define-derived-mode text-wrap-mode text-mode "Text wrap mode"
  "Text mode, plus autofill and flyspell"
  (auto-fill-mode)
  (flyspell-mode 1)
  (flyspell-buffer)
  )

;; Don't autofill when hitting return, only on space:
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

(defun text-hook ()
  (local-set-key "\C-m" 'newline-and-text-indent)
  ; Initializing flyspell on a large buffer takes forever -- like, MINUTES.
  ;; So, only use it when we really need it.
  ;(flyspell-mode 1)
  ;(flyspell-buffer)
  (local-set-key (kbd "C-;") 'insert-date)
  (global-set-key (kbd "C-;") 'insert-date)
  (local-set-key (kbd "C-:") 'insert-yesterday-date)
  (global-set-key (kbd "C-:") 'insert-yesterday-date)
  (dubcaps-mode t)
  )

(add-hook 'text-mode-hook 'text-hook)

;;
;; html-mode definitions:
;;

;; Don't insert a newline around <code> and </code> tags.
;; This worked when I first tested it, but now isn't working for any tag.
;(add-to-list 'html-tag-alist '("code"))

;; I give up. I've wasted dozens of hours finding ways around newlines
;; inserted by (sgml-tag) in the sgml-mode based html-mode,
;; only to have it break again in the next version.
;; And anyway, whatever smarts (sgml-tag) offers beyond just inserting the
;; tag is stuff I don't use. So let's just write something simple that
;; works without prompting or inserting a bunch of extra crap.
(defun insert-html-tag (tag) (interactive)
  (let (
        (rstart (if (region-active-p) (region-beginning) (point)))
        (rend   (if (region-active-p) (region-end)       (point))))
    ;; Insert the close tag first, because inserting the open tag
    ;; will mess up the rend position.
    (goto-char rend)
    (insert "</")
    (insert tag)
    (insert ">")
    ;; Now the open tag:
    (goto-char rstart)
    (insert "<")
    (insert tag)
    (insert ">")
))

;; Key bindings and such can be done in the mode hook.
(defun html-hook ()
  ;; Define keys for inserting tags in HTML mode:
  (local-set-key "\C-cb" (lambda () (interactive) (insert-html-tag "b")))
  (local-set-key "\C-ci" (lambda () (interactive) (insert-html-tag "i")))
  (local-set-key "\C-cp" (lambda () (interactive) (insert-html-tag "pre")))
  (local-set-key "\C-cc" (lambda () (interactive) (insert-html-tag "code")))
  (local-set-key "\C-c1" (lambda () (interactive) (insert-html-tag "h1")))
  (local-set-key "\C-c2" (lambda () (interactive) (insert-html-tag "h2")))
  (local-set-key "\C-c3" (lambda () (interactive) (insert-html-tag "h3")))
  (local-set-key "\C-c4" (lambda () (interactive) (insert-html-tag "h4")))

  ;(local-set-key "\C-m" (lambda () (interactive) (insert "\n")))

  ;; browse-url-of-buffer is on C-c C-v. Set it to quickbrowse:
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "quickbrowse")
  ;; More info, like how to write functions to do tabs or reload:
  ;; https://www.emacswiki.org/emacs/BrowseUrl#toc5

  ;; And finally, a generic shorthand to use with other tags:
  ;; Consider changing this to use insert-html-tag instead.
  (local-set-key "\C-ct"  (lambda () (interactive) (sgml-tag)))

  ;; Contents of <pre> tags get reindented, destroying their formatting.
  ;; You can avoid that by not inserting a newline, same as with <code>,
  ;; But better is to turn off indenting entirely in html-mode,
  ;; which is fine with me. 'ignore is elisp's nop function.
  (setq indent-line-function 'ignore)

  ;; Would be nice if this would fix the horked dash handling in sgml-mode,
  ;; but alas it has zero effect that I can find.
  ;;(setq sgml-specials nil)

  ;; Turn off flyspell; we'll turn it on only in html-wrap mode.
  ;(flyspell-mode 0)

  ;(message "Ran html-hook")
  ;(sleep-for 3)
  )
(add-hook 'sgml-mode-hook 'html-hook)

; Prevent -- dashed comments -- from screwing up auto-fill mode in sgml-mode.
; This is an ongoing arms race, breaks with every new emacs version.
(defun sgml-comment-indent-new-line (&optional soft)
  (save-excursion (forward-char -1) (delete-horizontal-space))
  (delete-horizontal-space)
  (newline-and-indent))
  ;(comment-indent-new-line soft))

;; Run this on a buffer inside a <pre> to convert chars like < into entities.
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

;; A mode for editing tab-separated tables, or any other file
;; where you want TAB to insert a tab. (You wouldn't think that
;; would be difficult, but it is! indent-tabs-mode doesn't do it,
;; lines inserted at the beginning of the file are still untabified.)
(define-derived-mode tabbed-mode text-mode "Tab separated mode"
  (local-set-key (kbd "TAB") 'self-insert-command)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;; https://github.com/jrblevin/markdown-mode
;; https://leanpub.com/markdown-mode/read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq markdown-command "/usr/bin/markdown_py")

(defun markdown-hook ()
  (flyspell-mode 1)
  (auto-fill-mode)
  )
(add-hook 'markdown-mode-hook 'markdown-hook)

(custom-set-faces
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.7))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face :background "gainsboro"))))
 '(markdown-link-face ((t (:inherit link))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face :background "gainsboro")))))

;; Customize lists
;; https://emacs.stackexchange.com/questions/723/how-can-i-use-the-se-flavor-of-markdown-in-emacs/761#761
(defvar endless/bullet-appearance
  (propertize (if (char-displayable-p ?•) "  •" "  *")
              'face 'markdown-list-face)
  "String to be displayed as the bullet of markdown list items.")

(require 'rx)
(defvar endless/markdown-link-regexp
    "\\[\\(?1:[^]]+\\)]\\(?:(\\(?2:[^)]+\\))\\|\\[\\(?3:[^]]+\\)]\\)"
  "Regexp matching a markdown link.")

(font-lock-add-keywords
 'markdown-mode
 '(("^ *\\(\\*\\|\\+\\|-\\|\\) "
    1 `(face nil display ,endless/bullet-appearance) prepend)
   (endless/markdown-link-regexp
    1 '(face nil display "") prepend))
 'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert the current date into the buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://ergoemacs.org/emacs/elisp_datetime.html
(defun insert-date (&optional time)
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  ;(insert (format-time-string "%Y-%m-%d"))
  (if (null time) (setq time (current-time)))
  ; (insert (format "%s" time))
  (insert (format-time-string "%Y-%m-%d" time))
  )

;; Yesterday's date, from http://emacswiki.org/emacs/Journal
(defun yesterday-time ()
"Provide the date/time 24 hours before the time now in the format of current-time."
  (setq
   now-time (current-time)              ; get the time now
   hi (car now-time)                    ; save off the high word
   lo (car (cdr now-time))              ; save off the low word
   msecs (nth 2 now-time)               ; save off the microseconds
   psecs (nth 3 now-time)               ; save off the picoseconds
   )

  (if (< lo 20864)                      ; if the low word is too small
      (setq hi (- hi 2)  lo (+ lo 44672)) ; take 2 from high word, add to low
    (setq hi (- hi 1) lo (- lo 20864))  ; else, add 86400 seconds (in two parts)
    )
  (list hi lo msecs psecs)              ; regurgitate the new values
  )                                     ; end of yesterday-time

(defun insert-yesterday-date ()
  "Insert yesterday's date yyyy-mm-dd."
  (interactive)
  (insert-date (yesterday-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text colors/styles. You can use this in conjunction with enriched-mode.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rich-style will affect the style of either the selected region,
;; or the current line if no region is selected.
;; style may be an atom indicating a rich-style face,
;; e.g. 'italic or 'bold, using
;;   (put-text-property START END PROPERTY VALUE &optional OBJECT)
;; or a color string, e.g. "red", using
;;   (facemenu-set-foreground COLOR &optional START END)
;; or nil, in which case style will be removed.
(defun rich-style (style)
  (let* ((start (if (use-region-p)
                    (region-beginning) (line-beginning-position)))
         (end   (if (use-region-p)
                    (region-end)  (line-end-position))))
    (cond
     ((null style)      (set-text-properties start end nil))
     ((stringp style)   (facemenu-set-foreground style start end))
     (t                 (add-text-properties start end (list 'face style)))
     )))

(defun enriched-mode-keys ()
  (define-key enriched-mode-map "\C-ci"
    (lambda () (interactive)    (rich-style 'italic)))
  (define-key enriched-mode-map "\C-cB"
    (lambda () (interactive)    (rich-style 'bold)))
  (define-key enriched-mode-map "\C-cu"
    (lambda () (interactive)    (rich-style 'underline)))
  (define-key enriched-mode-map "\C-cr"
    (lambda () (interactive)    (rich-style "red")))
  (define-key enriched-mode-map "\C-cb"
    (lambda () (interactive)    (rich-style "blue")))
  (define-key enriched-mode-map "\C-cg"
    (lambda () (interactive)    (rich-style "sea green")))
  (define-key enriched-mode-map "\C-cp"
    (lambda () (interactive)    (rich-style "purple")))

  (define-key enriched-mode-map (kbd "C-c <backspace>")
    (lambda () (interactive)    (rich-style nil)))
  )
(add-hook 'enriched-mode-hook 'enriched-mode-keys)

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
  (local-set-key "\C-cp" 'mypaint)

  ;; Our usual font is terrible for Greek letters.
  ;; For courses that require a lot of greek-letter equations,
  ;; use a different font.
  ;; 11x18 or sony 8x16 (those two look the same), Default is nice and bold
  ;; 8x13 is a little small and spidery but has great Greek and fits in
  ;; the normal window size, 9x15 isn't bad but doesn't actually fit
  (setq buffer-face-mode-face '(:family "DejaVu Sans" :height 105 :width semi-condensed))
  (buffer-face-mode)
  )

(defun get-and-insert-image (progname imgdir flags)
  "Prompt for a filename, prepend img/ and append .jpg if needed, then insert a URL for it into the buffer and run a program"
  (interactive)
  (let* ((imgfile (read-string "Filename? " imgdir 'my-history))
         (imgfile (if (string-match "\\." imgfile)
                      imgfile
                      ;; (mapconcat 'identity '(imgfile "jpg") ".")
                      (concat imgfile ".jpg")
                      ))
         (excpath (concat "/usr/bin/" progname))
         )
    ; (message (format "filename: %s, progname: %s" imgfile progname))
    ; (sleep-for 3)
    (when (not (file-exists-p imgdir))
      ;; Make the img directory if it's not there yet
      (make-directory imgdir t))
    (insert "file://" imgfile "\n" )

    ;; (if flags
    ;;     (start-process progname nil excpath flags imgfile)
    ;;     (start-process progname nil excpath imgfile))

    ;; Make a list of the arguments, including flags only if it's non-nil.
    (let ((arglist (if flags
                       (list progname nil excpath flags imgfile)
                       (list progname nil excpath imgfile))))
      (apply 'start-process arglist)
    )))

;; Call up mypaint to insert a new image
(defun mypaint ()
  "Prompt for a filename, then call up mypaint to create an image"
  (interactive)
  (get-and-insert-image "mypaint" "img/" nil))

;; Call up scrot to insert a new screenshot
;; It would be nice to have autocompletion on this, or some sort of
;; warning preventing replacing existing files.
(defun screenshot ()
 "Prompt for a filename, then call up scrot to create an interactive screenshot"
  (interactive)
  (get-and-insert-image "scrot" "img/" "-s"))

;; Call up scrot to insert a new screenshot
;; It would be nice to have autocompletion on this, or some sort of
;; warning preventing replacing existing files.
(defun screenshot-old ()
 "Prompt for a filename, then call up scrot to create an interactive screenshot"
  (interactive)
  (let* ((imgdir "img/")
         (imgfile (read-string "Filename? " imgdir 'my-history))
         (imgfile (if (string-match "\\." imgfile)
                      imgfile
                      ;; (mapconcat 'identity '(imgfile "jpg") ".")
                      (concat imgfile ".jpg")
                      ))
         )
    (when (not (file-exists-p imgdir))
      ;; Make the img directory if it's not there yet
      (make-directory imgdir t))
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
    ;(message (concat "scrot -s" filename))
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
;; Coding helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make Python files executable. It would be nice if this ran
;; only on Python files, but I haven't figured out how.
;; Hopefully it doesn't take too long to do it everywhere.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Mode for editing HTML/PHP files. It's not great but better than nothing.
(load "web-mode")

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;; This used to work:
  (setq web-mode-code-indent-offset 2)

  ;; Disable indentation for HTML and CSS, while keeping it for PHP and JS.
  ;; Unfortunately web-mode often ignores this.
  (setq-local web-mode-markup-indent-offset 0)
  (setq-local web-mode-css-indent-offset 0)
  ;; The possible indent variables:
  ;; coffee-tab-width              ; coffeescript
  ;; javascript-indent-level       ; javascript-mode
  ;; js-indent-level               ; js-mode
  ;; web-mode-markup-indent-offset ; web-mode, html tag in html file
  ;; web-mode-css-indent-offset    ; web-mode, css in html file
  ;; web-mode-code-indent-offset   ; web-mode, js code in html file
  ;; css-indent-offset             ; css-mode

)
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; Turn off auto-indentation in web mode: it does all kinds of crazy
;; things and makes HTML difficult to edit:
;; Unfortunately this may also turn off PHP indentation.
;; But no worries about that, it doesn't work anyway.
;; (setq web-mode-enable-auto-indentation nil)

(defun indent-whole-buffer ()
      "indent whole buffer and untabify it"
      (interactive)
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))

(setq octave-block-offset 4)

; Ruby
(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
(defun ruby-stuff-hook ()
  (local-set-key "\C-m" 'newline-and-text-indent)
  (turn-on-font-lock)
  )
(add-hook 'ruby-mode-hook 'ruby-stuff-hook)

(defun archive-hook ()
  (flyspell-mode 0)
  )
(add-hook 'archive-mode-hook 'archive-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attempts to turn off "electric" code reindenting.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable obnoxious "Electric" re-indenting in c- and java-modes
;; (and now, python too).
;; It's useful on some characters, but awful when you can't add a comment
;; or a colon or semicolon without re-indenting the line.
(defun no-electric (keymap)
  (progn
    (define-key keymap ";" 'self-insert-command)
    (define-key keymap ":" 'self-insert-command)
    (define-key keymap "L" 'self-insert-command)
    (define-key keymap "/" 'self-insert-command)
    (define-key keymap "*" 'self-insert-command)
    (define-key keymap "(" 'self-insert-command)
    (define-key keymap ")" 'self-insert-command)
;    (define-key keymap "{" 'self-insert-command)
;    (define-key keymap "}" 'self-insert-command)
    (define-key keymap "," 'self-insert-command)

    ;; It would be nice to add
    ;;(font-lock-add-keywords nil bad-whitespace)
    ;; here, because anywhere I want no-electrics I also want that.
    ;; But alas, putting it here doesn't work for some reason.
 ))

;; But these stopped working, maybe because the names for the maps
;; are wrong. thunk on #emacs points to:
;; ,,df current-local-map and ,,df local-unset-key
(add-hook 'c-mode-hook (lambda ()
                         (no-electric c-mode-map)
                         (font-lock-add-keywords nil bad-whitespace)))
(add-hook 'c++-mode-hook (lambda ()
                           (no-electric c-mode-map)
                           (font-lock-add-keywords nil bad-whitespace)))
(add-hook 'java-mode-hook (lambda ()
                            (no-electric java-mode-map)
                            (font-lock-add-keywords nil bad-whitespace)))

;; no-electric doesn't work for python mode -- even if : is bound
;; to self-insert it still reindents the line.
;(add-hook 'python-mode-hook (lambda () (electric-indent-mode -1)))
;; But this method does work!
;;http://stackoverflow.com/questions/21182550/how-to-turn-of-electric-indent-mode-for-specific-major-mode
(add-hook 'python-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)
            (font-lock-add-keywords nil bad-whitespace)
            ;(local-set-key "\C-cc" 'comment-region)
            ;(message "python hook")
            ))

(add-hook 'js-mode-hook (lambda ()
  (define-key js-mode-map "," 'self-insert-command)
  (define-key js-mode-map ";" 'self-insert-command)
  (font-lock-add-keywords nil bad-whitespace)
 ))

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
;(autoload 'java-mode   "cc-mode" "Java Editing Mode" t)
(autoload 'archive-mode "arc-mode" "Zip and Jar Mode" t)
;; iimage-mode used to work with a filename of iimage-mode, now it needs iimage.
(autoload 'iimage-mode "iimage" "Iimage Mode" t)

;; Default C style
(setq c-default-style '((java-mode . "java") (other . "stroustrup")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derived C modes, setting different styles for different files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode gnu-c-mode c-mode "GNU C mode"
  (c-set-style "gnu"))
(define-derived-mode linux-c-mode c-mode "Linux C mode"
  (c-set-style "linux"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-mode-alist: Modes to use on specific files.
;; Kinda weird that programming modes can't sort this out themselves.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Opening any file with a name ending in "browse" opens in
;; ebrowse-tree-mode ("C++ Tree" mode).
;; Stop anything from opening automatically in that mode
;; (I can't find a way to remove only the "browse" rule).
(rassq-delete-all 'ebrowse-tree-mode auto-mode-alist)

;; File types -- too bad emacs doesn't handle most of these automatically.
;; These will override any existing mode:
(mapc (apply-partially 'add-to-list 'auto-mode-alist)
      '(
        ("Docs/" . text-wrap-mode)
        ("Tags" . text-mode)
        ("\\.epub$" . archive-mode)
        ("\\.kmz$" . archive-mode)
        ("\\.jar$" . archive-mode)
        ("\\.ja$" . archive-mode)
        ("\\.pde$" . c-mode)
        ("\\.ino$" . c++-mode)
        ("\\.py$" . python-mode)
        ("\\.rb$" . ruby-mode)
        ("\\.R$" . R-mode)
        ("\\.m$" . octave-mode)
        ("\\.scm$" . scheme-mode)
        ("\\.blx$" . html-wrap-mode)
        ("\\.html$" . html-wrap-mode)
        ;("\\.xml$" . xml-mode)
        ;("\\.gpx$" . xml-mode)
        ("\\.js$" . javascript-mode)
        ("\\.r$" . r-mode)
        ("\\.gpx$" . xml-mode)
        ("\\.kml$" . xml-mode)
        ("\\.img$" . text-img-mode)
        ("\\.zsh\\'" . sh-mode)

        ;; Use web-mode by default for PEEC files:
        ("web/peec" . web-mode)
        ;; If there's a PHP mode installed, use it instead:
        ;; ("\\.php" . php-mode)
        ;; Otherwise use web mode:
        ("\\.php" . web-mode)

        ; STS are Nightshade "strato scripts", with no particular syntax
        ; except that they do have a comment syntax defined.
        ; conf-mode seems like a reasonable compromise.
        ("\\.sts" . conf-mode)

        ;; Don't wrap on LWV HTML files -- they tend to have long lines.
        ("lwvweb/" . html-mode)

        ;; Make sure changelogs don't use text-wrap-mode -- they're too long,
        ;; and text-mode invokes spellcheck which takes forever.
        ("ChangeLog" . fundamental-mode)

        ;; A few special settings by location or name,
        ;; for files that may not have type-specific extensions:
        ("Docs/Lists" . text-mode)
        ("Docs/Lists/books" . text-wrap-mode)
        ("blogstuff/" . html-wrap-mode)
        ("Docs/gimp/book/notes" . text-wrap-mode)
        ("README" . text-wrap-mode)
        ;; Book used to be longlines mode, but that was too flaky.
        ("Docs/gimp/book/" . text-wrap-mode)

        ("linux-.*/" . linux-c-mode)

        ;; iimage mode is so cool!
        ("Docs/classes/" . text-img-mode)
        ("Docs/Notes/househunt/houses" . text-img-mode)
        ("Docs/Notes/househunt/sold" . text-img-mode)

        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq completion-ignored-extensions
      '(".xpt" ".a" ".so" ".o" ".d" ".elc" ".class" "~" ".ckp" ".bak" ".pyc"
        ".imp" ".lpt" ".bin" ".otl" ".err" ".lib" ".x9700" ".aux" ".elf" ))

;; This is supposed to prevent the excessive making of local backup files.
;; http://jamesthornton.com/emacs/chapter/emacs_16.html#SEC150
(setq vc-cvs-stay-local nil)

;;;
;;; KEYPAD BINDINGS
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

;; Open recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Turn debugging back off.  Put any questionable code after these lines!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq debug-on-error nil)
(setq stack-trace-on-error nil)
(put 'eval-expression 'disabled nil)

;; A minibuffer completion package from
;; http://www.emacswiki.org/emacs/RecentFiles
;; but it's not very smart, doesn't pay any attention to the current directory.
(load "recent-minibuffer")
(setq enable-recursive-minibuffers t)
(global-set-key "\C-cr" 'recentf-minibuffer-dialog)

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
  (add-to-list 'ac-modes 'inferior-emacs.d/lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;;
;; A few other useful elisp tutorials:
;; http://ergoemacs.org/emacs/elisp_basics.html
;; http://cjohansen.no/an-introduction-to-elisp
;; http://ergoemacs.org/emacs/elisp.html
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings automatically added by Emacs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (markdown-mode elpy undo-tree)))
 '(safe-local-variable-values
   (quote
    ((not-flyspell-mode)
     (encoding . utf-8)
     (auto-fill-mode)
     (wrap-mode)))))
(put 'upcase-region 'disabled nil)
