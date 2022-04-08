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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set window size and font according to screen size. Adapted from
;; http://stackoverflow.com/questions/92971/how-do-i-set-the-size-of-emacs-window
(defun set-frame-size-by-resolution ()
  (interactive)
  (if (display-graphic-p)    ; older: (if window-system
  (progn
    ;; Emacs can't accept some fonts via Xdefaults. Here's how to set them here,
    ;; and we'd want to do it differently based on screen size:
    ;(message (number-to-string (x-display-pixel-height)))
    ;(sleep-for 2)

    ;; Small laptops
    (if (<= (x-display-pixel-height) 768)
        (set-frame-font "-misc-fixed-bold-r-normal-*-14-*-*-*-*-*-*-*")

        ;; X1C display
        (if (<= (x-display-pixel-height) 1080)
            ;; JetBrains doesn't display the same in emacs as in urxvt,
            ;; and ends up taking up a lot of extra vertical space.
            ; (set-frame-font "Monoid HalfTight-8:bold")
            (set-frame-font "JetBrains Mono-11:bold")

            ;; full monitor
            (set-frame-font "Monoid HalfTight-7.5")
            ; (set-frame-font "JetBrains Mono-9")
        ))

    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (let ((newheight  (+ (floor (/ (* (x-display-pixel-height) 0.55)
                                   (frame-char-height)))
                         22)))
      (message (format "New height: %d" newheight))
      (set-frame-height (selected-frame) newheight)
      )


    ;; Always use a width of 80.
    ;; You can't tell what the width is from xwininfo -- emacs reports 79
    ;; when it's really 80. So use the line above.
    (set-frame-width (selected-frame) 80)
    )))
;; To set initial window position too:
;; (set-frame-position (selected-frame) 10 30)

(set-frame-size-by-resolution)

;; Make it easy to zoom in and out, like when moving windows between
;; a laptop screen and large monitor.
;; Taking the adgice in zoom-frm to rebind the keys
;; associated with text-scale-adjust.
(load "zoom-frm")

;;
;; Basic key bindings

;; Emacs apparently doesn't have any way to make a truly global key binding:
;; global-set-key will be overridden by mode hooks.
;; However, general.el might be worth investigating,
;; https://github.com/noctuid/general.el
(defun global-settings ()

  (global-set-key "\C-h" 'delete-backward-char)
  (global-set-key "\C-w" 'backward-kill-word)
  (global-set-key "\C-x\C-c" 'save-buffers-kill-emacs)
  (global-set-key "\C-x\C-v" 'find-file-other-window)
  (global-set-key "\C-xs" 'save-buffer)
  (global-set-key "\M-w" 'kill-region)
  (global-set-key "\C-m" 'newline-and-indent)
  (global-set-key (kbd "RET") 'newline-and-indent)
  (global-set-key "\C-cc" 'comment-region)
  (global-set-key "\C-cC" 'uncomment-region)

  ;; Zooming
  (global-set-key (kbd "C-x C-=") 'zoom-in)
  (global-set-key (kbd "C-x C--") 'zoom-out)
  ;(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)
  (global-set-key (kbd "C-x C-0") 'set-frame-size-by-resolution)

  (setq indent-tabs-mode nil)

) ;; end global-key-bindings

(global-settings)

(add-hook 'prog-mode-hook 'global-settings)
(add-hook 'text-mode-hook 'global-settings)

;;;;;;;;;;;;; X Selection / Clipboard behavior

;; Copy to the kill ring (and primary) when you select with the mouse:
(setq mouse-drag-copy-region t)

;; Make default yank use the primary selection, not clipboard.
(setq select-enable-clipboard nil)
(setq select-enable-primary t)

;; However, that isn't enough. yank (for PRIMARY) and clipboard-paste
;; (for CLIPBOARD) each sometimes replaces the other's selection.
;; Here's a more reliable way to do it, and also turn off
;; emacs' annoying habit of pasting colors from unrelated buffers.
;; HOWEVER: x-selection and gui-get-selection both ignore the
;; current charset, e.g. they paste 326 instead of Ö.
;; (global-set-key (kbd "C-y")
;;                 (lambda () (interactive)
;;                   ;; (insert (x-selection 'PRIMARY))))
;;                   (insert (gui-get-selection 'PRIMARY))))
;; But this doesn't handle any nonascii characters that might be
;; in the selection.
;; mouse-yank-primary does the right thing.
(global-set-key (kbd "C-S-v")
                (lambda () (interactive)
                  (insert (gui-get-selection 'CLIPBOARD))))
;; If emacs goes back to retaining syntax highlighting colors
;; inappropriately on paste, change both of those like this:
;; (insert (substring-no-properties (x-selection 'PRIMARY)))

;; When something is selected and you type, replace the selected text.
(delete-selection-mode t)

;; This copies whatever's selected in emacs or in the kill ring
;; to both primary and clipboard X selections.
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)

;; Leaving a buffer tends to stomp the primary selection.
;; The only way I've found to stop that is to ensure that
;; nothing is selected when changing buffers:
(defun deselect-then (what-then)
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
                (lambda () (interactive) (deselect-then 'switch-to-buffer)))
(global-set-key "\C-x\C-f"
                (lambda () (interactive) (deselect-then 'find-file)))
;;;;;; end selection/clipboard hackery ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lately exchange-point-and-mark selects everything in between the two points,
;; clobbering anything in the primary selection.
;; But you can turn that off by passing t to activate "Transient Mark mode".
(global-set-key "\C-x\C-x" (lambda () (interactive)
                             (exchange-point-and-mark t)))

;; I am forever hitting this by accident, when my finger slips off
;; Ctrl-z and grazes C-x at the same time. Disable it:
(global-unset-key "\C-x\C-z")

;; I seem to spend half my free time chasing after various broken
;; electric indents in emacs. And really, the only time I ever want
;; electric indent is for }. So maybe the answer is to turn off
;; electrics everywhere, then rebind } to indent the current line
;; after inserting. Of course this doesn't really turn off electrics
;; everywhere, anyway.
;(setq electric-indent-mode nil)
;; Someone on #emacs suggests that this works better:
;(electric-indent-mode nil)

;; Despite the previous two lines, electric mode always ends up on anyway.
;; https://emacs.stackexchange.com/a/20899 claims this will tame
;; the worst part of electric indent, the re-indentation of the
;; current line, which I never ever want under any circumstances:
(setq-default electric-indent-inhibit t)

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

;; Insert the date
(define-key global-keys-minor-mode-map (kbd "C-;") 'insert-today-date)
(define-key global-keys-minor-mode-map (kbd "C-:") 'insert-yesterday-date)

;; Allow date insertion in the minibuffer too
(define-key minibuffer-local-map (kbd "C-;") 'insert-today-date)
(define-key minibuffer-local-map (kbd "C-:") 'insert-yesterday-date)
;; I'm always forgetting that I'm in the minibuffer and doing a ^Xb
;; to change buffers, which visits the file in a new window and doesn't
;; put the focus back in the minibuffer.
;; Ignoring it might help; otherwise, maybe make something that beeps
;; or otherwise alerts me.
(define-key minibuffer-local-map "\C-xb" 'ignore)

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

 ;; Markdown faces
 '(markdown-bold-face ((t (:family "Monoid HalfTight-7.5" :foreground "dark orchid" :weight bold :height 1.1))))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "ivory1"))))
 '(markdown-header-face ((t (:family "Liberation Serif" :height 1.5 :weight bold))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8 :foreground "navy blue"))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6 :foreground "medium violet red"))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.4 :foreground "dark red"))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2 :foreground "indian red"))))
 '(markdown-inline-code-face ((t (:inherit font-lock-constant-face :background "gainsboro"))))
 '(markdown-italic-face ((t (:foreground "dark green" :slant italic :height 1.1))))
 '(markdown-link-face ((t (:inherit link))))
 '(markdown-pre-face ((t (:background "ivory1" :family "monoid"))))

 ;; org mode fonts and colors
 '(org-level-1 ((t (:family "Liberation Serif" :height 2.3 :foreground "navy blue" :weight bold))))
 '(org-level-2 ((t (:family "Liberation Serif" :height 2.0 :foreground "dark magenta" :weight bold))))
 '(org-level-3 ((t (:family "Liberation Serif" :height 1.7 :foreground "dark red" :weight bold))))
 '(org-level-4 ((t (:family "Liberation Serif" :height 1.3 :foreground "indian red" :weight bold))))
 '(org-link ((t (:underline t :slant italic :background "white" :foreground "blue"))))
 '(whitespace-trailing ((t (:background "cyan" :foreground "yellow" :weight bold)))))

(set-face-foreground 'mode-line "yellow")
(set-face-background 'mode-line "purple")
(set-face-background 'mode-line-inactive "light blue")

(set-face-background 'trailing-whitespace "cornsilk")

(set-face-attribute 'region nil :background "#8df" :foreground "black")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turning off annoyances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-progressive-speed nil)

;; I'd rather not force this, since I occasionally edit binary files,
;; but it's just too annoying how emacs asks, then doesn't actually
;; add the newline so I have to go and do it myself.
;; Ideally I should do this only for text and Fundamental modes.
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
;; but this is annoying because it also shows up in things like
;; autocomplete of directories. XXX Make it happen in coding modes only.
;; (defface extra-whitespace-face
;;   '((t (:background "honeydew2")))
;;   "Used for tabs and such.")
;; (defvar bad-whitespace
;;   '(("\t" . 'extra-whitespace-face)))

;; Draw tabs with the same color as trailing whitespace
(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("\t" 0 'trailing-whitespace prepend)))))

;; Emacs 23 changed up/down behavior, so it goes to the next screen
;; line instead of the next buffer line (on lines long enough to wrap).
;; Revert to old behavior:
(setq line-move-visual nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A couple of functions to try to guess coding systems based on newlines.
;; Emacs' code for this is broken: it fails on any file that doesn't
;; end with a newline, which includes many formats like GPX files
;; and Spektrum SPM files.

(defun count-pat (pat &optional start end)
  "Return the number of times the given pattern occurs in the region or buffer"
  (interactive  "sPattern: ")
  (save-excursion
    (let ((patcount 0)
          (rstart (if start start (if (region-active-p) (region-beginning) 0)))
          (rend   (if end end (if (region-active-p) (region-end) (point-max))))
          )

      (goto-char rstart)
      (while (and (< (point) rend)
                  (re-search-forward pat rend t))
        (setq patcount (1+ patcount)))
      ;(message "%d occurrences of %s" patcount pat)
      patcount
      )))

(defun guess-line-endings ()
  "Guess whether a file has Unix, Mac or Windows line endings. If the file ends with a newline/cr, Emacs can handle this on its own; but on files with no final newline, Emacs gets confused."
  (interactive)
  ;; (message "Trying to guess line endings on %S" (buffer-name))
  ;; (sleep-for 1)
  ;; (message "")
  (save-excursion
    (goto-char (point-max))
    ;; (let ((lastchar (char-before)))
    ;;   (if (or (char-equal lastchar ?\n) (char-equal lastchar ?\r))
    ;;       (progn
    ;;         (message "the file ends with a newline")
    ;;         (sleep-for 1)
    ;;         nil)
          (let* ((win (count-pat "\r\n"))
                 (unix (- (count-pat "\n") win))
                 (mac  (- (count-pat "\r") win))
                 (few 5)
                 )
            (message "unix %d mac %d win %d" unix mac win)
            (sleep-for 4)
            (cond
             ((and (> unix few) (< mac few)  (< win few)) 'utf-8-unix)
             ((and (> mac few)  (< unix few) (< win few)) 'utf-8-mac)
             ((and (> win few)  (< unix few) (< mac few)) 'utf-8-dos)
             (t nil))
))) ;;)

;; Run on every file to as a pre-check for newlines/coding system:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Default-Coding-Systems.html
;; but this doesn't actually help, e.g. Spektrum SPM files
;; and this doesn't make it automatically run, anyway.
;;(set-variable 'auto-coding-functions (list 'guess-line-endings))

;; The preceding sounds nice, but doesn't actually do anything.
;; Force mac line break interpretation for SPM files.
(modify-coding-system-alist 'file "\\.SPM\\'" 'utf-8-mac)


;; Allow copy from urxvt into emacs to use UTF-8 nonascii characters.
;; But apparently this is for things copied IN emacs,
;; not things copied in other apps and pasted into emacs.
(set-variable 'selection-coding-system 'utf-8)

;; If that proves inadequate (and it does), some other suggestions from
;; https://stackoverflow.com/a/2903256
;; XXX Note that these only affect mouse pasting; they don't help
;; ctrl-Y insert from selection, which does:
;; (insert (gui-get-selection (quote PRIMARY)))
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
; (set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; XXX How can I get (x-selection 'PRIMARY) to use utf-8?
;; F1 C RET (or M-x describe-coding-system RET) lists coding systems in use:
;; they're all utf-8 except
;; Coding system for keyboard input:
;;   U -- utf-8-unix (alias: mule-utf-8-unix cp65001-unix)
;; https://ftp.gnu.org/old-gnu/Manuals/emacs-20.7/html_node/emacs_197.html
;; says I can change that with
;; C-x RET k utf-8 RET
;; but it doesn't work: after that sequence, F1 C RET still
;; lists they keyboard input coding system as utf-8-unix.


;;;;;;;;;;;;; end coding system/newline guessing ;;;;;;;;;;;;;;;;;;;;;;

;; don't paste syntax highlight color into buffers where it's meaningless.
;; In emacs 25 this seems to work, but it doesn't work in emacs 24.
(add-to-list 'yank-excluded-properties 'font)
(add-to-list 'yank-excluded-properties 'font-lock-face)

;; In emacs24, yank-excluded-properties doesn't work.
;; However, this works for Ctrl-Y:
;; (if (and (boundp 'emacs-major-version)
;;          (< emacs-major-version 25))

;;     (progn
;;       (defun yank-without-colors () (interactive)
;;              (insert (substring-no-properties
;;                       (x-selection 'PRIMARY))))
;;       (global-set-key "\C-y" 'yank-without-colors)

;;       ;; Unfortunately there seems to be no way in emacs24 to get
;;       ;; middlemouse to paste without colors.
;;       ;; You can try things like this:
;;       ;;
;;       ;; (defun mouse-paste-without-colors (e)
;;       ;;   (interactive "e")
;;       ;;   (goto-char (posn-point (event-start e)))
;;       ;;   (insert (substring-no-properties (x-selection 'PRIMARY)))
;;       ;;   )
;;       ;; (global-set-key (kbd "<mouse-2>") 'mouse-paste-without-colors)
;;       ;;
;;       ;; but then something happens after the paste that stomps the
;;       ;; primary X selection and replaces it with a bunch of other crap.
;;       ;; That happens in emacs25 too, but fortunately emacs25 does
;;       ;; the right thing with yank-excluded-properties
;;       ;; so we don't need these rebindings.
;;       ;; Nobody seems to know why this is happening.
;;       ;; There's something called <down-mouse-2> but it happens
;;       ;; before <mouse-2>, not after, and binding it to 'nop doesn't help.
;;       ;; There's no <up-mouse-2>.
;;     )
;; )

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

;; Templates for files of specific types:
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

;; Is this still needed?
;; (autoload 'sgml-mode "my-sgml-mode" "Load custom sgml-mode")

;;
;; auto-insert for HTML: check for a blank.html in the directory
;; and insert it if it exists. Otherwise insert a template.
;;
(defun insert-html-template ()
  "Insert the contents of blank.html if it exists"
  (interactive)
  (let ((file-name (concat
                    (file-name-directory (buffer-file-name (current-buffer)))
                    "blank.html")))
    (if (file-exists-p file-name)
        (progn
          (message (concat "Inserting: " file-name))
          (insert-file-contents file-name))

        ; else insert standard HTML boilerplate
        (let ((str (read-string "Title: ")))
          (message "Inserting standard HTML")
          (insert
           "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n"
           "<html>\n"
           "<head>\n"
           "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n"
           "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"

           "<title>" str "</title>\n"
           "</head>\n\n"
           "<body>\n\n"
           "<h1>" str "</h1>\n\n"
           "<p>\n"
            "\n\n"      ;; The skeleton had a _ before the string, for point
           "</body>\n"
           "</html>\n")
          (goto-char (point-max))
          (forward-line -4)
))))

(add-to-list 'auto-insert-alist
             '(("\\.html$" . "HTML file")
               . insert-html-template
))

;; Override the standard HTML content for some files.
(add-to-list 'auto-insert-alist
             '((".*app/templates.*\\.html$" . "HTML file")
               nil
               "{% extends \"base.html\" %}\n"
               "\n"
               "{% block content %}\n"
               "\n"
               "\n"
               "{% endblock %}\n"
))

(add-to-list 'auto-insert-alist
             '((".*\\(lwvweb\\|fairdistrictsnm\\|jemezdarkskies\\).*\\.html$" . "LWVNM HTML file")
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
  "Change line breaks to Unix style. This no longer works because emacs tries to figure out line encodings on its own now, often gets it wrong but that overrides attempts to change things."
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t)
  (replace-string "\r" "")
  ;(message "fixm")
)

;; https://www.emacswiki.org/emacs/EndOfLineTips
(defun unix-file ()
  "Change the current buffer to Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'unix t))

(defun dos-file ()
  "Change the current buffer to DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'dos t))

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
;; XXX This doesn't seem to be used any more.
;; (define-derived-mode html-wrap-mode html-mode "HTML wrap mode"
;;   "HTML mode, plus autofill and flyspell"

;;   (message "HTML wrap mode")

;;   (auto-fill-mode)
;;   (if (<= (buffer-size) 10000)
;;       (progn (flyspell-mode 1)
;;              (flyspell-buffer) )
;;       (message "Buffer too big, not spellchecking")
;;       )

;;   ;; New annoyance in emacs24: every time you save an html file,
;;   ;; it calls a browser on it, replacing whatever's in your current
;;   ;; browser window.
;;   (html-autoview-mode -1)
;;   )

;;  (if (string-match (buffer-local-value 'major-mode (current-buffer))
;;                    "html-mode")
;;  (if (eq (buffer-local-value 'major-mode (current-buffer)) 'html-mode)

(define-derived-mode text-wrap-mode text-mode "Text wrap mode"
  "Text mode, plus autofill and flyspell"
  (auto-fill-mode)
  (if (<= (buffer-size) 10000)
      (progn (flyspell-mode 1)
             (flyspell-buffer) )
      (message "Buffer too big, not spellchecking")
      )
  )

;; Don't autofill when hitting return, only on space:
(set-char-table-range auto-fill-chars 10 nil)

;; In text mode, I don't want it auto-indenting for the first
;; line in the file, or lines following blank lines.
;; Everywhere else is okay.
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
  (local-set-key (kbd "C-;") 'insert-today-date)
  (global-set-key (kbd "C-;") 'insert-today-date)
  (local-set-key (kbd "C-:") 'insert-yesterday-date)
  (global-set-key (kbd "C-:") 'insert-yesterday-date)
  (dubcaps-mode t)
  (toggle-word-wrap)
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

;; Add an inline HTML tag:
(defun add-html-tag (tag) (interactive "sHTML Tag")
  (let (
        (rstart (if (region-active-p) (region-beginning) (point)))
        (rend   (if (region-active-p) (region-end)       (point))))

    ;; Insert the close tag first, because inserting the open tag
    ;; will mess up the rend position.
    (goto-char rend)
    (insert "</" tag ">")

    ;; Now the open tag:
    (goto-char rstart)
    (insert "<" tag ">")
))

;; Plus one that *does* add newlines:
(defun add-html-block (tag) (interactive "sHTML Block")
  (let (
        (rstart (if (region-active-p) (region-beginning) (point)))
        (rend   (if (region-active-p) (region-end)       (point))))

    ;; Insert the close tag first, because inserting the open tag
    ;; will mess up the rend position.
    (goto-char rend)
    (insert "</" tag ">\n")

    ;; Now the open tag:
    (goto-char rstart)
    (insert "<" tag ">\n")
))

;; and a special function for <a href= that tries to be smart about
;; whether the current selection is a URL or link text.

;; First we need a way of checking whether the region contains a URL:
;; (interactive "r") means the function will have two arguments,
;; the current region's start and end. If no region is selected,
;; this will usually give an error, "the mark is not set now".
(defun url-in-region (start end) (interactive "r")
  (save-excursion
    (goto-char start)
    (re-search-forward "://" end t)
    ))

;; And, given that, here's the link-adding function:
(defun add-html-link () (interactive)
  (let ((href     "")
        (linktext "")
        (start    (point))
        (end      (point))
        )

    (if (region-active-p)
      (progn
        (setq start (region-beginning))
        (setq end   (region-end))

        (if (url-in-region start end)
          (setq href     (buffer-substring-no-properties start end))
          (setq linktext (buffer-substring-no-properties start end))
          )

        (kill-region start end)
      )
    )

    (insert "<a href=\"" href "\">" linktext "</a>")

    ;; Move the cursor to a useful place. By default it's at the end
    ;; of the inserted text.
    (if (string-empty-p href)
        ;; No href, put the cursor there
        (progn
          (goto-char start)
          (forward-char 9))
        (if (string-empty-p linktext)
          ;; href but no link text, put the cursor there
          ;; Already at end, no need to (goto-char end)
          (backward-char 4))
    )
))

;; In HTML modes, turn ' -- ' to &mdash;
(defun dashes-to-mdash ()
  "Convert ' -- ' to &mdash;"
  (interactive)
  (if (looking-back " -- ")
      (progn
       (backward-delete-char 4)
       (insert " &mdash; ")
       )))


;; Key bindings and such can be done in the mode hook.
(defun html-hook-fcn ()
  ;; Define keys for inserting tags in HTML and web modes:
  (local-set-key "\C-cb" (lambda () (interactive) (add-html-tag "b")))
  (local-set-key "\C-ci" (lambda () (interactive) (add-html-tag "i")))
  (local-set-key "\C-cc" (lambda () (interactive) (add-html-tag "code")))
  (local-set-key "\C-c1" (lambda () (interactive) (add-html-tag "h1")))
  (local-set-key "\C-c2" (lambda () (interactive) (add-html-tag "h2")))
  (local-set-key "\C-c3" (lambda () (interactive) (add-html-tag "h3")))
  (local-set-key "\C-c4" (lambda () (interactive) (add-html-tag "h4")))

  (local-set-key "\C-cp" (lambda () (interactive) (add-html-block "pre")))
  (local-set-key "\C-cB" (lambda () (interactive) (add-html-block "blockquote")))

  ;; I can never remember if links are on C-c a or C-c l, so use both.
  (local-set-key "\C-ca" 'add-html-link)
  (local-set-key "\C-cl" 'add-html-link)

  ;(local-set-key "\C-m" (lambda () (interactive) (insert "\n")))

  ;; browse-url-of-buffer is on C-c C-v. Set it to quickbrowse:
  ;; (setq browse-url-browser-function 'browse-url-generic
  ;;       browse-url-generic-program "quickbrowse")
  ;; default is xdg-open, set with xdg-settings get|set default-web-browser
  ;; If that ever starts getting called when saving,
  ;; it's probably because html-autoview-mode got mistakenly toggled on;
  ;; toggle it back off with C-c C-s.
  ;; For debugging such things: F1 m shows which minor modes are active,
  ;; and also shows key bindings related to those modes.

  ;; (setq browse-url-browser-function 'browse-url-generic
  ;;             browse-url-generic-program "web-browser")

    ;; (defun my-browse-url-firefox-new-tab (url &optional new-window)
    ;;   "Open URL in a new tab in Mozilla."
    ;;   (interactive (browse-url-interactive-arg "URL: "))
    ;;   (unless
    ;;       (string= ""
    ;;                (shell-command-to-string
    ;;                 (concat "mozilla-firefox -a firefox -new-tab 'openURL("
    ;;                         url ",new-tab)'")))
    ;;     (message "Starting Mozilla Firefox...")))
    ;; (setq browse-url-browser-function 'my-browse-url-firefox-new-tab)

  ;; https://www.emacswiki.org/emacs/BrowseUrl
  (setq browse-url-new-window-flag t)
  (defun browse-url-firefox (url &optional new-window)
    "Ask the Firefox WWW browser to load URL.
  Default to the URL around or before point.  The strings in
  variable `browse-url-firefox-arguments' are also passed to
  Firefox.

  When called interactively, if variable
  `browse-url-new-window-flag' is non-nil, load the document in a
  new Firefox window, otherwise use a random existing one.  A
  non-nil interactive prefix argument reverses the effect of
  `browse-url-new-window-flag'.

  If `browse-url-firefox-new-window-is-tab' is non-nil, then
  whenever a document would otherwise be loaded in a new window, it
  is loaded in a new tab in an existing window instead.

  When called non-interactively, optional second argument
  NEW-WINDOW is used instead of `browse-url-new-window-flag'."
    (interactive (browse-url-interactive-arg "URL: "))
    (setq url (browse-url-encode-url url))
    (let* ((process-environment (browse-url-process-environment))
           (window-args (if (browse-url-maybe-new-window new-window)
                            (if browse-url-firefox-new-window-is-tab
                                '("-new-tab")
                              '("-new-window"))))
           (ff-args (append browse-url-firefox-arguments window-args (list url)))
           (process-name (concat "firefox " url))
           (process (apply 'start-process process-name nil
                           browse-url-firefox-program ff-args) )) ))
    (setq browse-url-browser-function 'my-browse-url-firefox-new-tab)

  ;; More info, like how to write functions to do tabs or reload:
  ;; https://www.emacswiki.org/emacs/BrowseUrl#toc5

  ;; And finally, a generic shorthand to use with other tags:
  ;; Consider changing this to use add-html-tag instead.
  (local-set-key "\C-ct"  (lambda () (interactive) (sgml-tag)))

  ;; Convert " -- " to &mdash;
  (add-hook 'post-self-insert-hook #'dashes-to-mdash nil 'local)

  ;; Contents of <pre> tags get reindented, destroying their formatting.
  ;; You can avoid that by not inserting a newline, same as with <code>,
  ;; But better is to turn off indenting entirely in html-mode,
  ;; which is fine with me. 'ignore is elisp's nop function.
  ;; That's for html mode. In web-mode, indent works better so
  ;; no need to comment it out.
  ;; (setq indent-line-function 'ignore)

  ;; Web mode is super aggressive about indenting everything, all the time.
  ;; Try to disable it entirely.
  ;; Setting electric-indent-mode to nilhas no effect;
  ;; The help for c-electric-slash says that setting c-electric-flag
  ;; to bil will disable it, and describe-variable knows it,
  ;; but it doesn't show up in autocomplete for set-variable.
  (setq web-mode-enable-auto-indentation nil)

  (setq c-electric-flag nil)
  (electric-indent-mode nil)

  ;; Indent for web-mode
  (setq web-mode-markup-indent-offset 0)
  ; (setq js-indent-level 4)
  (setq web-mode-code-indent-offset 4)

  ;; Prevent the obnoxious line breaking in the middle of --
  ;; when sgml-mode thinks it's a comment.
  ;; The misbehavior currently comes from this line in sgml-mode:
  ;; (setq-local comment-line-break-function 'sgml-comment-indent-new-line)
  ;; so redefining sgml-comment-indent-new-line also works.
  ;; See also bug http://debbugs.gnu.org/cgi/bugreport.cgi?bug=36227
  (kill-local-variable 'comment-line-break-function)

  ;; Would be nice if this would fix the horked dash handling in sgml-mode,
  ;; but alas it has zero effect that I can find.
  ;;(setq sgml-specials nil)

  ;; Turn off flyspell; we'll turn it on only in html-wrap mode.
  ;(flyspell-mode 0)

  ;; (message "Ran html-hook")
  ;; (sleep-for 2)

  (if (<= (buffer-size) 10000)
      (progn (flyspell-mode 1)
             (flyspell-buffer) )
      (message "Buffer too big, not spellchecking")
      )

  (dubcaps-mode t)

  )
(add-hook 'sgml-mode-hook 'html-hook-fcn)

;; Previous attempts to prevent -- dashed sections -- from screwing up
;; auto-fill mode in sgml-mode.
;; (defun sgml-comment-indent-new-line (&optional soft)
;;   (save-excursion (forward-char -1) (delete-horizontal-space))
;;   (delete-horizontal-space)
;;   (newline-and-indent))
;;   ;(comment-indent-new-line soft))
;; (defvar sgml-specials '(?\"))
;; (defun sgml-comment-indent-new-line (&optional soft)
;;   (comment-indent-new-line soft))

;; html-mode has become awful, because it treats all single quotes
;; as the beginning of a string. So if you have any text like
;;     here's an example
;; in your HTML content, html-mode will colorize everything after the '
;; as though it's part of a string. So I'm mostly using web-mode instead,
;; and it also handles PHP and JS (sort of).
(autoload 'web-mode "web-mode" "Web Mode")

;; backward-kill-word is from Mars on PHP files. It kills whole clauses.
;; This improves its behavior.
;; https://emacs.stackexchange.com/a/13063
;; Unfortunately it loses the nice property of backward-kill-word
;; that consecutive word kills can all be restored with a single yank.
(defun dwim-backward-kill-word ()
  "DWIM kill characters backward until encountering the beginning of a
word or non-word."
  (interactive)
  (if (thing-at-point 'word) (backward-kill-word 1)
    (let* ((orig-point              (point))
           (orig-line               (line-number-at-pos))
           (backward-word-point     (progn (backward-word) (point)))
           (backward-non-word-point (progn (goto-char orig-point)
                                           (backward-non-word) (point)))
           (min-point               (max backward-word-point
                                         backward-non-word-point)))

      (if (< (line-number-at-pos min-point) orig-line)
          (progn
            (goto-char min-point)
            (end-of-line)
            (delete-horizontal-space))
        (delete-region min-point orig-point)
        (goto-char min-point))
      )))

(defun backward-non-word ()
  "Move backward until encountering the beginning of a non-word."
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9\s\n]")
  (while (looking-at "[^a-zA-Z0-9\s\n]")
    (backward-char))
  (forward-char))

(defun web-mode-hook-fcn ()
  "Hooks for Web mode, which also calls html-hook-fcn."

  ;; Start with everything that's in the html-mode hook:
  (html-hook-fcn)

  ;; (message "web-mode hook")
  ;; (sleep-for 2)

  ;; Disable indentation for HTML and CSS, while keeping it for PHP and JS.
  ;; Unfortunately web-mode often ignores this.
  (setq-local web-mode-markup-indent-offset 0)
  (setq-local web-mode-css-indent-offset 0)

  ;; Since that doesn't work and there's apparently no way to turn off
  ;; indentation in web-mode, at least don't do it on newline.
  ;(local-set-key (kbd "RET") 'newline)

  ;; web-mode's backward-kill-word is terrible on PHP files;
  ;; but now that I found a PHP mode, maybe it doesn't matter so much.
  ;; (local-set-key "\C-w" 'dwim-backward-kill-word)

  ;; The possible indent variables:
  ;; coffee-tab-width              ; coffeescript
  ;; javascript-indent-level       ; javascript-mode
  ;; js-indent-level               ; js-mode
  ;; web-mode-markup-indent-offset ; web-mode, html tag in html file
  ;; web-mode-css-indent-offset    ; web-mode, css in html file
  ;; web-mode-code-indent-offset   ; web-mode, js code in html file
  ;; css-indent-offset             ; css-mode

)

(add-hook 'web-mode-hook 'web-mode-hook-fcn)

(defun php-mode-hook-fcn ()
  "Hooks for PHP mode, which also calls web-mode-hook-fcn and html-hook-fcn."
  (web-mode-hook-fcn)

  (no-electric php-mode-map)
  ;; (message "php-mode hook")
  ;; (sleep-for 2)
  )

(add-hook 'php-mode-hook 'php-mode-hook-fcn)

;; Turn off auto-indentation in web mode: it does all kinds of crazy
;; things and makes HTML difficult to edit:
;; Unfortunately this may also turn off PHP indentation.
;; But no worries about that, it doesn't work anyway.
;; (setq web-mode-enable-auto-indentation nil)

;; Run this on a region inside a <pre> to convert chars like < into entities.
(defun unhtml (start end)
  (interactive "rStart" "rEnd")
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

(defun indent-whole-buffer ()
      "indent whole buffer and untabify it"
      (interactive)
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))

;; A mode for editing tab-separated tables, or any other file
;; where you want TAB to insert a tab. (You wouldn't think that
;; would be difficult, but it is! indent-tabs-mode doesn't do it,
;; lines inserted at the beginning of the file are still untabified.)
(define-derived-mode tabbed-mode text-mode "Tab separated mode"
  (local-set-key (kbd "TAB") 'self-insert-command)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode
;; https://github.com/jrblevin/markdown-mode
;; https://leanpub.com/markdown-mode/read
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(setq markdown-command "/usr/bin/markdown_py")

(defun markdown-hook ()
  (flyspell-mode 1)
  (auto-fill-mode)
  )
(add-hook 'markdown-mode-hook 'markdown-hook)


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

;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;;;;;;;;;;;;;;;;;;;;;

;; Run M-x org-mode-restart to reload changes

;; https://emacs.stackexchange.com/a/35632 has a very complicated solution
;; to adding new emphasis characters, but it's dependent on org-habit
;; whatever that is.

;; Don't collapse document structure by default,
;; in case of using org-mode for richtext rather than organizing.
(setq org-startup-folded nil)

;; Hide things like the slashes in /italic/
(setq org-hide-emphasis-markers t)

;; Use colors for *bold*, *italic* etc.
;; *** See also the org-mode section higher up in this file
;; under "custom-set-faces", which sets colors and fonts
;; for org mode headers and links.
(setq org-emphasis-alist
  '(("*" (bold :foreground "maroon"
               :weight bold
               :family "Noto Mono"
               ;; was "Noto Mono", some fonts that work:
               ;; If the base font is already bold, bolding it here
               ;; is less visible. Some fonts that help:
               ;; "Overload" "Joshs Font" "Hot Pizza"
               ;; "Snappy Service" "Funny Pages"
               ;; but none of them look good smaller, on a large screen
               ;; and besides, they're not monospaced.
               ;; Probably the solution is to re-set this alist
               ;; when changing resolutions

               ;; also make it a little bigger
               :height 1.3))
    ("/" (italic :foreground "purple"))
    ("_" (underline :foreground "navy blue"))

    ;; = is "verbatim"
    ("=" (:background "white" :foreground "MidnightBlue"))

    ;; ~ is code
    ("~" (:background "beige" :foreground "black"
                      :family "Noto Mono" :height 1.2))
    ("+" (:strike-through t))

    ;; Try a user-defined style (doesn't work)
    ;;("!" (:background "beige" :foreground "black"))
    ))

;; no need to show the *, / etc.
(setq org-hide-emphasis-markers t)

;; Show bullets for lists
;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
;; but it doesn't work
;; (font-lock-add-keywords 'org-mode
;;     '(("^ +\\([-*]\\) "
;;        (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun org-emphasisify (tag)
  (interactive "sAdd emphasis around region or point")
  (let (
        (rstart (if (region-active-p) (region-beginning) (point)))
        (rend   (if (region-active-p) (region-end)       (point))))

    ;; Insert the close tag first, because inserting the open tag
    ;; will mess up the rend position.
    (goto-char rend)
    (insert tag)

    ;; Now the open tag:
    (goto-char rstart)
    (insert tag)
))

(defun org-hook-fcn ()
  (setq truncate-lines nil)

  ;; wrap long lines at word boundaries
  ;; A simple way that shows continuation lines:
  (toggle-word-wrap)
  ;; some people prefer visual-line-mode, which doesn't show the
  ;; continuation. I'm not clear what difference org-indent-mode makes.
  ;(visual-line-mode)
  ;; which usually also includes
  ;(org-indent-mode)
  ;; but I find that too annoying

  (local-set-key "\C-cb" (lambda () (interactive) (org-emphasisify "*")))
  (local-set-key "\C-c*" (lambda () (interactive) (org-emphasisify "*")))
  (local-set-key "\C-ci" (lambda () (interactive) (org-emphasisify "/")))
  (local-set-key "\C-c/" (lambda () (interactive) (org-emphasisify "/")))
  (local-set-key "\C-cc" (lambda () (interactive) (org-emphasisify "~")))
  (local-set-key "\C-c_" (lambda () (interactive) (org-emphasisify "_")))
  (local-set-key "\C-cu" (lambda () (interactive) (org-emphasisify "_")))
  (local-set-key "\C-c-" (lambda () (interactive) (org-emphasisify "-")))
)
(add-hook 'org-mode-hook 'org-hook-fcn)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert the current date into the buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://ergoemacs.org/emacs/elisp_datetime.html
(defun insert-date (&optional time)
  "Insert current date yyyy-mm-dd."
  (interactive "sDate")
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  ;(insert (format-time-string "%Y-%m-%d"))
  (if (null time) (setq time (current-time)))
  ; (insert (format "%s" time))
  (insert (format-time-string "%Y-%m-%d" time))
  )

(defun insert-today-date ()
  "Insert today's date yyyy-mm-dd."
  (interactive)
  (insert-date))

;; Yesterday's date, from http://emacswiki.org/emacs/Journal
;; This doesn't work on Mar 1 2021: it inserts 2/27, not 2/28
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
(defun rich-style (style) "Set a style in rich-text-mode"
  (interactive "sStyle (e.g. bold)")
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
  (setq buffer-face-mode-face
        '(:family "DejaVu Sans" :height 105 :width semi-condensed))
  (buffer-face-mode)
  )

(defun get-and-insert-image (progname imgdir flags)
  "Prompt for a filename, prepend img/ and append .jpg if needed, then insert a URL for it into the buffer and run a program"
  (interactive "sProgram name" "sImage Dir" "sFlags")
  (let* ((imgfile (read-string "Filename? " imgdir 'my-history))
         (imgfile (if (string-match "\\." imgfile)
                      imgfile
                      ;; (mapconcat 'identity '(imgfile "jpg") ".")
                      (concat imgfile ".jpg")
                      ))
         (excpath (concat "/usr/bin/" progname " " flags))
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

;; If you find you need to run something after a hook loads,
;; look into delayed-after-hook-functions
;; e.g., in js-hook-fcn, add things to delayed-after-hook-functions

;; Make Python files executable. It would be nice if this ran
;; only on Python files, but I haven't figured out how.
;; Hopefully it doesn't take too long to do it everywhere.
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(setq octave-block-offset 4)

; Ruby
(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")
(defun ruby-hook-fcn ()
  (local-set-key "\C-m" 'newline-and-text-indent)
  (turn-on-font-lock)
  )
(add-hook 'ruby-mode-hook 'ruby-hook-fcn)

(defun archive-hook-fcn ()
  (flyspell-mode 0)
  )
(add-hook 'archive-mode-hook 'archive-hook-fcn)

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
(defun c-hook-fcn ()
  (no-electric c-mode-map)
  ;; Now giving: file mode specification error:
  ;;   (void-variable bad-whitespace)
  ;;(font-lock-add-keywords nil bad-whitespace)
  )
(add-hook 'c-mode-hook 'c-hook-fcn)
(add-hook 'c++-mode-hook 'c-hook-fcn)
(add-hook 'java-mode-hook 'c-hook-fcn)

;; no-electric doesn't work for python mode -- even if : is bound
;; to self-insert it still reindents the line.
;(add-hook 'python-mode-hook (lambda () (electric-indent-mode -1)))
;; But this method does work!
;;http://stackoverflow.com/questions/21182550/how-to-turn-of-electric-indent-mode-for-specific-major-mode
(defun python-hook-fcn ()
  (electric-indent-local-mode -1)
  ;(font-lock-add-keywords nil bad-whitespace)
  ;(local-set-key "\C-cc" 'comment-region)

  ;; Enable autocomplete using jedi
  ;; Commented out for now, until I figure out how to prevent annoying popups.
  ;(jedi:setup)

  ;(message "python hook")
  )
(add-hook 'python-mode-hook 'python-hook-fcn)

;; http://tkf.github.io/emacs-jedi/latest/
;; Not sure if these are both needed: apt install elpa-elpy elpa-jedi
; (add-hook 'python-mode-hook 'jedi:setup)
;; If jedi turns out not to work well, another way involves
;; emacs-autocomplete plus yasnippet.

;; How to show tooltips. '(popup) or nil uses minibuffer,
;; '(popup) pops up a tooltip window, but it covers the current buffer
;; and then cuts off on the right because it can't cross the toolbar
;; https://emacs.stackexchange.com/questions/21685/jedi-popup-window-how-to-make-it-larger-than-its-parent-frame
;; so it's pretty useless. And unfortauntely there's no way to call up
;; this info on command, the only way to get it is after a timeout.
(setq jedi:tooltip-method nil)
;; Don't pop up tooltips that block the buffer content all the time.
;; There's apparently no setting that says never pop them up.
(setq jedi:get-in-function-call-delay 1000000000)
(setq jedi:complete-on-dot t)                 ; optional

;; THIS DOESN"T WORK, neither javascript-mode-hook nor js2-mode-hook
;; gets run.
;; javascript-mode ignores (setq-default indent-tabs-mode nil)
;; and needs it to be specified separately.

(defun js-hook-fcn ()
  (define-key js-mode-map "," 'self-insert-command)
  (define-key js-mode-map ";" 'self-insert-command)
  ;(font-lock-add-keywords nil bad-whitespace)

  (setq js-indent-level 4)
  (setq indent-tabs-mode nil)
  (debug-on-variable-change 'indent-tabs-mode)
  (message "js-mode-hook running in %s" (buffer-name))
 )
(add-hook 'js-mode-jook 'js-hook-fcn)

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
;; Indent case labels relative to the case statement:
(c-set-offset 'case-label '+)

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
(define-derived-mode web-wrap-mode web-mode "Web Wrap Mode"
  (auto-fill-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; change-log-mode should allow tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun change-log-hook ()
  (electric-indent-local-mode)
  (setq indent-tabs-mode t)
  ; These didn't work:
  ; (local-set-key (kbd "<S-return>") 'newline)
  ; (define-key change-log-mode-map (kbd "<S-return>") 'newline)
  ; (define-key change-log-mode-map (kbd "<S-return>") 'self-insert-command)
  ; This one did, but it turns out it isn't needed:
  ; with electric-indent-local-mode
  ; (define-key change-log-mode-map (kbd "<S-return>")
  ;   (lambda () (interactive) (insert "\n")))
)
(add-hook 'change-log-mode-hook 'change-log-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-mode-alist: Modes to use on specific files.
;; Kinda weird that programming modes can't sort this out themselves.
;;
;; In auto-mode-alist, the first match wins. However, the way this
;; list is built up ends up adding everything in reverse order:
;; so in the below list, the *last* match wins.
;;
;; https://www.emacswiki.org/emacs/AutoModeAlist
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
        ("\\.blx$" . web-wrap-mode)
        ("\\.html$" . web-wrap-mode)
        ;("\\.xml$" . xml-mode)
        ;("\\.gpx$" . xml-mode)
        ("\\.js$" . javascript-mode)
        ("\\.r$" . r-mode)
        ("\\.gpx$" . xml-mode)
        ("\\.kml$" . xml-mode)
        ("\\.img$" . text-img-mode)
        ("\\.zsh\\'" . sh-mode)

        ("\\.md$" . markdown-mode)
        ("\\.org$" . org-mode)

        ;; Spektrum transmitter model definition files:
        ;; line endings are screwy, don't try to change them
        ;; or wrap long lines or otherwise add line endings.
        ;; ("\\.SPM$" . hexl-mode)

        ; STS are Nightshade "strato scripts", with no particular syntax
        ; except that they do have a comment syntax defined.
        ; conf-mode seems like a reasonable compromise.
        ("\\.sts" . conf-mode)

        ;; Don't wrap on LWV HTML files -- they tend to have long lines.
        ("lwvweb/" . web-mode)
        ;; fairdistricts website is mostly adding long links,
        ("fairdistrictsnm/" . web-mode)

        ;; Make sure changelogs don't use text-wrap-mode -- they're too long,
        ;; and text-mode invokes spellcheck which takes forever.
        ("ChangeLog" . change-log-mode)
        ("CHANGELOG" . change-log-mode)

        ;; A few special settings by location or name,
        ;; for files that may not have type-specific extensions
        ;; or that might want to override a setting like auto-fill:
        ("Docs/Lists" . text-mode)
        ("Docs/Lists/books" . text-wrap-mode)
        ; ("blogstuff/" . web-mode)
        ("Docs/gimp/book/notes" . text-wrap-mode)
        ("README$" . text-wrap-mode)

        ;; Book used to be longlines mode, but that was too flaky.
        ("Docs/gimp/book/" . text-wrap-mode)

        ("linux-.*/" . linux-c-mode)

        ;; iimage mode is so cool!
        ("Docs/classes/" . text-img-mode)
        ("Docs/classes/.*\.py" . python-mode)

        ("Docs/Notes/househunt/houses" . text-img-mode)
        ("Docs/Notes/househunt/sold" . text-img-mode)

        ("Docs/classes/welding/" . text-wrap-mode)
        ))


;; If there's a PHP mode installed, use it instead:
(if (condition-case nil (require 'php-mode) (error nil))
    (add-to-list 'auto-mode-alist '("\\.php" . php-mode))

    ;; Otherwise use web mode:
    (add-to-list 'auto-mode-alist '("\\.php" . web-mode))
    )

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

;; Save recent files every N minutes.
;; This is saved at exit, but since X crashes so often on the CX1,
;; emacs often doesn't get a chance to exit gracefully.
;; This is the simple way, but then you get frequent "saved recentf" messages.
;;(run-at-time nil (* 5 60) 'recentf-save-list)
(run-at-time nil (* 5 60)
             (lambda ()
               (let ((save-silently t))
                 (recentf-save-list))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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

;; How to ignore a key completely:
;; My keyboard has started misbehaving: the spacebar sends both Space and
;; Hangul_Hanja, but only emacs sees it. Apparently either nil or 'ignore works.
;(global-set-key [Hangul_Hanja] 'ignore)
;(global-set-key [Hangul_Hanja] nil)
;; https://www.reddit.com/r/emacs/comments/8on9zi/can_i_make_emacs_completely_ignore_a_key/e04wd3w/
;; (define-key special-event-map (kbd "<Hangul_Hanja>") 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings automatically added by Emacs:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages '(jedi markdown-mode elpy undo-tree))
 '(safe-local-variable-values
   '((\#+STARTUP . Content)
     (\#+STARTUP . overview)
     (org-startup-folded . t)
     (auto-fill)
     (not-flyspell-mode)
     (encoding . utf-8)
     (auto-fill-mode)
     (wrap-mode)))
 '(web-mode-code-indent-offset 4)
 '(web-mode-markup-indent-offset 0))
(put 'upcase-region 'disabled nil)

(put 'downcase-region 'disabled nil)
