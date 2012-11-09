;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Highlight and Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'font-lock)
;(setq font-lock-auto-fontify t)
;(setq font-lock-support-mode 'lazy-lock-mode)
(if (fboundp 'global-font-lock-mode) (global-font-lock-mode 1))

;; Cool code from alecf to do mozilla-specific syntax highlighting:
;; Options Menu Settings
;; =====================
(cond
 ((or (string-match "XEmacs" emacs-version)
      (and
       (boundp 'emacs-major-version)
       (or (and 
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (>= emacs-major-version 20))))

;; Syntax colorizing
(setq-default teach-extended-commands-p t)
(setq-default teach-extended-commands-timeout 4)
;(setq-default font-lock-use-colors '(color))
(require 'font-lock)
;(remove-hook 'font-lock-mode-hook 'turn-on-fast-lock)
;(remove-hook 'font-lock-mode-hook 'turn-on-lazy-shot)
(add-hook 'c-mode-common-hook 'turn-on-font-lock)
;; From hober on #emacs:
;(setq font-lock-auto-fontify t)
;(setq font-lock-support-mode 'lazy-lock-mode)

;(set-face-foreground 'font-lock-comment-face "cadetblue")
;(set-face-foreground 'font-lock-keyword-face "darkred")
;(set-face-foreground 'font-lock-string-face "blue")
;(set-face-foreground 'font-lock-type-face "forestgreen")
;(set-face-foreground 'font-lock-variable-name-face "purple")
;(set-face-foreground 'font-lock-function-name-face "red")
;(set-face-foreground 'font-lock-warning-face "yellow")
;(set-face-foreground 'font-lock-constant-face "darkyellow")
;(set-face-foreground 'font-lock-builtin-face "firebrick")
;; These used to work, but don't any more (maybe they're .XEmacs stuff):
;(cond ((string-match "XEmacs" emacs-version)
;       (set-face-foreground 'font-lock-preprocessor-face "darkred")
;       (set-face-foreground 'font-lock-reference-face "yellow")
;       (set-face-foreground 'font-lock-doc-string-face "purple")))

;(custom-set-faces)

(setq moz-font-lock-keywords 
      ;; nspr types 
      '(("\\<\\(PR\\(\\(Ui\\|I\\)nt\\(16\\|32\\|64\\)\\|\\(Packed\\|\\)Bool\\)\\)\\>" .
	 font-lock-type-face)

        ;; nspr special values 
        ("\\<PR_\\(TRUE\\|FALSE\\)\\>" . font-lock-reference-face) 
  
        ;; PRUnichar
        ("PR_Unichar" . font-lock-type-face) 
  
        ;; string types 
        ("\\<ns\\(XPIDL\\|A\\)?C?\\(Auto\\)?\\(Writable\\)?String\\>" . font-lock-type-face)
  
        ;; XPCOM types 
        ("\\<ns\\(result\\|ISupports\\(Array\\)?\\)\\>" . font-lock-type-face) 
        ("\\<NS_IMETHOD\\(IMP\\)?\\>" . font-lock-type-face) 
  
        ;; XPCOM special values 
  
("\\<NS_\\(OK\\|FAILED\\|SUCCEEDED\\|ASSERTION\\|\\(STATIC\\|REINTERPRET\\|CONST\\)_CAST\\|\\(IF_\\)?\\(RELEASE\\|ADDREF\\)\\)\\>"
. font-lock-preprocessor-face) 
        ("\\<nsnull\\>" . font-lock-keyword-face) 
        ("\\<nsCOMPtr\\>" . font-lock-type-face)))
;; End of cool mozilla code from alecf
)) ;; end XEmacs- and emacs 19.14+ specific code

