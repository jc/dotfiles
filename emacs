;; -*- mode: lisp -*-
(setq debug-on-error nil)
(setq inhibit-startup-message t)
(setq user-full-name "James Clarke")
(setq user-mail-address "james@jamesclarke.net")

;;assumes erc installed via macports
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/erc")
(require 'erc-auto)

;;assume color-theme.el installed via macports
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-gnome2)))

;;flyspell text modes assumes aspell installed via macports
(setq-default ispell-program-name "/opt/local/bin/aspell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;;git stuff assumes git-core installed via macports
(add-to-list 'load-path "/opt/local/share/doc/git-core/contrib/emacs")
(require 'git)
(require 'git-blame)

;;gist assumes https://github.com/defunkt/gist.el
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/gist.el")
(require 'gist)

;; magit https://github.com/philjackson/magit
(require 'magit)

;; dsvn
;; http://svn.apache.org/repos/asf/subversion/trunk/contrib/client-side/emacs/dsvn.el
(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)
(require 'vc-svn)

;; smart-tab https://github.com/genehack/smart-tab
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/smart-tab")
(require 'smart-tab)
(global-smart-tab-mode 1)
(add-to-list 'smart-tab-disabled-major-modes 'eshell-mode)

;; simplenote https://github.com/cefstat/simplenote.el
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/simplenote.el")
(require 'simplenote)
(simplenote-setup)

;; auto revert modified files
(global-auto-revert-mode 1)

;; type less yes or no?
(fset 'yes-or-no-p 'y-or-n-p)

;; markdown-mode http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;assumes auctex installed
(require 'tex-site)

(require 'cl)

;; because filenames are not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; mac setup
(setq mac-command-modifier 'meta) ;; command is meta
(setq mac-option-modifier 'meta) ;; option is meta
(setq x-select-enable-clipboard t)

;; omg it is the font stuff!
(set-face-attribute 'default nil :family "Anonymous Pro" :height 140)

;; visual bell
;(setq visible-bell t)
;(setq visible-bell 'top-bottom)
;(defun my-bell-function ()
;  (unless (memq this-command
;        '(isearch-abort abort-recursive-edit exit-minibuffer
;              keyboard-quit mwheel-scroll down up next-line previous-line
;              backward-char forward-char))
;    (ding)))
;(setq ring-bell-function 'my-bell-function)

;; sometimes I like line numbers on the left
(require 'linum)

;; shell stuff
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; don't write over my prompt (don't confuse me!)
(setq comint-prompt-read-only)

;; cua mode customization
(cua-mode t)
;(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only t) ;; no transient mark mode
(cua-selection-mode t)

;; longlines customization
;(setq longlines-wrap-follows-window-size t)

;; better line wrapping
;(global-visual-line-mode 1)

;; kill ring browsing
;; browse-kill-ring.el http://www.emacswiki.org/emacs/download/browse-kill-ring.el
;; browse-kill-ring+.el http://www.emacswiki.org/emacs/download/browse-kill-ring%2b.el
(require 'browse-kill-ring)
(require 'browse-kill-ring+)
(browse-kill-ring-default-keybindings)

;; When not using X, don't show the menu
(if (not window-system)
    (menu-bar-mode 0))

; http://www.emacswiki.org/emacs/DynamicAbbreviations
; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)
(eval-after-load "hippie-exp"
  '(setq hippie-expand-try-functions-list
         (remove 'try-expand-line hippie-expand-try-functions-list)))

; add imenu
(defun try-to-add-imenu ()
   (condition-case nil (imenu-add-menubar-index) (error nil)))
 (add-hook 'font-lock-mode-hook 'try-to-add-imenu)
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
        (goto-char position))))

;; goto to symbol shortcut
(global-set-key (kbd "C-c t") 'ido-goto-symbol)

;;paren matching
(show-paren-mode 1)
(setq show-paren-style 'expression)

;;tabs? no thanks
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq default-tab-width 4)

;;truncate
;(set-default 'truncate-lines t)

;;column numbers
(setq column-number-mode t)

;;ido please
(ido-mode t)
(setq ido-enable-flex-matching t)

;; highlight URLs in comments/strings
(add-hook 'find-file-hooks 'goto-address-prog-mode)

;; Make a passable attempt at using UTF-8 in buffers
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;; encoding
(modify-coding-system-alist 'file "\\.*\\'" 'utf-8)

;;turn off emacs 21 toolbar
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; ugly scrollbars? no thanks!
(if (window-system)
    (scroll-bar-mode -1))

;;;see what I am highlighting!
(transient-mark-mode t)
;;delete what I have highlighted
(delete-selection-mode nil)


(setq-default fill-column 80)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph       
;;; Takes a multi-line paragraph and makes it into a single line of text.       
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;;full filename in title?
(setq frame-title-format "%f")

;; chmod u+x when saving a script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; buffer switching
(require 'bs)
;; use bs for buffer selection
(global-set-key "\C-x\C-b" 'bs-show)
;; next buffer
(global-set-key "\C-cp" 'bs-cycle-previous)
;; previous buffer
(global-set-key "\C-cn" 'bs-cycle-next)

(defun kill-all-dired-buffers()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let((count 0))
      (dolist(buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'dired-mode)
          (setq count (1+ count))
          (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count ))))

(define-key bs-mode-map (kbd "D D") 'kill-all-dired-buffers)

;; hyde note
(defun hyde-new-note()
  "Init a new note entry"
  (interactive)
  (insert "{% extends \"_post.html\" %}\n{% hyde\ntitle:\ncreated: ")
  (insert (format-time-string "%Y-%m-%d %H:%M:%S"))
  (insert "\n%}\n{% block article %}\n")
  (insert "<div class=\"photo\"></div>\n")
  (insert "<blockquote><p> &mdash; <cite></cite>.</p></blockquote>\n")
  (insert "\n{% endblock %}")
  (forward-line -3)
)
