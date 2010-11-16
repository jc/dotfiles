(setq debug-on-error nil)
(setq inhibit-startup-message t)
(setq user-full-name "James Clarke")
(setq user-mail-address "clarkeje@gmail.com")


;; assumes erc installed via macports
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/erc")
(require 'erc-auto)


(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-gnome2)))

;;flyspell text modes
(setq-default ispell-program-name "/opt/local/bin/aspell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))


(require 'cl)

(require 'tex-site)

;; because filenames are not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
(setq x-select-enable-clipboard t)

;; omg it is the font stuff!
(set-face-attribute 'default nil :family "Anonymous Pro" :height 140)

;; visual bell
(setq visible-bell t)
(setq visible-bell 'top-bottom)
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; sometimes I like line numbers on the left
(require 'linum)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; cua mode customization
(cua-mode t)
;(setq cua-enable-cua-keys nil)
(setq cua-highlight-region-shift-only t) ;; no transient mark mode
(cua-selection-mode t)


(setq longlines-wrap-follows-window-size t)

;; When not using X, don't show the menu
(if (not window-system)
    (menu-bar-mode 0))

; http://www.emacswiki.org/emacs/DynamicAbbreviations
; http://trey-jackson.blogspot.com/2007/12/emacs-tip-5-hippie-expand.html
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key minibuffer-local-map (kbd "C-<tab>") 'hippie-expand)

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

;; Make a passable attempt at using UTF-8 in buffers
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
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

;; look ma, I can do full screen too!
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth))) 
