(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA
  ;; and MELPA Stable as desired
  (add-to-list 'package-archives
               (cons "melpa-stable"
                     (concat proto "://stable.melpa.org/packages/"))
               t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
                 (cons "gnu"
                       (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq user-full-name "Vijay Akkineni"
      user-mail-address "vakkineni@cardlytics.com")

(setq load-prefer-newer t)
(setq gc-cons-threshold 50000000)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(font . "Fira Code Retina-16"))
(set-face-attribute 'default t :font  "Fira Code Retina-16")

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst akkineni-savefile-dir (expand-file-name "savefile"
                                                  user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p akkineni-savefile-dir)
  (make-directory akkineni-savefile-dir))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(setq ring-bell-function 'ignore)
(blink-cursor-mode -1)
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance
(setq require-final-newline t)
(delete-selection-mode t)
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(global-hl-line-mode +1)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(setq save-abbrevs 'silent)
(setq-default abbrev-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*")

(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            ;; Set dired-x global variables here.  For example:
            ;; (setq dired-guess-shell-gnutar "gtar")
            ;; (setq dired-x-hands-off-my-keys nil)
            ))
(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            ;; (dired-omit-mode 1)
            ))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (add-to-list 'company-backends 'merlin-company-backend)
  (global-company-mode))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (show-paren-mode 1)
            ))

(add-hook 'lisp-interaction-mode
          (lambda ()
            (paredit-mode t)
            (rainbow-delimiters-mode t)
            (show-paren-mode 1)
            ))

(require 'ielm)

(defun ielm/clear-repl ()
  "Clear current REPL buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ielm-send-input)))

(define-key inferior-emacs-lisp-mode-map
  (kbd "M-RET")
  #'ielm-return)

(define-key inferior-emacs-lisp-mode-map
  (kbd "C-j")
  #'ielm-return)

(define-key inferior-emacs-lisp-mode-map
  (kbd "RET")
  #'electric-newline-and-maybe-indent)

(define-key inferior-emacs-lisp-mode-map
  (kbd "<up>")
  #'previous-line)

(define-key inferior-emacs-lisp-mode-map
  (kbd "<down>")
  #'next-line)

(define-key inferior-emacs-lisp-mode-map
  (kbd "C-c C-q")
  #'ielm/clear-repl)

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package aggressive-indent
  :ensure t
  :config
  (progn
    (global-aggressive-indent-mode 1)))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package magit
  :ensure t
  :bind
  (("C-x g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind
  (("s-g" . git-timemachine)))

(use-package paren
  :ensure t
  :config
  (show-paren-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.15)
  (dimmer-mode))

(use-package ag
  :ensure t)

(use-package flx
  :ensure t)

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :init
  (setq ivy-height 30)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :config
  (ivy-mode 1)
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper))
  :diminish)

(use-package all-the-icons
  :ensure t)

(use-package ivy-rich
  :ensure t
  :init
  (setq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (defun counsel-rg-at-point ()
    (interactive)
    (let ((selection (thing-at-point 'word)))
      (if (<= 4 (length selection))
          (counsel-rg selection)
        (counsel-rg))))
  :bind
  (("M-x" . counsel-M-x)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> u" . counsel-unicode-char)
   ("C-c h" . counsel-rg)
   ("C-c H" . counsel-rg-at-point)
   ("C-c i" . counsel-imenu)
   ("C-c C-g" . counsel-ag)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-x l" . counsel-locate)
   ("C-x C-f" . counsel-find-file)
   ("C-c y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("C-r" . counsel-minibuffer-history))
  :diminish)

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" akkineni-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" akkineni-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" akkineni-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package easy-kill
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)))

(use-package whitespace
  :ensure t
  :config
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'text-mode-hook #'whitespace-mode)
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package yaml-mode
  :ensure t)

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package super-save
  :ensure t
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package clojure-mode
  :mode "\\.clj\\'"
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :after clojure-mode
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package clj-refactor
  :ensure t
  :after clojure-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package haskell-mode
  :defer t
  :mode ("\\.lhs\\'" "\\.hs\\'")
  :bind (:map haskell-mode-map
              ("M-g i" . haskell-navigate-imports)
              ("M-g M-i" . haskell-navigate-imports))
  :init
  (progn
    (setq haskell-compile-cabal-build-alt-command
          "cd %s && stack clean && stack build --ghc-options -ferror-spans"
          haskell-compile-cabal-build-command
          "cd %s && stack build --ghc-options -ferror-spans"
          haskell-compile-command
          "stack ghc -- -Wall -ferror-spans -fforce-recomp -c %s")))

(use-package haskell-mode
  :mode ("\\.lhs\\'" "\\.hs\\'")
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-mode-hook 'eldoc-mode))

(use-package hindent
  :ensure t
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package intero
  :ensure t
  :after haskell-mode
  :config
  (intero-global-mode)
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package ensime
  :ensure t)

(use-package scala-mode
  :after ensime
  :ensure t
  :interpreter
  ("scala" . scala-mode))

(use-package sbt-mode
  :after ensime
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package org
  :commands (org-mode)
  :bind(("C-c l" . org-store-link)
        ("C-c c" . org-capture)
        ("C-c a" . org-agenda)
        ("C-c b" . org-switchb)
        ("C-c C-w" . org-refile)
        ("C-c j" . org-clock-goto)
        ("C-c C-x C-o" . org-clock-out))
  :config
  (setq org-directory "~/Documents/Notes/"
	org-agenda-files '("~/Documents/Notes/")
	org-mobile-directory "~/Documents/Notes/.mobile"
	org-mobile-inbox-for-pull  "~/Documents/Notes/todo.org"
	org-fontify-whole-heading-line t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-src-window-setup 'current-window
  org-startup-indented t
	org-confirm-babel-evaluate nil
  org-html-htmlize-output-type nil)
  (org-indent-mode 1)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (python . t)
     (shell . t)
     (clojure . t)
     (scala . t))))

(use-package ox-gfm
  :ensure t)

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-download
  :after org
  :config
  (setq org-download-method 'attach))

(use-package htmlize
  :ensure t
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region))

(use-package ox-html
  :init
  (setq org-html-postamble t)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org-download ensime htmlize ox-gfm ox-html ox-md flymake-hlint flymake-easy hlint-refactor haskell-snippets exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
