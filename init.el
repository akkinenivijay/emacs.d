(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA
  ;; and MELPA Stable as desired
  (add-to-list 'package-archives
               (cons "melpa"
                     (concat proto "://melpa.org/packages/"))
               t)
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
(add-to-list 'default-frame-alist '(font . "Hack-16"))
(set-face-attribute 'default t :font  "Hack-16")

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

(global-hl-line-mode +1)

(use-package lisp-mode
  :config
  (defun vijay-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'vijay-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package avy
  :ensure t
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package aggressive-indent
  :ensure t
  :init
  (progn
    (global-aggressive-indent-mode 1)))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

; (use-package doom-modeline
;   :init
;   (setq doom-modeline-height 22)
;   (doom-modeline-init)
;   (doom-modeline-def-modeline
;    'vijay
;    '(window-number
;      bar
;      matches
;      " "
;      buffer-info
;      buffer-position
;      " "
;      selection-info)
;    '(global buffer-encoding major-mode process vcs flycheck))
;   (doom-modeline-set-modeline 'vijay t))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.15)
  (dimmer-mode))

(use-package ag
  :ensure t)

(use-package flx
  :ensure t)

(use-package smex)

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (setq ivy-height 30)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper))
  :diminish)

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

(use-package counsel
  :ensure t
  :after ivy
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

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "s-w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

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

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))

(use-package easy-kill
  :bind (([remap kill-ring-save] . easy-kill)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package flycheck-joker
  :ensure t
  :pin melpa-stable)

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package yaml-mode
  :ensure t)

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
  :after clojure-mode
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r"))

(use-package elixir-mode
  :mode ("\\.exs\\'" "\\.ex\\'")
  :config
  (add-hook 'elixir-mode #'subword-mode))

(use-package alchemist
  :after elixir-mode
  :config
  (setq alchemist-mix-command "/usr/local/bin/mix")
  (setq alchemist-execute-command "/usr/local/bin/elixir")
  (setq alchemist-iex-program-name "/usr/local/bin/iex")
  (setq alchemist-execute-command "/usr/local/bin/elixir")
  (setq alchemist-compile-command "/usr/local/bin/elixirc")
  (setq alchemist-hooks-compile-on-save t))

(use-package haskell-mode
  :mode ("\\.lhs\\'" "\\.hs\\'")
  :config
  ; (add-hook 'haskell-mode-hook
  ;           (lambda ()
  ;             (set (make-local-variable 'company-backends)
  ;                  (append '((company-capf company-dabbrev-code))
  ;                          company-backends))))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode))

(use-package hindent
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package intero
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package utop
  :config
  (setq utop-command "opam config exec -- utop -emacs"))

(use-package ocp-indent)

(use-package tuareg
  :init
  (setq tuareg-indent-align-with-first-arg t)
  (setq auto-mode-alist
      (append '(("_oasis\\'" . conf-mode)
    ("_tags\\'" . conf-mode)
    ("_log\\'" . conf-mode))
        auto-mode-alist))
  :config
  (add-hook 'tuareg-mode-hook
            ;; Turn on auto-fill minor mode.
            (lambda ()
              (when (functionp 'prettify-symbols-mode)
                (prettify-symbols-mode))
              (auto-fill-mode 1)
              (add-hook 'before-save-hook 'ocp-indent-buffer (merlin-mode))))
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  (add-hook 'tuareg-mode-hook 'merlin-mode))

(use-package merlin
  :after tuareg
  :init
  (add-to-list 'auto-mode-alist '("/\\.merlin\\'" . conf-mode))
  :config
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (add-hook 'merlin-mode-hook 'company-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF"])
 '(custom-safe-themes
   (quote
    ("6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(package-selected-packages
   (quote
    (ivy-rich smex dimmer doom-modeline doom-themes flycheck flycheck-joker intero haskell-mode cider clojure-mode paredit elisp-slime-nav move-text adoc-mode markdown-mode hl-todo imenu-anywhere super-save diff-hl undo-tree volatile-highlights crux company yaml-mode exec-path-from-shell easy-kill anzu git-timemachine expand-region ace-window spacemacs-theme flx projectile ag try counsel which-key magit zenburn-theme aggressive-indent rainbow-delimiters rainbow-identifiers use-package)))
 '(vc-annotate-background "#282c34")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#e6ab6a")
    (cons 120 "#e09859")
    (cons 140 "#da8548")
    (cons 160 "#d38079")
    (cons 180 "#cc7cab")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#cf6162")
    (cons 300 "#9f585a")
    (cons 320 "#6f4e52")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
