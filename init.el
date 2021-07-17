;; UI improvements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)
(blink-cursor-mode -1)
(toggle-frame-fullscreen)

;; Backups are irritating
(setq make-backup-files nil)
(setq backup-directory-alist '("" . (expand-file-name ".emacs-backups" "~")))

;; Don't want an audible bell
(setq visible-bell t)

;; Nicer font
(set-face-attribute 'default nil :font "VictorMono Nerd Font Mono" :height 100)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; custom-set-variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Line and column numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
		vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1)
  :custom (ivy-count-format "(%d/%d)"))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :init (ivy-posframe-mode 1)
  :custom (ivy-posframe-display-functions-alist
            '((swiper          . ivy-posframe-display-at-frame-center)
              (complete-symbol . ivy-posframe-display-at-frame-center)
              (counsel-M-x     . ivy-posframe-display-at-frame-center)
              (t               . ivy-posframe-display))))

;; icons
(use-package all-the-icons)

;; theme
(use-package doom-themes
  :init (load-theme 'doom-monokai-classic t))

;; modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
	(display-time-mode 1)
	(set-face-attribute 'mode-line nil :height 90)
  :custom ((doom-modeline-height 30)
	   (doom-modeline-bar-width 10)
	   (display-time-24hr-format t)
	   (display-time-default-load-average nil)))

;; color brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; remember keybindings
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

;; better help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; evil stuff
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq-default evil-escape-key-sequence "jk")
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil keybindings for common emacs modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; jk to escape
(use-package evil-escape
  :init (setq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode))

;; surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; better keybindings
(use-package general
  :config
  (general-create-definer my/leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (my/leader
    "w"   'save-buffer
    "SPC" 'save-buffer
    "q"   'delete-window
    "s"   'vterm
    "g"   'magit
    "x"   'counsel-M-x
    "p"   'counsel-find-file
    "t"   'counsel-find-file))

;; better terminal
(use-package vterm
  :ensure t
  :config
  (add-hook 'vterm-mode-hook
     (function
      (lambda ()
        (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

;; fish syntax
(use-package fish-mode
    :ensure t)

;; markdown mode
(use-package markdown-mode
  :ensure t)

;; magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; (use-package evil-magit
;;   :after magit)

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))
