;; Load config
;; (org-babel-load-file "~/.emacs.d/README.org")

;; UI improvements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 0)
(blink-cursor-mode -1)
(electric-pair-mode 1)
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
  :config
  ;; constant width
  ;; https://github.com/tumashu/ivy-posframe/issues/105
  (defun da/ivy-posframe-get-size ()
    "Set the ivy-posframe size according to the current frame."
    (let ((height (or ivy-posframe-height (or ivy-height 10)))
          (width (min (or ivy-posframe-width 200) (round (* .60 (frame-width))))))
      (list :height height :width width :min-height height :min-width width)))

  (setq ivy-posframe-size-function 'da/ivy-posframe-get-size)

  :custom (ivy-posframe-display-functions-alist
            '((swiper          . ivy-posframe-display-at-frame-center)
              (complete-symbol . ivy-posframe-display-at-frame-center)
              (counsel-M-x     . ivy-posframe-display-at-frame-center)
              (t               . ivy-posframe-display))))

;; icons
(use-package all-the-icons)

;; theme
(use-package doom-themes
  :init (load-theme 'doom-snazzy t))

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
  :after evil magit
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
(defun da/save-all () (interactive) (save-some-buffers t))
(use-package general
  :config
  (general-create-definer my/leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (my/leader
    "w"   'save-buffer
    "SPC" 'da/save-all
    "q"   'delete-window
    "s"   'vterm
    "b"   'counsel-ibuffer
    "g"   'magit
    "a"   'org-agenda
    "c"   'org-capture
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

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;; org mode
(defun da/org-mode-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Hide the ugly stuff
  (setq org-hide-emphasis-markers t)

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.05)
                  (org-level-2 . 1.05)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 110)

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun da/org-mode-hook ()
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode 0))

(use-package org
  :hook (org-mode . da/org-mode-hook)
  :config
  (setq org-ellipsis " ▾")

  ;; Agenda
  (setq org-agenda-files (directory-files-recursively "~/notes/" "\\.org$"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Capture
  (setq org-default-notes-file "~/notes/capture.org")
  (setq org-capture-templates
      '(("l" "Link" entry (file+datetree "~/notes/links.org")
         "* %?\nEntered on %U\n  %i\n")))

  (da/org-mode-font-setup))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("●" "●" "●" "●" "●" "●" "●")))

(defun da/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . da/org-mode-visual-fill))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
