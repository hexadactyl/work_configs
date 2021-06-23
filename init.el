;; initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			("melpa-stable" . "https://stable.melpa.org/packages/")
                        ("org" . "https://orgmode.org/elpa/")
			("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; turn off bad defaults
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell t)

;; HOW THE FUCK DOES THE GOD DAMN CLIPBOARD WORK
;(setq x-select-enable-clipboard t)

;; make escape work like it should
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; display line numbers
(column-number-mode)
(global-display-line-numbers-mode 1)

;; disable line numbers in some contexts
(dolist (mode
   '(org-mode-hook
    term-mode-hook
    eshell-mode-hook
    pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; diminish allows hiding modes in the modeline
(use-package diminish)

(use-package telephone-line
  :init (telephone-line-mode)
  :custom
  (telephone-line-primary-left-separator 'telephone-line-identity-left)
  (telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left)
  (telephone-line-primary-right-separator 'telephone-line-identity-right)
  (telephone-line-secondary-right-separator 'telephone-line-identity-hollow-right))

(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package all-the-icons)

;; set tab width
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)

;; use spaces for indentation
(setq-default indent-tabs-mode nil)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
    :init (ivy-mode)
    :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("RET" . ivy-immediate-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))

; enable fuzzy finding EVERYWHERE
;(setq ivy-re-builders-alist
      ;'((t . ivy--regex-fuzzy)))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-hydra
  :defer t
  :after hydra)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
          ("C-x b" . counsel-ibuffer)
          ("C-x C-f" . counsel-find-file)
          ("C-x z" . counsel-fzf)
          :map minibuffer-local-map
          ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package ripgrep)

(use-package helm)
  ;(use-package helm-config)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-create-definer matt/leader-keys
        :keymaps '(normal insert visual emacs)
        :prefix "SPC"
        :global-prefix "C-SPC")
  (matt/leader-keys
    "b"   '(:ignore t :which-key "buffers")
    "bb"  '(hydra/cycle-buffers/body :which-key "cycle buffers")
    "bc"  'kill-buffer-and-window
    "bk"  'kill-buffer
    "br"  'revert-buffer-noauto
    "bs"  'counsel-ibuffer
    "bx"  'kill-this-buffer

    "d"   '(:ignore t :which-key "dired")
    "dc"  'dired-config
    "dd"  'dired
    "dh"  'dired-home
    ;"dh"  (lambda nil (interactive)
    ;        (dired (getenv "HOME")))
    "dj"  'dired-jump
    "do"  'dired-jump-other-window
    "dp"  'dired-projects

    "f"   '(:ignore t :which-key "find")
    "fc"  '(counsel-fzf-config-files :which-key "fzf config dir")
    "ff"  'find-file
    "fg"  '(counsel-rg :which-key "grep")
    "fz"  'counsel-fzf

    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gd"  'magit-diff-unstaged
    "gc"  '(:ignore t :which-key "checkout")
    "gcf"  'magit-file-checkout
    "gcb"  'magit-branch-or-checkout
    "gl"  '(:ignore t :which-key "log")
    "glc" 'magit-log-current
    "glf" 'magit-log-buffer-file
    "gb"  'magit-branch
    "gP"  'magit-push-current
    "gp"  'magit-pull-branch
    "gf"  'magit-fetch
    "gF"  'magit-fetch-all
    "gr"  'magit-rebase

    "o"   '(:ignore t :which-key "open")
    "oc"  '(open-org-config :which-key "open emacs config")

    ;"p"   '(:keymap projectile-command-map :package projectile)
    ;"p"   'projectile-command-map TODO: fix this


    "w"   '(:ignore t :which-key "windows")
    "wc"  'evil-window-delete
    "wh"  'evil-window-left
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "wl"  'evil-window-right
    "wr"  '(hydra/resize-window/body :which-key "resize window")
    "ws"  'evil-window-split
    "wv"  'evil-window-vsplit
    "ww"  (lambda nil (interactive)
            (evil-window-set-height 30))

    "z"   '(hydra/text-zoom/body :which-key "zoom text")
    ";"   '(eval-config :which-key "eval config")))

; helper functions
                                        ; TODO: decide if I should use lambdas or helper functions
(defun counsel-fzf-config-files ()
  (interactive)
  (counsel-fzf nil (getenv "XDG_CONFIG_HOME")))

(defun open-org-config ()
  (interactive)
  (find-file (concat (getenv "XDG_CONFIG_HOME") "/emacs/emacs.org")))

(defun revert-buffer-noauto ()
  (interactive)
  (revert-buffer nil t t)) ;TODO: figure out args

(defun dired-home ()
  (interactive)
  (dired (getenv "HOME")))

(defun dired-config ()
  (interactive)
  (dired (getenv "XDG_CONFIG_HOME")))

(defun dired-projects ()
  (interactive)
  (dired "/home/matt/projects"))

(defun eval-config nil (interactive)
  (load-file (getenv "HOME") "/.emacs.d/init.el"))

(use-package hydra
  :defer 1)

(defhydra hydra/cycle-buffers ()
  "cycle buffers"
  ("j" next-buffer)
  ("k" previous-buffer)
  ("x" kill-this-buffer)
  ("c" kill-buffer-and-window)
  ("SPC" nil "quit" :exit t))

(defhydra hydra/resize-window (:timeout 15)
  "resize window"
  ("=" evil-window-increase-height)
  ("-" evil-window-decrease-height)
  ("." evil-window-increase-width)
  ("," evil-window-decrease-width)
  ("SPC" nil "quit" :exit t))

(defhydra hydra/text-zoom (:timeout 10)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("SPC" nil "quit" :exit t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;(use-package evil-magit
  ;:after magit)

(use-package all-the-icons-dired)

(use-package dired-single
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-ahgo --group-directories-first"))

(add-hook 'dired-mode-hook
        (lambda ()
          (all-the-icons-dired-mode 1)))

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-single-up-directory
  "l" 'dired-single-buffer
  "y" 'dired-ranger-copy
  "X" 'dired-ranger-move
  "p" 'dired-ranger-paste
  ;"j" 'peep-dired-next-file
  ;"k" 'peep-dired-prev-file
)

(use-package slime)
(setq inferior-lisp-program "clisp")
(use-package lispy)
(use-package evil-lispy)
(electric-pair-mode)

;(use-package scheme-mode
  ;:ensure nil)

(use-package racket-mode)

;(use-package geiser
  ;:config
  ;(setq geiser-default-implementation 'racket))

(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . lsp-deferred))
(use-package queue)
(use-package cider
  :bind (("c-c j" . cider-jack-in-clj)
         ("C-c C-j" . cider-jack-in-clj)))

(use-package python-mode
  :ensure t
  ;:hook (python-mode . lsp-deferred) 
  :custom (python-shell-interpreter "python3"))

; needed for org-babel
(require 'ob-js)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . matt/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(defun matt/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-ssegments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
          ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

;; Org Mode Configuration ------------------------------------------------------

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (lisp . t)
    (scheme . t)
    (python . t)
    (js . t)
    (clojure . t)
    ))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("cl" . "src lisp"))
  (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))
