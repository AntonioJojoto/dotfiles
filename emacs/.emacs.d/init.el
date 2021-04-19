;; Straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; packages
; shallow clones not working properly right now
; (setq straight-vc-git-default-clone-depth 1)
; org mode
(straight-use-package 'org-bullets)
(straight-use-package 'org-roam)
(straight-use-package 'org-roam-server)
(straight-use-package 'org-attach-screenshot)
; evil-mode
(straight-use-package 'evil)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-org)
; functionality
(straight-use-package 'helm)
(straight-use-package 'helm-projectile)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'general)
(straight-use-package 'avy)
; gui
(straight-use-package 'dashboard)
(straight-use-package 'which-key)
(straight-use-package 'all-the-icons)
(straight-use-package 'page-break-lines)
(straight-use-package 'telephone-line)
(straight-use-package 'doom-themes)
(straight-use-package 'dimmer)
; programming
;; (straight-use-package 'flycheck)
(straight-use-package 'lsp-mode
		      :init
		      (setq lsp-keymap-prefix "C-c l")
		      )
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-python-ms)
(straight-use-package 'company)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'cmake-mode)
(require 'cmake-mode)
;; documents
(straight-use-package 'pdf-tools)
(straight-use-package 'auctex)
;; dictionary
(straight-use-package 'synosaurus)

;; Other
; Jupyter Notebooks in emacs
(straight-use-package 'ein)
(straight-use-package 'markdown-mode)

;; hide GUI
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; hide scroll bar in new frames
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

;; scroll off
(setq scroll-margin 7)
(setq scroll-conservatively 999) ;; do not center when scrolling

;; disable bell
(setq ring-bell-function 'ignore)

;; Line numbering
(add-hook 'prog-mode-hook 'display-line-numbers-mode)	; only show numbers in programming buffers
(setq display-line-numbers-type 'relative)		; relative line numbers

;; Linebreaks

;; Spellcheck
(defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "es") "english" "es")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))

; Get the next spell error and prompts the correct possibilities
; When invoking commands that require some input arguments, run as interactively
(defun f1()
  (interactive)
  (call-interactively 'evil-prev-flyspell-error) ; go to the next error
  (call-interactively 'ispell-word))             ; and prompt the correction

; Get the next spell error and prompts the correct possibilities
(defun f2()
  (interactive)
  (call-interactively 'evil-next-flyspell-error) ; go to the next error
  (call-interactively 'ispell-word))             ; and prompt the correction


;; font size
(set-face-attribute 'default nil :family "Iosevka" :height 130)

;; Line highlighting
(global-hl-line-mode 1)

;; Show matching parenthesis
(show-paren-mode 1)

;; backup files to another folder
(setq backup-directory-alist `(("." . "~/.saves")))

;; evil mode
(setq evil-want-C-u-scroll t)   ; use C-u to scroll up in normal mode
(require 'evil)
(require 'evil-numbers)
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
(require 'evil-nerd-commenter)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
(require 'evil-surround)
(global-evil-leader-mode)
(global-evil-surround-mode 1)
(evil-mode 1)
(evil-snipe-mode 1)

;; Company
(require 'company)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map (kbd "C-.") 'company-files)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; LSP
(setq gc-cons-threshold 100000000) ; needed because communication generates a lot of garbage
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'rust-mode-hook #'lsp)
; run  npm i -g bash-language-server
(add-hook 'bash-mode-hook #'lsp)
(setq lsp-enable-snippet t)
(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(setq python-shell-interpreter "python")

;; helm mode
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list) ;; List buffers ( Emacs way )
(global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
(global-set-key (kbd "C-x C-f") 'helm-find-files) ;; Finding files with Helm
(global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
(global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
(global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
(global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to pastelm-mode 1)
(require 'helm-projectile)
(helm-projectile-on)

;; projectile
(projectile-mode 1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; which key
(require 'which-key)
(which-key-mode 1)

;; dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info t)
(setq dashboard-projects-backend 'projectile)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
;; avy
(avy-setup-default)

;; modeline
;; for telephone-line configuration needs to be before (telephhone-line-mode 1)
(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-right
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
(setq telephone-line-height 24)
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-buffer-segment
		   telephone-line-minor-mode-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))
(telephone-line-mode 1)

;; doom themes
(load-theme 'doom-gruvbox t)

;; dimmer
(require 'dimmer)
(dimmer-configure-which-key)
(dimmer-configure-helm)
(dimmer-configure-magit)
(dimmer-configure-which-key)
(dimmer-configure-org)
(dimmer-mode t)

;; org mode
(setq org-directory "~/.org") ; main org directory
(setq org-agenda-files '("~/.org/agenda"))
(setq org-agenda-span 17
      org-agenda-start-day "-3d")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

; screenshots using flameshot
(setq org-attach-screenshot-command-line "emacshot %f")    
(setq org-attach-screenshot-relative-links t)
(setq org-attach-screenshot-insertfunction
      (lambda (linkfilename)
       (insert (concat "[[file:" linkfilename "]]\n\n")) ))

; org-babel for souce blocks
(org-babel-do-load-languages 'org-babel-load-languages
			     '((python . t)
			       (C .t)
			       (shell .t)
			       (emacs-lisp . t)
			       ;; (arduino . t)
			       (octave . t)
			       (org . t)
			       (matlab . t)
			       ))


; org-roam
(require 'org-roam)
(setq org-roam-directory "~/.org/roam")
(setq org-roam-db-location "~/.org/roam.db")
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-completion-system 'helm)
(global-set-key (kbd "C-c r b") 'org-roam)
(global-set-key (kbd "C-c r c") 'org-roam-capture)
(global-set-key (kbd "C-c r d") 'org-roam-doctor)
(global-set-key (kbd "C-c r f") 'org-roam-find-file)
(global-set-key (kbd "C-c r g") 'org-roam-graph)
(global-set-key (kbd "C-c r i") 'org-roam-insert)
(global-set-key (kbd "C-c r m") 'org-roam-mode)
(global-set-key (kbd "C-c r r") 'org-roam-find-ref)
(global-set-key (kbd "C-c r t") 'org-roam-buffer-toggle-display)

(define-key global-map "\C-cc" 'org-capture)
;; configure org capture templates
(setq org-capture-templates
'(("t"               ; hotkey
	 "Todo list item"  ; name
	 entry             ; type
					; heading type and title
	 (file+headline "~/.org/agenda/trabajo.org" "Pa aburrirme")
	 "* TODO %?\n %i\n %a") ; template

	;;
	("w"               ; hotkey
	 "Grupo Investigación"  ; name
	 entry             ; type
	 (file+headline "~/.org/agenda/trabajo.org" "Grupo de Investigación Diabetes")
	 "* TODO %?\n %i\n %a") ; template


	;;
	("f"               ; hotkey
	 "TFG Entry"  ; name
	 entry             ; type
	 (file+headline "~/.org/agenda/trabajo.org" "TFG")
	 "* TODO %?\n %i\n %a") ; template

	;; 
	("u"               ; hotkey
	 "University todo item"  ; name
	 entry             ; type
	 (file+headline "~/.org/agenda/notes.org" "Universidad")
	 "* TODO %?\n %i") ; template

	;; 
	("o"               ; hotkey
	 "Other Entry"  ; name
	 entry             ; type
	 (file+headline "~/.org/agenda/notes.org" "Otros")
	 "* %?\n %i") ; template
	
	;;
	("b"
	 "BibTex reference"
	 plain
	 (file "~/.org/references.bib")
	 "%i\n\n")

	
	;;
	("c"               ; hotkey
	 "Chuletario Entry"  ; name
	 entry             ; type
	 (file+headline "~/.org/agenda/Chuletas.org" "Emacs")
	 "* %?\n %i") ; template
	))


; org-refile targets
; several files can be added
(setq org-refile-targets '(
	(org-agenda-files . (:maxlevel . 6)) ; all agenda files folder
	("~/.org/roam/20210414004403-emacs_tutorial.org" . (:maxlevel . 3)) ; any doubt about emacs shall land here
	("~/.org/roam/20210413202926-linux_problems.org" . (:maxlevel . 3)) ; problems of linux
	))

; org-roam-server
(require 'org-roam-server)
(setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 7777
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)

;; pdf tools
(pdf-tools-install)
;; (evil-set-initial-state 'pdf-view-mode 'normal)

;; auctex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
; revert the PDF-buffer after the TeX compilation has finished
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

;; keybinds
(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
  "'" '(term :which-key "iterm")
  ;; magit
  "m" '(magit :which-key "magit")
  ;; buffers
  "bn" '(evil-next-buffer :which-key "next buffer")
  "bp" '(evil-previous-buffer :which-key "previous buffer")
  "bd" '(evil-delete-buffer :which-key "delete buffer")
  "bb" '(helm-buffers-list :which-key "change buffer")
  ;; Visual toggles
  "vl" '(display-line-numbers-mode :which-key "toggle line numbers")
  "vt" '(toggle-truncate-lines :which-key "toggle truncated lines")
  "vv" '(visual-line-mode :which-key "toggle visual mode")
  ;; windows
  "wj" '(evil-window-down :which-key "window down")
  "wk" '(evil-window-up :which-key "window up")
  "wh" '(evil-window-left :which-key "window left")
  "wl" '(evil-window-right :which-key "window right")
  "ws" '(evil-window-split :which-key "window split")
  "wv" '(evil-window-vsplit :which-key "window vsplit")
  "wd" '(evil-window-delete :which-key "window delete")
  ;; tabs
  "tn" '(tab-new :which-key "tab new")
  "th" '(tab-previous :which-key "tab previous")
  "tl" '(tab-next :which-key "tab next")
  "tc" '(tab-close :which-key "tab close")
  ;; evil-commenter
  "ci" '(evilnc-comment-or-uncomment-lines :which-key "(un)comment line")
  "cl" '(evilnc-quick-comment-or-uncomment-to-the-line :which-key "(un)comment to the line")
  "cc" '(evilnc-copy-and-comment-lines :which-key "copy & comment")
  "cp" '(evilnc-comment-or-uncomment-paragraphs :which-key "(un)comment parahraphs")
  "cr" '(comment-or-uncomment-region :which-key "(un)comment region")
  "cv" '(evilnc-toggle-invert-comment-line-by-line :which-key "invert comment by line")
  "\\" '(evilnc-comment-operator :which-key "comment operator")
  ;; avy
  ";"  '(avy-goto-char-timer :which-key "avy char timer")
  ;; term
  "tt" '(term :which-key "shell")
  ;; Dictionary
  "sd" '(fd-switch-dictionary :which-key "toggle language")
  "se" '(flyspell-mode :which-key "toggle flyspell ON/OFF")
  "sh" '(f1 :which-key "previous spell error") 
  "sl" '(f2 :which-key "next spell error")    
  "kr" '((lambda () (interactive) (load-file "~/.emacs.d/init.el")):which-key "reload emacs init file")
			      
  ;; ...
)
;; (general-nvmap
;;  "'" (general-simulate-keys "C-c")
;;  "M-'" 'evil-goto-mark
;;  "b" 'helm-buffers-list
;;  ;; ...
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/.org/agenda/trabajo.org" "~/.org/agenda/Universidad.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
