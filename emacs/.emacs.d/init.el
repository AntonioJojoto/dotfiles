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
;(straight-use-package 'org-roam-server)
(straight-use-package 'org-attach-screenshot)
(straight-use-package 'org-ref)
(straight-use-package 'org-fragtog)
(straight-use-package 'org-roam-ui)

; evil-mode
(straight-use-package 'evil)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-org)
; functionality
(straight-use-package 'helm)
(straight-use-package 'counsel)
(straight-use-package 'ivy)
(straight-use-package 'ivy-rich)
(straight-use-package 'all-the-icons-ivy-rich)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-bibtex)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'general)
(straight-use-package 'cheat-sh)
; gui
(straight-use-package 'dashboard)
(straight-use-package 'which-key)
(straight-use-package 'all-the-icons)
(straight-use-package 'page-break-lines)
(straight-use-package 'doom-themes)
(straight-use-package 'doom-modeline)
(straight-use-package 'dimmer)
; programming
;(straight-use-package 'flycheck)
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

; AMPL in emacs
(load-file "ampl-mode.elc")
(setq auto-mode-alist
      (cons '("\\.mod$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.dat$" . ampl-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.ampl$" . ampl-mode) auto-mode-alist))
(setq interpreter-mode-alist
      (cons '("ampl" . ampl-mode)
            interpreter-mode-alist))
(autoload 'ampl-mode "ampl-mode" "Ampl editing mode." t)


(require 'cmake-mode)
;; documents
(straight-use-package 'pdf-tools)
(straight-use-package 'auctex)
(straight-use-package 'cdlatex)

;; hide GUI
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; doom themes
(require 'doom-themes)
(load-theme 'doom-material t)
(doom-modeline-mode 1)
(setq doom-modeline-height 45)



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

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

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

;; Preview all latex fragments
(defun latex-preview-all()
  (interactive)
  (setq init-point (point)) ; This is where we start
  (goto-char 1) ; Go to the beginning of the document
  (while (< (point) (point-max)) ; Until the end of the document is reached
    (call-interactively 'org-next-visible-heading) ; Go to the next heading
    (call-interactively 'org-latex-preview) ; Preview the whole heading
    )
  ; Go to the point at which the function was called
  (goto-char init-point)) 


;; font size
(set-face-attribute 'default nil :family "Iosevka" :height 140)

;; Line highlighting
(global-hl-line-mode 0)

;; Show matching parenthesis
(show-paren-mode 1)

;; backup files to another folder
(setq backup-directory-alist `(("." . "~/.saves")))

;; evil mode
(require 'evil)
(setq evil-want-C-u-scroll t)   ; use C-u to scroll up in normal mode
(require 'evil-nerd-commenter)
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)
(global-evil-leader-mode)
(evil-mode 1)
(evil-snipe-mode 1)

;; Company
(require 'company)
(with-eval-after-load 'company)
  ;; (define-key company-active-map (kbd "M-n") nil)
  ;; (define-key company-active-map (kbd "M-p") nil)
  ;; (define-key company-active-map (kbd "C-n") #'company-select-next)
  ;; (define-key company-active-map (kbd "C-p") #'company-select-previous))
(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map (kbd "C-.") 'company-files)

;; Yasnippet
(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/dotfiles/emacs/snippets"                 ;; personal snippets
        "~/.emacs.d/straight/build/yasnippet-snippets/snippets"           ;; foo-mode and bar-mode snippet collection
;;      "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
        ))

(yas-global-mode 1)

;; LSP
(setq gc-cons-threshold 100000000) ; needed because communication generates a lot of garbage
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)
(add-hook 'python-mode-hook #'lsp)
(add-hook 'elisp-mode-hook #'lsp)
; run  npm i -g bash-language-server
(add-hook 'bash-mode-hook #'lsp)
(setq lsp-enable-snippet t)
(require 'lsp-python-ms)
(setq lsp-python-ms-auto-install-server t)
(setq python-shell-interpreter "python")

;; Use ivy for searching and launching
;; And also helm
(require 'ivy)
(require 'ivy-rich)
(ivy-rich-mode 1)
(all-the-icons-ivy-rich-mode 1)

; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-c p") 'counsel-descbinds)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
; Helm key bindings
(helm-mode t)
(global-set-key (kbd "C-x k") 'kill-buffer)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c b b") 'helm-bookmarks)
(global-set-key (kbd "C-c b c") 'org-capture)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-z") 'helm-all-mark-rings)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-c") 'counsel-M-x)

; Helpful key bindings
(global-set-key (kbd "<f1> f") 'describe-function)
(global-set-key (kbd "<f1> v") 'describe-variable)



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

; org-agenda
(setq org-agenda-files '("~/.org/agenda/"))
(setq org-agenda-span 17
      org-agenda-start-day "-3d")

;(setq org-agenda-start-with-log-mode t)
;(setq org-log-done 'time)
;(setq org-log-into-drawer t)

; org-archive
(setq org-archive-location "~/.org/agenda/archive.org::* From %s")
(global-set-key (kbd "<f1> v") 'org-archive-subtree-default)

; org-refile
(setq org-refile-targets '((nil :maxlevel . 2)
                                (org-agenda-files :maxlevel . 2)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t) 

; org capture templates
(setq org-capture-templates
      '(("t" "Recordatorio" entry (file "~/.org/agenda/recordar.org")
         "* TODO %?\n  %i\n  %a")
        ("s" "Braindump" entry (file "~/.org/braindump.org")
         "* TODO %?\n  %i\n  %a")
	("c" "Cookbook" entry (file "~/.org/cookbook.org")
         "* %^{Recipe title: }\n** Propierties\n- Servings: \n- Prep-Time: \n** Ingredients\n%?\n** Directions\n\n")))



;; org mode enhanced visuals
(setq org-ellipsis " ▾")
; We wont see the markers of italic, bold, etc.
 (setq org-hide-emphasis-markers t) 

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

; change the rendering of the headings
(dolist (face '((org-level-1 . 1.2)
                      (org-level-2 . 1.1)
                      (org-level-3 . 1.05)
                      (org-level-4 . 1.0)
                      (org-level-5 . 1.1)
                      (org-level-6 . 1.1)
                      (org-level-7 . 1.1)
                      (org-level-8 . 1.1)))
        (set-face-attribute (car face) nil :font "Iosevka Heavy" :weight 'medium :height (cdr face)))


;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


; turn on 'org-indent-mode' by default
(setq org-startup-indented t)
(add-hook 'org-mode-hook 'turn-on-org-cdlatex) 
(add-hook 'org-mode-hook 'visual-line-mode) 
(add-hook 'latex-mode-hook 'turn-on-cdlatex)


; turn on org latex preview by default
(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook 'latex-preview-all) ; homemade function

; scale to display org latex preview
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

; update latex preview size with font size
(defun update-org-latex-fragments ()
  (org-latex-preview '(64))
  (plist-put org-format-latex-options :scale text-scale-mode-amount)
  (org-latex-preview '(16)))
(add-hook 'text-scale-mode-hook 'update-org-latex-fragments)

; org-ref and bibtex
; place where the bibliografy will be stored
(require 'org-ref)
; main .bib file
(setq reftex-default-bibliography '("~/.org/references.bib"))
(setq org-ref-default-bibliography '("~/.org/references.bib"))
; folder where pdf will be downloaded
(setq org-ref-pdf- '("~/.org/bib_pdfs/"))
; file where citation notes will be stored
(setq org-ref-bibliografy-notes '("~/.org/bib_notes.org"))
(setq org-latex-prefer-user-labels t)
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

(require 'helm-bibtex)
(setq bibtex-completion-bibliography '("~/.org/references.bib"))




;; Function to remove underscore from strings
; screenshots using flameshot
(setq org-attach-screenshot-command-line "emacshot %f")    
(setq org-attach-screenshot-relative-links t)
(setq org-attach-screenshot-insertfunction
      (lambda (linkfilename)
       (insert (concat "#+CAPTION:" (substring linkfilename 7 -4) "\n[[file:"  (replace-regexp-in-string " " "_" linkfilename) "]]\n")) ))

; org-babel for souce blocks
(org-babel-do-load-languages 'org-babel-load-languages
			     '((python . t)
			       (C .t)
			       (emacs-lisp . t)
			       (shell . t)
			       ))

; do not ask for confirmation when running code blocks
(setq org-confirm-babel-evaluate nil)


; org-roam
(require 'org-roam)
(setq org-roam-v2-ack t)
(setq org-roam-directory "~/.org/roam")
(setq org-roam-dailies-directory "journal/")
(org-roam-setup)
(setq org-roam-completion-system 'helm)
(global-set-key (kbd "C-c r d") 'org-roam-doctor)
(global-set-key (kbd "C-c r f") 'org-roam-node-find)
(global-set-key (kbd "C-c r g") 'org-roam-graph)
(global-set-key (kbd "C-c r i") 'org-roam-node-insert)
(global-set-key (kbd "C-c r r") 'org-roam-find-ref)
(global-set-key (kbd "C-c r b") 'org-roam-buffer-toggle)
(global-set-key (kbd "C-c r c") 'org-id-get-create)


; capture templates
(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("c" "clean" plain "%?"
         :if-new
         (file+head "clean/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
	("u" "Uni" plain "%?"
         :if-new
         (file+head "Uni/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
	("e" "Resumen" plain "%?"
         :if-new
         (file+head "clean_journal/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)))

; Creating the propierty type in the nodes
(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))

; Now the display shows the type of zettel and the tags
(setq org-roam-node-display-template
          (concat "${type:15} ${title:*}" (propertize "${tags:10}" 'face 'org-tag)))


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
  ;; org roam
  "aa" '(org-roam-alias-add :which-key "Roam Alias Add")
  "ad" '(org-roam-alias-remove :which-key "Roam Alias Remove")
  "ta" '(org-roam-tag-add :which-key "Roam Tag Add")
  "td" '(org-roam-tag-remove :which-key "Roam Tag Remove")
  "ra" '(org-roam-ref-add :which-key "Roam Ref Add")
  "rd" '(org-roam-ref-remove :which-key "Roam Ref Remove")

  "dt" '(org-roam-dailies-capture-today :which-key "Dailies Capture Today")
  "dy" '(org-roam-dailies-capture-yesterday :which-key "Dailies Capture Yesterday")
  "gt" '(org-roam-dailies-goto-today :which-key "Dailies goto today")
  "gy" '(org-roam-dailies-goto-yesterday :which-key "Dailies goto yesterday")
  "gd" '(org-roam-dailies-goto-date :which-key "Dailies goto date")
  "gl" '(org-roam-dailies-goto-next-note :which-key "Dailies goto next note")
  "gh" '(org-roam-dailies-goto-previous-note :which-key "Dailies goto previous note")
  ;; magit
  "m" '(magit :which-key "magit")
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
  "cc" '(nc-copy-and-comment-lines :which-key "copy & comment")
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
  ;; agenda files
  "aa" '(org-agenda :which-key "open agenda menu")
  ;; helm-projectile
  "pp" '(helm-projectile-switch-project :which-key "switch project")
  "pf" '(helm-projectile-find-file :which-key "find file")
  "pd" '(helm-projectile-dir :which-key "find dir")
  "pg" '(helm-projectile-grep :which-key "grep in project")
  "pb" '(helm-projectile-switch-to-buffer :which-key "switch buffer")
  ;; ayuda para tomar apuntes
  ;;"ar" '(org-ref-helm-insert-ref-link :which-key "org-ref insert link")
  ;;"af" '(org-fragtog-mode :which-key "Toggle fragtop mode")
  
			      
  ;; ...
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" default))
 '(warning-suppress-log-types
   '((websocket)
     (websocket)
     (websocket)
     (org-element-cache)
     (auto-save)
     (comp)))
 '(warning-suppress-types
   '((websocket)
     (websocket)
     (org-element-cache)
     (auto-save)
     (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
