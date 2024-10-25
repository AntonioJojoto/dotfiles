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
(straight-use-package 'org-bullets)
(straight-use-package 'org-roam)
(straight-use-package 'org-attach-screenshot)
(straight-use-package 'org-ref)
(straight-use-package 'org-fragtog)
(straight-use-package 'consult-org-roam)
(straight-use-package 'org-roam-ui)


;; org roam ui configuration
(setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t)

; evil-mode
(straight-use-package 'evil)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-leader)
(straight-use-package 'evil-numbers)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-org)

; functionality
(straight-use-package 'helm)
(straight-use-package 'helm-company)
(straight-use-package 'helm-rg)
(straight-use-package 'company-quickhelp)
(straight-use-package 'consult)
(straight-use-package 'counsel)
(straight-use-package 'ivy)
(straight-use-package 'ivy-rich)
(straight-use-package 'all-the-icons-ivy-rich)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-bibtex)
(straight-use-package 'magit)
(straight-use-package 'projectile)
(straight-use-package 'general)

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
(straight-use-package 'company)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'cmake-mode)

(require 'cmake-mode)
;; documents
;; (straight-use-package 'pdf-tools)
(straight-use-package 'auctex)
(straight-use-package 'cdlatex)
(straight-use-package 'xenops)

;; Load custom functions
(load "~/.emacs.d/useful_functions.el")


;; hide GUI
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; doom themes
(require 'doom-themes)
(load-theme 'doom-material t)
(doom-modeline-mode 1)
(setq doom-modeline-height 45)

;; This is to prevent errors with syncthing 
(setq create-lockfiles nil)

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
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;      IDE Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Eglot and hooks
(require 'eglot)


;; Company
(require 'company)
(with-eval-after-load 'company)
  ;; (define-key company-active-map (kbd "M-n") nil)
  ;; (define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)
;; Do you want company mode active on all the buffer? Like org?
(add-hook 'after-init-hook 'global-company-mode)
(define-key global-map (kbd "C-.") 'company-files)
(setq company-dabbrev-minimum-length 1)
(setq company-idle-delay 0.1)
;; (add-to-list 'company-backends 'company-yasnippet)
;; Consult todo uses keywords todo, hack, 
(straight-use-package 'consult-todo)
(setq consult-todo-only-comment t)
(global-set-key (kbd "C-c e t") 'consult-todo)
(global-set-key (kbd "C-c e T") 'consult-todo-project)


;; C 
; add eglot and company when calling treesiter
(use-package c-ts-mode
  :hook ((c-ts-mode . eglot-ensure)
	 (c-ts-mode . company-mode))
  )
; begin ts when c mode
(add-hook 'c-mode-hook 'c-ts-mode)

;; C++
(use-package c++-ts-mode
  :hook ((c++-ts-mode . eglot-ensure)
	 (c++-ts-mode . company-mode))
  )
(add-hook 'c++-mode-hook 'c++-ts-mode)

;; python

(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package python-ts-mode
  :hook ((python-ts-mode . eglot-ensure)
	 (python-ts-mode . company-mode))
  :mode (("\\.py\\'" . python-ts-mode))
  )

(straight-use-package 'pyvenv)
(pyvenv-mode 1)

;; LaTeX
(add-hook 'latex-mode-hook 'turn-on-cdlatex)
(add-hook 'LaTeX-mode-hook 'eglot-ensure)
(add-hook 'LaTeX-mode-hook 'company-mode)
(require 'tex)
(add-hook 'latex-mode-hook #'xenops-mode)
(add-hook 'LaTeX-mode-hook #'xenops-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Use pdflatex and open output with zathura
(setq TeX-view-program-selection '((output-pdf "Zathura")))
(setq TeX-view-program-list
      '(("Zathura" "zathura %o")))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(setq reftex-bibpath-environment-variables
      '("~/3ro_Carrera/Thesis/references.bib"))

;; keybindings for programming mode
(global-set-key (kbd "C-c e a") 'eglot-code-actions)
(global-set-key (kbd "C-c e r") 'eglot-rename)
(global-set-key (kbd "C-c e d") 'eldoc)
(global-set-key (kbd "C-c e f") 'eglot-format)
(global-set-key (kbd "C-c e F") 'eglot-format-buffer)
(global-set-key (kbd "C-c e d") 'xref-find-definitions)
(global-set-key (kbd "C-c e d") 'xref-find-definitions)
(global-set-key (kbd "C-c e m") 'consult-imenu-multi)
(global-set-key (kbd "C-c e e") 'consult-flymake)
(global-set-key (kbd "C-c e t") 'reftex)
(global-set-key (kbd "<f5> c") 'recompile)

;; (global-set-key (kbd "<f5> c") 'projectile-compile-project)

;; elisp
;; (add-hook 'emacs-lisp-mode 'company-mode)

;; Use ivy for searching and launching
;; And also helm
(require 'ivy)
(require 'ivy-rich)
(ivy-rich-mode 1)
(all-the-icons-ivy-rich-mode 1)

; Ivy-based interface to standard commands
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c n f") 'counsel-fzf)
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
(setq org-refile-targets '(
			   ("~/.org/agenda/RL.org" :maxlevel . 1)
			   ;; ("~/.org/agenda/todo.org" :maxlevel . 1)
			   ;; ("~/.org/agenda/someday.org" :maxlevel . 1)
			   ))

(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t) 

; org capture templates
(setq org-capture-templates
      '(	;; Also includes a link to where the capture has been done

	;; Inbox Notes
        ("i" "Inbox" entry (file "~/.org/agenda/Inbox.org")
         "* TODO %?\n  %i\n")

	;; Inbox with link (useful for code or things in roam)
	;; that is what %a does
        ("l" "Inbox with link" entry (file "~/.org/agenda/Inbox.org")
         "* TODO %?\n  %i\n %a")

	;; someday Notes
        ("o" "someday" entry (file "~/.org/agenda/someday.org")
         "* TODO %?\n  %i\n")

	;; someday with link (useful for code or things in roam)
	;; that is what %a does
        (";" "someday with link" entry (file "~/.org/agenda/someday.org")
         "* TODO %?\n  %i\n %a")

	;; BibTeX entry
	("b"
	 "BibTex reference"
	 plain
	 (file "~/.org/references.bib")
	 "%i\n\n")

	;; Thesis TODO
	("t" "Thesis Inbox" entry (file+headline "~/.org/agenda/RL.org" "Inbox")  
         "** TODO %?\n  %i\n")

	;; Thesis TODO with link
	("T" "Thesis Inbox" entry (file+headline "~/.org/agenda/RL.org" "Inbox")  
         "** TODO %?\n  %i\n %a")

	))



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
(setq reftex-default-bibliography '("~/.org/current_refs.bib"))
(setq org-ref-default-bibliography '("~/.org/current_refs.bib"))
; folder where pdf will be downloaded
(setq org-ref-pdf- '("~/.org/bib_pdfs/"))
; file where citation notes will be stored
(setq org-ref-bibliografy-notes '("~/.org/bib_notes.org"))
(setq org-latex-prefer-user-labels t)

; commands for exporting the bibtex
(setq org-latex-pdf-process
    '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

(require 'helm-bibtex)
(setq bibtex-completion-bibliography '("~/.org/current_refs.bib"))




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

;; searching the org-roam database
(require 'consult-org-roam)
;; Activate the minor mode
(consult-org-roam-mode 1)
;; Use `ripgrep' for searching with `consult-org-roam-search'
(setq consult-org-roam-grep-func #'consult-grep)
;; Configure a custom narrow key for `consult-buffer'
(setq consult-org-roam-buffer-narrow-key ?r)
;; Display org-roam buffers right after non-org-roam buffers
;; in consult-buffer (and not down at the bottom)
(setq consult-org-roam-buffer-after-buffers t)
;; Eventually suppress previewing for certain functions
; Este no vale nada porque es como roam-find
(global-set-key (kbd "C-c n b") 'consult-org-roam-backlinks)
(global-set-key (kbd "C-c n f") 'consult-org-roam-forward-links)
(global-set-key (kbd "C-c n s") 'rg-roam-search)




; capture templates
(setq org-roam-capture-templates
      ;; Note summarizing a single idea, references other ones
      '(("p" "Permanent note" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)

	;; Literature notes: Condensed notes on a book
	("l" "Literature Note" plain
         "%?"
         :if-new (file+head "literature/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)


	;; Only a small reference to an idea or whatever
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)

	;; Clean, something to publish
        ("c" "clean" plain "%?"
         :if-new
         (file+head "clean/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
	))

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

  ;; GTD Workflow

  ;; Not currently using org-roam for daily notes
  ;; "dt" '(org-roam-dailies-capture-today :which-key "Dailies Capture Today")
  ;; "dy" '(org-roam-dailies-capture-yesterday :which-key "Dailies Capture Yesterday")
  ;; "gt" '(org-roam-dailies-goto-today :which-key "Dailies goto today")
  ;; "gy" '(org-roam-dailies-goto-yesterday :which-key "Dailies goto yesterday")
  ;; "gd" '(org-roam-dailies-goto-date :which-key "Dailies goto date")
  ;; "gl" '(org-roam-dailies-goto-next-note :which-key "Dailies goto next note")
  ;; "gh" '(org-roam-dailies-goto-previous-note :which-key "Dailies goto previous note")


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
  "ps" '(helm-projectile-find-other-file :which-key "switch file")
  "pf" '(projetile-find-file :which-key "find file")
  "pg" '(helm-projectile-grep :which-key "grep in project")
  "pb" '(helm-projectile-switch-to-buffer :which-key "switch buffer")
  "pS" '(projectile-save-project-buffers :which-key "save project buffers")
  "pk" '(projectile-kill-buffers :which-key "kill project buffers")
  ;; org-ref help
  "rr" '(org-ref-cite-insert-helm :which-key "org-ref insert link")
  "af" '(org-fragtog-mode :which-key "Toggle fragtop mode")
  
  ;; help in emacs
			      
  ;; ...
)

;; Other keybidings
; Save the init.el file and reloads emacs
(global-set-key (kbd "C-c e i") (lambda () (interactive)
                             (let ((init-file "~/.emacs.d/init.el"))
                               (if (equal (buffer-file-name) (expand-file-name init-file))
                                   (save-buffer)
                                 (with-temp-buffer
                                   (find-file init-file)
                                   (save-buffer))))
                             (load-file "~/.emacs.d/init.el")))

; Evaluation of emacs code
;; C-x C-e eval just one line of code
(global-set-key (kbd "C-c m f") 'eval-defun)
(global-set-key (kbd "C-c m r") 'eval-region)
(global-set-key (kbd "C-c m b") 'eval-buffer)

; Function that saves all org buffers, we don't want to run this on code.
; may be run every two minutes
(run-with-timer 0 120 'save-all-org-buffers)
(put 'downcase-region 'disabled nil)

(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-;") 'helm-company)
     (define-key company-active-map (kbd "C-;") 'helm-company)))

(company-quickhelp-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/.org/tesis/borrador.org" "/home/tony/.org/agenda/Inbox.org" "/home/tony/.org/agenda/RL.org" "/home/tony/.org/agenda/archive.org" "/home/tony/.org/agenda/learning.org" "/home/tony/.org/agenda/someday.org" "/home/tony/.org/agenda/todo.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
