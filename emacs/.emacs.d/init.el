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

;; Installed packages: They are in their corresponding categories
; gui
(straight-use-package 'dashboard)
(straight-use-package 'which-key)
(straight-use-package 'all-the-icons)
(straight-use-package 'page-break-lines)
(straight-use-package 'telephone-line)
(straight-use-package 'doom-themes)
(straight-use-package 'dimmer)

; evil mode AKA vim mode
(straight-use-package 'evil)
(straight-use-package 'evil-snipe)
(straight-use-package 'evil-numbers)
;(straight-use-package 'evil-magit)
;(straight-use-package 'evil-leader)
;(straight-use-package 'evil-surround)
;(straight-use-package 'evil-nerd-commenter)
;(straight-use-package 'evil-org)


;; Now that all the packages are defined and installed, go thought each one of the catergories and set up everything
