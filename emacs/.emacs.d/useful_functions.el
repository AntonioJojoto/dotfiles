; Function that saves all org buffers, we don't want to run this on code.
; may be run every two minutes
(defun save-all-org-buffers ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) ; Check if buffer is associated with a file
                 (string= (file-name-extension (buffer-file-name)) "org")
                 (buffer-modified-p))
        (save-buffer)))))

;; Dictionary and spell check functions
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

(defun fd-switch-dictionary()
      (interactive)
      (let* ((dic ispell-current-dictionary)
    	 (change (if (string= dic "es") "english" "es")))
        (ispell-change-dictionary change)
        (message "Dictionary switched from %s to %s" dic change)
        ))

;; Custom search functions
(defun rg-roam-search (search-term)
  "Perform a helm-rg search within `org-roam-directory' with predefined settings."
  (interactive "sSearch for: ") ;; Prompts the user for the search term
  (let ((helm-rg-default-directory org-roam-directory) ;; Set search directory to org-roam-directory
        (helm-rg-default-extra-args '("--ignore-case" ;; Case insensitive search
                                      "--type" "org"  ;; Search only org files
                                      "--max-columns=500" ;; Max column width
                                      )))
    ;; Execute helm-rg search
    (helm-rg search-term nil)))
