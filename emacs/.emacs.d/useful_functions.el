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

