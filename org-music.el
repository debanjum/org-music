(require 'shell)

(defun get-org-headings-from-region ()
  "extract song headings from active/narrowed/buffer org"
  (narrow-to-region (point) (mark))
  (setq headings (org-element-map (org-element-parse-buffer) 'headline
     	      (lambda (hs) (when (equal "song" (org-element-property :TYPE hs))
     			     (org-element-property :title hs)))))
  (widen)
  headings)

(defun flatten (l)
  (cond ((null l) nil)
   ((atom l) (list l))
   (t (loop for a in l appending (flatten a)))))

(defun enqueue-list ()
  "enqueue songs in active-region/sparse-tree/buffer"
  (interactive)
  (mpsyt-execute
   (format "\n/%s\nvp\nall\n"
      (reduce #'(lambda (a b) (concatenate 'string a (format "\nadd 1\n/%s" b)))
     	      (flatten (get-org-headings-from-region))))))

(defun play-list ()
  "play songs in active-region/sparse-tree/buffer"
  (interactive)
  (mpsyt-execute
   (format "\nvp\nrm all\n/%s\nvp\nall\n"
      (reduce #'(lambda (a b) (concatenate 'string a (format "\nadd 1\n/%s" b)))
     	      (flatten (get-org-headings-from-region))))))

(defun search-youtube (search-term)
  "search songs in youtube"
  (interactive "sEnter search term: ")
  (message "\n/%s\n" search-term)
  (mpsyt-execute (format "\n/%s\n" search-term))
  (switch-to-buffer "*mpsyt*"))

(defun start-mpsyt (proc-name)
  (let ((proc (start-process proc-name "*mpsyt*" "mpsyt")))
    (with-current-buffer (process-buffer proc)
      (display-buffer (current-buffer))
      (shell-mode)
      (set-process-filter proc 'comint-output-filter))))

(defun mpsyt-execute (command)
  "Send command to mpsyt"
  (interactive)
  (let* ((proc-name "*mpsyt")
         (proc (cond ((get-process proc-name))
		     ((start-mpsyt)))))
    (process-send-string proc-name command)))

(define-minor-mode mpsyt-mode
  "Interact with mpsyt through emacs"
  :lighter " Mpsyt"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c m s") 'search-youtube)
	    (define-key map (kbd "<SPC>") '(lambda () "play/pause" (interactive) (mpsyt-execute (kbd "SPC"))))
	    (define-key map (kbd "q") 'delete-window)
	    (define-key map (kbd "<") '(lambda () "play next track in playlist" (interactive) (mpsyt-execute (kbd "<"))))
	    (define-key map (kbd ">") '(lambda () "play previous track in playlist" (interactive) (mpsyt-execute (kbd ">"))))
	    (define-key map (kbd "0") '(lambda () "increase volume" (interactive) (mpsyt-execute (kbd "0"))))
	    (define-key map (kbd "9") '(lambda () "decrease volume" (interactive) (mpsyt-execute (kbd "9"))))
            map))

(define-minor-mode org-music-mode
  "Play music from org"
  :lighter " Org-music"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c m m") 'open-mpsyt-buffer)
	    (define-key map (kbd "C-c m a") 'play-list)
	    (define-key map (kbd "C-c m e") 'enqueue-list)
	    (define-key map (kbd "C-c m s") 'search-youtube)
	    (define-key map (kbd "C-c m <SPC>") '(lambda () "play/pause" (interactive) (mpsyt-execute (kbd "SPC"))))
	    (define-key map (kbd "C-c m p") '(lambda () "play next track in playlist" (interactive) (mpsyt-execute (kbd "<"))))
	    (define-key map (kbd "C-c m n") '(lambda () "play previous track in playlist" (interactive) (mpsyt-execute (kbd ">"))))
	    (define-key map (kbd "C-c m 0") '(lambda () "increase volume" (interactive) (mpsyt-execute (kbd "0"))))
	    (define-key map (kbd "C-c m 9") '(lambda () "decrease volume" (interactive) (mpsyt-execute (kbd "9"))))
            map)

  ;; define music speed commands
  (setq org-speed-commands-music
	'(("o" . (lambda () "Open media at point"
		   (let ((song-name (format "%s" (nth 4 (org-heading-components)))))
		     (message "Streaming: %s" song-name)
		     (mpsyt-execute (format "\n/%s\nadd 1\nvp\nall\n" song-name)))))
	  ("s" . (lambda () "play/pause" (process-send-string "*mpsyt" (kbd "SPC"))))
	  ("g" . (lambda () "open status buffer" (switch-to-buffer "*mpsyt*")))
	  ("j" . (lambda () "play next track in playlist" (process-send-string "*mpsyt" (kbd "<"))))
	  ("k" . (lambda () "play previous track in playlist" (process-send-string "*mpsyt" (kbd ">"))))
	  ("h" . (lambda () "increase volume" (process-send-string "*mpsyt" (kbd "0"))))
	  ("a" . (lambda () "enqueue selected" (play-list)))
	  ("e" . (lambda () "play selected " (enqueue-list)))
	  ("l" . (lambda () "decrease volume" (process-send-string "*mpsyt" (kbd "9"))))))

  (defun org-speed-music (keys)
    "Use speed commands if at cursor at beginning of an org-heading line"
    (when (and (bolp) (looking-at org-outline-regexp))
      (cdr (assoc keys org-speed-commands-music))))
  
  ;; add to org-speed-command-hook
  (add-hook 'org-speed-command-hook 'org-speed-music))

(provide 'org-music-mode)
