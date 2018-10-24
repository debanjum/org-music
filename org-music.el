(require 'shell)
(require 'org)

(defun flatten (l)
  (cond ((null l) nil)
   ((atom l) (list l))
   (t (loop for a in l appending (flatten a)))))

(defun get-org-headings ()
  "extract song headings from active/narrowed/buffer org"
  (if (use-region-p)
      (narrow-to-region (point) (mark)))
  (setq headings
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hs) (when (equal "song" (org-element-property :TYPE hs))
                         (org-element-property :title hs)))))
  (widen)
  (flatten headings))


(defun search-song-at-point ()
  (interactive)
  (cond ((org-entry-get nil "QUERY"))
        ((nth 4 (org-heading-components)))))

(defun play-song-at-point ()
  "Open song at point"
  (let* ((song-name (format "%s" (nth 4 (org-heading-components))))
         (query (search-song-at-point)))
    (message "Streaming: %s" song-name)
    (mpsyt-execute (format "\n/%s\nadd 1\nvp\nall\n" (flatten query)))))

(defun enqueue-song-at-point ()
  "Open song at point"
  (let ((song-name (format "%s" (nth 4 (org-heading-components)))))
    (message "Streaming: %s" song-name)
    (mpsyt-execute (format "\n/%s\nadd 1\nvp\n<SPC>\n" (flatten song-name)))))

(defun start-mpsyt ()
  (let ((proc (start-process "*mpsyt" "*mpsyt*" "mpsyt")))
    (with-current-buffer (process-buffer proc)
      (display-buffer (current-buffer))
      (shell-mode)
      (mpsyt-mode)
      (set-process-sentinel proc #'(lambda (process event) (if (equal "killed" event)
                                                               (kill-buffer "*mpsyt*"))))
      (set-process-filter proc 'comint-output-filter))))

(defun mpsyt-execute (command)
  "Send command to mpsyt process"
  (interactive)
  (let* ((proc-name "*mpsyt")
         (proc (cond ((get-process proc-name))
                     ((start-mpsyt)))))
    (process-send-string proc-name command)))

(defun mpsyt-playing? ()
  "returns t if playing anything currently else nil"
  (if (get-process "*mpsyt")
      (with-temp-buffer
        (progn
          (insert-buffer-substring "*mpsyt*")
          (end-of-buffer)
          (previous-line)
          (let* ((sline (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (result (equal 0 (length sline))))
            (not result))))))

(defun enqueue-list ()
  "enqueue songs in active-region/sparse-tree/buffer"
  (interactive)
  (if (mpsyt-playing?)
      (mpsyt-execute "q"))
  (let ((command
         (format "%s\nvp\nall\n"
                 (seq-reduce
                  #'(lambda (a b) (concatenate 'string a (format "/%s\nadd 1\n" b)))
                  (get-org-headings) ""))))
    (message "%s" command)
    (mpsyt-execute command)))

(defun play-list-mpsyt ()
  "play songs in active-region/sparse-tree/buffer"
  (interactive)
  (if (get-process "*mpsyt")
      (delete-process "*mpsyt*"))
  (mpsyt-execute
   (format "\nvp\nrm 1-\n%svp\nshuffle all\n"
           (seq-reduce
            #'(lambda (a b) (concatenate 'string a (format "/%s\nadd 1\n" b)))
            (get-org-headings) ""))))

(defun play-agenda (search-string)
  "play org-agenda filtered playlist"
  ;; filter songs in music library using search terms
  (execute-kbd-macro (kbd (format "C-c a p %s SPC +{:TYPE:\\s-+song}" (replace-regexp-in-string " " " SPC " search-string))))
  ;; write filtered playlist to org file and open
  (org-agenda-write "/tmp/playlist.org" t)
  ;; get song-name from org playlist's headings, format it to enqueue and play in mpsyt, trigger mpsyt
  (play-list-on-android)
  (kill-buffer "playlist.org"))

(defun play-mpsyt-buffer-song-at-point ()
  "play song at point in mpsyt process buffer"
  (interactive)
  (let ((proc-name "*mpsyt")
        (sline (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (mpsyt-execute "\n")
    (search-backward sline)
    (setq pline (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
    (mpsyt-execute (format "%s-\n" (car (split-string pline))))))

(defun open-mpsyt-buffer ()
  (interactive)
  (cond ((get-process "*mpsyt"))
        ((start-mpsyt)))
  (switch-to-buffer-other-window "*mpsyt*"))

(defun clear-playlist ()
  "clear songs in mpsyt playlist"
  (interactive)
  (if (mpsyt-playing?)
      (delete-process "*mpsyt*"))
  (let ((command (format "vp\nrm 1-\n")))
    (mpsyt-execute command)))

(defun search-youtube (search-term)
  "search songs in youtube"
  (interactive "sEnter search term: ")
  (message "\n/%s\n" search-term)
  (mpsyt-execute (format "\n/%s\n" search-term))
  (switch-to-buffer "*mpsyt*"))

(defun play-list-on-android ()
  (android-play-org-entries (get-org-headings)))

(defun android-play-org-entries (song-entries)
  "map each song in playlist to its youtube-id and share via termux to android youtube player"
  (mapcar #'(lambda (song)
              (android-share-youtube-song (get-youtube-id-of-song song)))
          song-entries))

(defun get-youtube-id-of-song (song-entry)
  "retrieve youtube-id of top result on youtube for org song heading via youtube-dl"
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "youtube-dl --get-id ytsearch:\"%s\"" (car song-entry)))))

(defun android-share-youtube-song (song-id)
  "share constructed youtube-url via termux to play on android youtube player"
  (shell-command-to-string (format "termux-open-url \"https://youtube.com/watch?v=%s\"" song-id)))

(define-minor-mode mpsyt-mode
  "Interact with mpsyt through emacs"
  :lighter " Mpsyt"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c m s") 'search-youtube)
	    (define-key map (kbd "<SPC>") '(lambda () "play/pause" (interactive) (mpsyt-execute (kbd "SPC"))))
	    (define-key map (kbd "q") 'delete-window)
	    (define-key map (kbd "<") '(lambda () "play next track in playlist" (interactive) (mpsyt-execute (kbd "<"))))
	    (define-key map (kbd "o") 'play-mpsyt-buffer-song-at-point)
	    (define-key map (kbd ">") '(lambda () "play previous track in playlist" (interactive) (mpsyt-execute (kbd ">"))))
	    (define-key map (kbd "0") '(lambda () "increase volume" (interactive) (mpsyt-execute (kbd "0"))))
	    (define-key map (kbd "9") '(lambda () "decrease volume" (interactive) (mpsyt-execute (kbd "9"))))
	    map))

(define-minor-mode org-music-mode
  "Play music from org"
  :lighter " Org-music"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c m m") 'open-mpsyt-buffer)
	    (define-key map (kbd "C-c m o") 'play-list)
	    (define-key map (kbd "C-c m e") 'enqueue-list)
	    (define-key map (kbd "C-c m s") 'search-youtube)
	    (define-key map (kbd "C-c m <SPC>") '(lambda () "play/pause" (interactive) (mpsyt-execute (kbd "SPC"))))
	    (define-key map (kbd "C-c m p") '(lambda () "play previous track in playlist" (interactive) (mpsyt-execute (kbd "<"))))
	    (define-key map (kbd "C-c m n") '(lambda () "play next track in playlist" (interactive) (mpsyt-execute (kbd ">"))))
	    (define-key map (kbd "C-c m 0") '(lambda () "increase volume" (interactive) (mpsyt-execute (kbd "0"))))
	    (define-key map (kbd "C-c m 9") '(lambda () "decrease volume" (interactive) (mpsyt-execute (kbd "9"))))
	    map)

  ;; define music speed commands
  (setq org-speed-commands-music
	'(("o" . (lambda () "play song at point" (play-song-at-point)))
	  ("e" . (lambda () "enqueue song at point" (enqueue-song-at-point)))
	  ("m" . (lambda () "open mpsyt buffer" (open-mpsyt-buffer)))
	  ("s" . (lambda () "play/pause" (message "toggle play/pause") (mpsyt-execute (kbd "SPC"))))
	  ("j" . (lambda () "play next track in playlist" (mpsyt-execute (kbd "<"))))
	  ("k" . (lambda () "play previous track in playlist" (mpsyt-execute (kbd ">"))))
	  ("h" . (lambda () "increase volume" (mpsyt-execute (kbd "0"))))
	  ("l" . (lambda () "decrease volume" (mpsyt-execute (kbd "9"))))))

  (defun org-speed-music (keys)
    "Use speed commands if at cursor at beginning of an org-heading line"
    (when (and (bolp) (looking-at org-outline-regexp))
      (cdr (assoc keys org-speed-commands-music))))

  ;; add to org-speed-command-hook
  (add-hook 'org-speed-command-hook 'org-speed-music))

(provide 'org-music)
