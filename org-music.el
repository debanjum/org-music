(require 'shell)
(require 'org)
(require 'playerctl)

;; Org Music Library Media Control
;; -------------------------------
(defvar org-music-files '("~/Notes/Music.org"))

(defvar org-music-media-directory "~/Music/OrgMusic/"
  "Location where media is cached")

(defvar org-music-cache-size 30
  "Media cache size")

(defvar org-music-cache-song-format "m4a"
  "Format to store songs in cache")

(defvar org-music-last-playlist-filter nil
  "Last org filter used to create playlist")

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

(defun log-song-state (state)
  "add current state start time to logbook of song"
  (interactive)
  (let ((opos (point))
        (log-spos (org-log-beginning t))
        (log-time (format-time-string
                   (org-time-stamp-format 'long 'inactive)(org-current-effective-time))))
    (goto-char log-spos)
    (insert state ": " log-time "\n")
    (previous-line)
    (org-indent-region (line-beginning-position) (line-end-position))
    (goto-char opos)))

(defun get-search-query ()
  "use value of QUERY property if it exists to search provider, else use song headings"
  (if (use-region-p)
      (narrow-to-region (point) (mark)))
  (setq queries
        (org-element-map (org-element-parse-buffer) 'headline
          (lambda (hs) (when (equal "song" (org-element-property :TYPE hs))
                         (org-element-property :QUERY hs)))))
  (widen)
  (flatten queries))

(defun search-song-at-point ()
  "use value of QUERY property if it exists to search provider, else use song headings"
  (interactive)
  (cond ((org-entry-get nil "QUERY"))
        ((nth 4 (org-heading-components)))))

(defun get-song-source ()
  "use value of CATEGORY property if it exists, else default to youtube. This is the source of the song"
  (interactive)
  (cond ((org-entry-get nil "CATEGORY"))
        ("youtube")))

;; Music Player Controls
;; ---------------------

;; Control MPV on Linux via Shell
;; ------------------------------
(defun mpv-start ()
  (shell-command
   (format "mpv --idle --input-ipc-server=/tmp/mpvsocket --ytdl-format=bestaudio &")))

(defun mpv-running? ()
  (member "mpv" (split-string (shell-command-to-string "playerctl -l"))))

(defun get-media-url (search-query source)
  (interactive)
  (if (equal "nextcloud" source)
      (get-nextcloud-url search-query)
    (get-youtube-url search-query)))

(defun emms-enqueue (search-query source)
  (emms-add-url (get-media-url search-query source)))

(defun emms-play (search-query source)
  (emms-play-url (get-media-url search-query source)))

(defun mpv-enqueue (search-query)
  "enqueue in mpv first youtube result based on search-query"
  (when (not (mpv-running?))
      (mpv-start))
  (shell-command-to-string
   (format
    "echo '{ \"command\": [\"loadfile\", \"ytdl://ytsearch:\\\"%s\\\"\", \"append-play\"] }' | socat - /tmp/mpvsocket"
    search-query)))

(defun jump-to-random-song (&optional match)
  "jump to a random song satisfying 'match' in the music library"
  (interactive)
  (let ((org-randomnote-candidates org-music-files)
        (song-match (concat (or match org-music-last-playlist-filter "") "+TYPE=\"song\"")))
    (setq org-music-last-playlist-filter match)
    (org-randomnote song-match)))

(defun play-random-song (&optional match)
  "play random song satisfying 'match' in the music library"
  (interactive)
  (jump-to-random-song (or match org-music-last-playlist-filter nil))
  (play-song-at-point)
  (bury-buffer))

(defun play-random-songs (&optional match)
  "Play random songs satisfying 'match' in the music library"
  (interactive)
  (let ((playlist-filter (or match org-music-last-playlist-filter)))
    (play-random-song playlist-filter)
    (add-hook 'emms-player-finished-hook 'play-random-songs)
    (add-hook 'emms-player-stopped-hook #'(lambda () (remove-hook 'emms-player-finished-hook 'play-random-songs)))))

(defun play-highlighted (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring-no-properties start end)))
        (emms-play-url (get-media-url regionp "youtube")))))

(defun enqueue-highlighted (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring-no-properties start end)))
        (emms-add-url (get-media-url regionp "youtube")))))

(defun mpv-play (search-query)
  "enqueue in mpv first youtube result based on search-query"
  (when (not (mpv-running?))
      (mpv-start))
  (shell-command-to-string
   (format
    "echo '{ \"command\": [\"loadfile\", \"ytdl://ytsearch:\\\"%s\\\"\", \"append\"] }' | socat - /tmp/mpvsocket"
    search-query)))

(defun cache-play-song-at-point ()
  "download song at point, play downloaded"
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (search-song-at-point))
        (source (get-song-source)))
    (emms-play (flatten query) source)
    (message "Streaming: %s" song-name)
    (log-song-state "ENQUEUED")))

(defun enqueue-song-at-point ()
  "enqueue song at point"
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (search-song-at-point))
        (source (get-song-source)))
    (emms-enqueue (flatten query) source)
    (message "Streaming: %s" song-name)
    (log-song-state "ENQUEUED")))

(defun play-song-at-point ()
  "open song at point"
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (search-song-at-point))
        (source (get-song-source)))
    (emms-play (flatten query) source)
    (message "Streaming: %s" song-name)
    (log-song-state "ENQUEUED")))

(defun enqueue-list ()
  "enqueue songs in active-region/sparse-tree/buffer"
  (interactive)
  (mapcar #'emms-enqueue (get-org-headings)))

(defun play-list ()
  "play songs in active-region/sparse-tree/buffer"
  (interactive)
  (mapcar #'emms-play (get-org-headings)))


;; Control Music Player on Android via Termux
;; ------------------------------------------
(defun play-agenda (search-string)
  "play org-agenda filtered playlist"
  ;; filter songs in music library using search terms
  (execute-kbd-macro (kbd (format "C-c a p %s SPC +{:TYPE:\\s-+song}" (replace-regexp-in-string " " " SPC " search-string))))
  ;; write filtered playlist to org file and open
  (org-agenda-write "/tmp/playlist.org" t)
  ;; get song-name from org playlist's headings, format it to enqueue and play in mpsyt, trigger mpsyt
  (play-list-on-android)
  (kill-buffer "playlist.org"))

(defun play-list-on-android ()
  (android-play-org-entries (get-org-headings)))

(defun android-play-org-entries (song-entries)
  "map each song in playlist to its youtube-id and share via termux to android youtube player"
  (mapcar #'(lambda (song)
              (android-share-youtube-song (get-youtube-id-of-song song)))
          song-entries))

(defun get-youtube-url (search-query)
  (format "https://youtube.com/watch?v=%s" (get-youtube-id-of-song (list search-query))))

(defun get-youtube-id-of-song (song-entry)
  "retrieve youtube-id of top result on youtube for org song heading via youtube-dl"
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "youtube-dl --get-id ytsearch:\"%s\"" (car song-entry)))))

(defun android-share-youtube-song (song-id)
  "share constructed youtube-url via termux to play on android youtube player"
  (shell-command-to-string (format "termux-open-url \"https://youtube.com/watch?v=%s\"" song-id)))

(defun get-nextcloud-url (song-entry)
  "retrieve song url of top result on nextcloud for org song heading"
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "~/Scripts/bin/nextcloud \"get_url\" \"%s\"" (car song-entry)))))

(defun get-song (song-name)
  "download org song from youtube via youtube-dl"
  (interactive)
  (let ((download-command
         (format "youtube-dl -f %3$s --quiet ytsearch:\"%1$s\" -o \"%2$s%1$s.%3$s\"" song-name org-music-media-directory org-music-cache-song-format)))
    (message "%s" download-command)
    (shell-command-to-string download-command)))

(defun play-local-song (song-location enqueue)
  (interactive)
  (if enqueue
      (emms-add-file song-location)
    (emms-play-file song-location)))

(defun play-cache-song (song-entry enqueue)
  "If song not available in local, download, then play"
  (interactive)
  (let ((song-name
         (if (listp song-entry) (car song-entry) (song-entry)))
        (song-local-location
         (format "%s%s.%s" org-music-media-directory song-name org-music-cache-song-format)))
    (if (not (file-exists-p song-local-location))
        (trim-cache)
        (get-song song))
    (play-local-song song-local-location enqueue)))

(defun trim-cache ()
  "trim media cache if larger than cache-size"
  (interactive)
  (shell-command-to-string (format "ls -tp %s | grep -v '/$' | tail -n +%s | xargs -I \{\} rm -- \{\}" org-music-media-directory org-music-cache-size)))

;; Org Music Library Metadata Enhancement Methods
;; ----------------------------------------------
(defun append-outline-to-song-entries ()
  "add outline parents of all visible song headings to their org entries"
  (interactive)
  (while (not (org-entry-get nil "SEQ"))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil nil " " t)))
      (if (equal "song" (org-entry-get nil "TYPE"))
          (insert-outline-of-entry outline)))))

(defun append-outline-to-song-query-property ()
  "add outline parents of all visible song headings to their query property"
  (interactive)
  (while (not (org-entry-get nil "SEQ"))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil t " - " t)))
      (if (equal "song" (org-entry-get nil "TYPE"))
          (org-entry-put nil "QUERY" outline)))))

(defun remove-colon-from-org-headings ()
  "remove colon from non song org headings"
  (interactive)
  (while (or (not (org-entry-get nil "SEQ"))
             (outline-invisible=p))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil t " - " t)))
      (if (not (equal "song" (org-entry-get nil "TYPE")))
          (progn
            (end-of-line)
            (if (equal ":" (string (char-before)))
                (delete-backward-char 1)))))))

(defun insert-outline-of-entry (outline)
  "add outline parents of song heading at point to its org entries"
  (progn
    (org-next-visible-heading 1)
    (insert "\n")
    (previous-line)
    (insert outline)
    (beginning-of-line)
    (org-cycle)
    (org-previous-visible-heading 1)))

;; Configure Org-Music Mode
(define-minor-mode org-music-mode
  "Play music from org"
  :lighter " Org-music"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m o") 'play-list)
            (define-key map (kbd "C-c m e") 'enqueue-list)
            (define-key map (kbd "C-c m <SPC>") '(lambda () "play/pause" (interactive) (playerctl-play-pause-song)))
            (define-key map (kbd "C-c m p") '(lambda () "play previous track in playlist" (interactive) (playerctl-previous-song)))
            (define-key map (kbd "C-c m n") '(lambda () "play next track in playlist" (interactive) (playerctl-next-song)))
            (define-key map (kbd "C-c m r") '(lambda () "play previous track in playlist" (interactive) (play-random-song)))
            (define-key map (kbd "C-c m R") '(lambda () "play next track in playlist" (interactive) (play-random-songs)))
            map)

  ;; define music speed commands
  (setq org-speed-commands-music
        '(("o" . (lambda () "play song at point" (play-song-at-point)))
          ("e" . (lambda () "enqueue song at point" (enqueue-song-at-point)))
          ("s" . (lambda () "play/pause" (message "toggle play/pause") (emms-pause)))
          ("d" . (lambda () "play next track in playlist" (emms-previous)))
          ("f" . (lambda () "play previous track in playlist" (emms-next)))))

  (defun org-speed-music (keys)
    "Use speed commands if at cursor at beginning of an org-heading line"
    (when (and (bolp) (looking-at org-outline-regexp))
      (cdr (assoc keys org-speed-commands-music))))

  ;; add to org-speed-command-hook
  (add-hook 'org-speed-command-hook 'org-speed-music))

(provide 'org-music)
