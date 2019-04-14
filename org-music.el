;;; org-music.el --- Store and play music from a simple org-mode file

;; Copyright (C) 2019 Free Software Foundation, Inc.


;; Author: Debanjum S. Solanky <debanjum@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emms "1.0") (request "1.0"))
;; Keywords: hypermedia, multimedia, outlines, music, org-mode
;; URL: http://gitlab.com/debanjum/org-music


;;; Commentary:

;; This package provides a minor mode to turn an org file into an
;; interactive music library. Get syncing, versioning, tagging,
;; sharing, semantic, full text search and more straight out of the
;; box. It's plain-text after all!

(require 'shell)
(require 'org)

;; Org Music Library Media Control
;; -------------------------------
(defvar org-music-files '("~/Notes/Music.org"))

(defvar org-music-media-directory "~/Music/OrgMusic/"
  "Location where media is cached")

(defvar org-music-android-media-directory "file:///storage/emulated/0/Music/OrgMusic/"
  "Location where media is cached on android")

(defvar org-music-cache-size 30
  "Media cache size")

(defvar org-music-cache-song-format "m4a"
  "Format to store songs in cache")

(defvar org-music-last-playlist-filter nil
  "Last org filter used to create playlist")

(defvar org-music-next-cloud-script "~/Scripts/bin/nextcloud.py"
  "Nextcloud script location. Gets song's nextcloud stream url")

(defun flatten (l)
  (cond ((null l) nil)
   ((atom l) (list l))
   (t (loop for a in l appending (flatten a)))))

(defun get-org-headings ()
  "extract song headings from active/narrowed/sparse-tree region of org buffer"
  (interactive)
  (if (use-region-p)
      (narrow-to-region (point) (mark)))
  (setq headings
        (org-element-map (org-element-parse-buffer "object" t) 'headline
          (lambda (hs)
            (when (equal "song" (org-element-property :TYPE hs))
              (if (not (null (org-element-property :QUERY hs)))
                  (list (org-element-property :QUERY hs) (org-element-property :CATEGORY hs))
                (list (org-element-property :raw-value hs) (org-element-property :CATEGORY hs)))))))
  (message "%s" headings)
  (widen)
  headings)

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
        (org-element-map (org-element-parse-buffer "object" t) 'headline
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

(defun emms-enqueue (search-query &optional source)
  (emms-add-url (get-media-url search-query source)))

(defun emms-play (search-query &optional source)
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

(defun enqueue-song-at-point ()
  "enqueue song at point"
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (search-song-at-point))
        (source (get-song-source)))
    ;(emms-enqueue (flatten query) source)
    (play-cached-song (car (flatten query)) source t)
    (message "Streaming: %s" song-name)
    (log-song-state "ENQUEUED")))

(defun play-song-at-point ()
  "open song at point"
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (search-song-at-point))
        (source (get-song-source)))
    ;(emms-play (flatten query) source)
    (play-cached-song (car (flatten query)) source nil)
    (message "Streaming: %s" song-name)
    (log-song-state "ENQUEUED")))

(defun enqueue-list (&optional songs-list)
  "enqueue songs in active-region/sparse-tree/buffer"
  (interactive)
  (let ((songs (or songs-list (get-org-headings))))
    (mapcar #'(lambda (s)
                (apply #'play-cached-song (append s (list t))))
            songs)))

(defun play-list ()
  "play songs in active-region/sparse-tree/buffer"
  (interactive)
  (let ((songs (get-org-headings)))
    (apply #'play-cached-song (pop songs))
    (enqueue-list songs)))

;; Control Music Player on Android via Termux
;; ------------------------------------------
(defun play-agenda (search-string)
  "play org-agenda filtered playlist"
  ;; filter songs in music library using search terms
  (execute-kbd-macro (kbd (format "C-c a p %s SPC +{:TYPE:\\s-+song}" (replace-regexp-in-string " " " SPC " search-string))))
  ;; write filtered playlist to org file and open
  (org-agenda-write "~/.playlist.org" t)
  ;; get song-name from org playlist's headings, format it to enqueue and play in mpsyt, trigger mpsyt
  (play-list-on-android)
  (kill-buffer ".playlist.org"))

(defun play-list-on-android ()
  "create playlist from org songs and share via termux to android music player"
  (create-m3u-playlist (get-org-headings))
  (android-share-playlist))

(defun create-m3u-playlist (song-entries)
  "create m3u playlist from org songs"
  (write-playlist-to-file
   (mapconcat
    #'(lambda (song)
        (format "#EXTINF:,%s\n%s"
                (car song)
                (apply #'cache-song (append song (list "android")))))
    song-entries
    "\n")))

(defun write-playlist-to-file (m3u-playlist)
  (let ((playlist-file (format "%s%s" org-music-media-directory "orgmusic.m3u")))
    (write-region (format "#EXTM3U\n%s" m3u-playlist) nil playlist-file)))

(defun android-share-playlist ()
  "share playlist via termux to an android music player"
  (let ((playlist-file (format "%s%s" org-music-android-media-directory "orgmusic.m3u")))
  (shell-command-to-string (format "termux-open %S" playlist-file))))

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
    (format "%s \"get_url\" \"%s\"" org-music-next-cloud-script (car song-entry)))))

(defun get-song (name file-location)
  "download org song from youtube via youtube-dl"
  (interactive)
  (let ((download-command
         (format "youtube-dl -f %s --quiet ytsearch:%S -o %S" org-music-cache-song-format name file-location)))
    (message "%s" download-command)
    (shell-command-to-string download-command)))

(defun play-cached-song (song-entry &optional source enqueue)
  "cache and play song"
  (interactive)
  (let ((uri-location (cache-song song-entry source)))
    (message "%s" uri-location source)
    (if (equal source "nextcloud")
        (if enqueue
            (emms-add-url uri-location)
          (emms-play-url uri-location))
      (if enqueue
          (emms-add-file uri-location)
      (emms-play-file uri-location)))))

(defun cache-song (song-name &optional source os)
  "If song not available in local, download"
  (interactive)
  (let ((song-file-location
         (format "%s%s.%s" org-music-media-directory song-name org-music-cache-song-format)))
    (message "cache location: %s, source: %s" song-file-location source)
    (if (equal source "nextcloud")
        (get-nextcloud-url (if (listp song-name) song-name (list song-name)))
       ;; if file doesn't exist, trim cache and download file
      (if (not (file-exists-p song-file-location))
          (progn
            (trim-cache)
            (get-song song-name song-file-location)))
      ;; return song uri based on operating system
      (if (equal os "android")
          (format "%s%s.%s" org-music-android-media-directory song-name org-music-cache-song-format)
        song-file-location))))

(defun trim-cache ()
  "trim media cache if larger than cache-size"
  (interactive)
  (let ((sorted-files
         (reverse
         (sort
          (directory-files (expand-file-name org-music-media-directory) t directory-files-no-dot-files-regexp)
          'file-newer-than-file-p))))
    (setq excess-count (- (list-length sorted-files) org-music-cache-size))
    (if (> excess-count 0)
        (progn
              (message "trim music cache of: %s" (last-sorted-files excess-count))
              (mapcar 'delete-file (last sorted-files excess-count))))))

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

;;;###autoload
;; Configure Org-Music Mode
(define-minor-mode org-music-mode
  "Play music from org"
  :lighter " Org-music"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m o") 'play-list)
            (define-key map (kbd "C-c m e") 'enqueue-list)
            (define-key map (kbd "C-c m <SPC>") '(lambda () "play/pause" (interactive) (emms-pause)))
            (define-key map (kbd "C-c m p") '(lambda () "play previous track in playlist" (interactive) (emms-previous)))
            (define-key map (kbd "C-c m n") '(lambda () "play next track in playlist" (interactive) (emms-next)))
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
