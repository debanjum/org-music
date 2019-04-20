;;; org-music.el --- Store and play music from a simple org-mode file

;; Copyright (C) 2017-2019 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 1.0
;; Package-Requires: ((org "9.0") (emms "5.0") (org-randomnote "0.1"))
;; Keywords: hypermedia, multimedia, outlines, music, org-mode
;; URL: http://gitlab.com/debanjum/org-music

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode to turn an org file into an
;; interactive music library. Get syncing, versioning, tagging,
;; sharing, semantic, full text search and more straight out of the
;; box. It's plain-text after all!

;;; Code:


(require 'shell)
(require 'org)
(require 'emms)
(require 'org-randomnote)
(require 'url)
(require 'json)

(declare-function emms-add-url "ext:emms")
(declare-function emms-play-url "ext:emms")
(declare-function emms-add-file "ext:emms")
(declare-function emms-play-file "ext:emms")
(declare-function org-randomnote "ext:org-randomnote")
(declare-function org-agenda-write "ext:org-agenda")

;; Org Music Library Media Control
;; -------------------------------
(defcustom org-music-file "~/Notes/Music.org"
  "Music org file location."
  :group 'org-music
  :type '(file :must-match t))

(defcustom org-music-media-directory "~/Music/OrgMusic/"
  "Media cache location.
Read, write path on Linux. Write path on Android relative to Termux root"
  :group 'org-music
  :type 'directory)

(defcustom org-music-android-media-directory "file:///storage/emulated/0/Music/OrgMusic/"
  "Media cache location on android. Used to retrieve songs, playlists on android."
  :group 'org-music
  :type 'string)

(defcustom org-music-next-cloud-script "~/Scripts/bin/nextcloud.py"
  "Location of Nextcloud script. Used to get nextcloud url of media."
  :group 'org-music
  :type '(file :must-match t))

(defcustom org-music-cache-size 30
  "Media cache size."
  :group 'org-music
  :type 'integer)

(defcustom org-music-cache-song-format "m4a"
  "Format to store songs in cache. See youtube-dl for available formats."
  :group 'org-music
  :type 'string)

(defcustom org-music-samvayati-root-url "http://localhost:5000"
  "Location of samvayati, the contextual awareness server."
  :group 'org-music
  :type 'string)

(defvar org-music-last-playlist-filter nil
  "Last org filter used to create playlist.")

(defun org-music--flatten (l)
  "Org-Music--Flatten recursive list L."
  (cond ((null l) nil)
   ((atom l) (list l))
   (t (cl-loop for a in l appending (org-music--flatten a)))))

(defun org-music--get-org-headings ()
  "Extract song headings from active/narrowed/sparse-tree region of org buffer."
  (interactive)
  (if (use-region-p)
      (narrow-to-region (point) (mark)))
  (let ((headings
        (org-element-map (org-element-parse-buffer "object" t) 'headline
          (lambda (hs)
            (when (equal "song" (org-element-property :TYPE hs))
              (if (not (null (org-element-property :QUERY hs)))
                  (list (org-element-property :QUERY hs) (org-element-property :CATEGORY hs))
                (list (org-element-property :raw-value hs) (org-element-property :CATEGORY hs))))))))
        (message "%s" headings)
        (widen)
        headings))

(defun org-music--log-song-state (state)
  "Add STATE: `org-current-effective-time` to song's LOGBOOK."
  (interactive)
  (let ((opos (point))
        (log-spos (org-log-beginning t))
        (log-time (format-time-string
                   (org-time-stamp-format 'long 'inactive)(org-current-effective-time))))
    (goto-char log-spos)
    (insert state ": " log-time "\n")
    (forward-line)
    (org-indent-region (line-beginning-position) (line-end-position))
    (goto-char opos)))

(defun org-music--get-search-query ()
  "Retrieve QUERY property values or headings of org entries in active/narrowed/sparse-tree region of org buffer."
  (if (use-region-p)
      (narrow-to-region (point) (mark)))
  (let ((queries
        (org-element-map (org-element-parse-buffer "object" t) 'headline
          (lambda (hs) (when (equal "song" (org-element-property :TYPE hs))
                         (org-element-property :QUERY hs))))))
    (widen)
    (org-music--flatten queries)))

(defun org-music--search-song-at-point ()
  "Retrieve QUERY property value or heading of song at point."
  (interactive)
  (cond ((org-entry-get nil "QUERY"))
        ((nth 4 (org-heading-components)))))

(defun org-music--get-song-source ()
  "Retrieve data source from CATEGORY property value of song's org-entry if it exists. Defaults to Youtube."
  (interactive)
  (cond ((org-entry-get nil "CATEGORY"))
        ("youtube")))

;; Music Player Controls
;; ---------------------

;; Control MPV on Linux via Shell
;; ------------------------------
(defun org-music--mpv-start ()
  "Start mpv player."
  (shell-command
   (format "mpv --idle --input-ipc-server=/tmp/mpvsocket --ytdl-format=bestaudio &")))

(defun org-music--mpv-running-p ()
  "Check if mpv is running."
  (member "mpv" (split-string (shell-command-to-string "playerctl -l"))))

(defun org-music--get-media-url (search-query source)
  "Retrieve media url from SOURCE based on SEARCH-QUERY."
  (interactive)
  (if (equal "nextcloud" source)
      (org-music--get-nextcloud-url search-query)
    (org-music--get-youtube-url search-query)))

(defun org-music--emms-enqueue (search-query &optional source)
  "Enqueue media retrieved from SOURCE based on SEARCH-QUERY."
  (emms-add-url (org-music--get-media-url search-query source)))

(defun org-music--emms-play (search-query &optional source)
  "Play media retrieved from SOURCE based on SEARCH-QUERY."
  (emms-play-url (org-music--get-media-url search-query source)))

(defun org-music--mpv-enqueue (search-query)
  "Enqueue in mpv the top result for SEARCH-QUERY on Youtube."
  (when (not (org-music--mpv-running-p))
      (org-music--mpv-start))
  (shell-command-to-string
   (format
    "echo '{ \"command\": [\"loadfile\", \"ytdl://ytsearch:\\\"%s\\\"\", \"append-play\"] }' | socat - /tmp/mpvsocket"
    search-query)))

(defun org-music-jump-to-random-song (&optional match)
  "Jump to a random song satisfying 'MATCH' in the music library."
  (interactive)
  (let ((org-randomnote-candidates (list org-music-file))
        (song-match (concat (or match org-music-last-playlist-filter "") "+TYPE=\"song\"")))
    (setq org-music-last-playlist-filter match)
    (org-randomnote song-match)))

(defun org-music-play-random-song (&optional match enqueue)
  "Play (or ENQUEUE) random song satisfying 'MATCH' in the music library."
  (interactive)
  (org-music-jump-to-random-song (or match org-music-last-playlist-filter nil))
  (if enqueue
      (org-music-enqueue-song-at-point)
    (org-music-play-song-at-point))
  (bury-buffer))

(defun org-music-play-random-songs (&optional match)
  "Play random songs satisfying 'MATCH' in the music library."
  (interactive)
  (let ((playlist-filter (or match org-music-last-playlist-filter)))
    (org-music-play-random-song playlist-filter)
    (add-hook 'emms-player-finished-hook 'org-music-play-random-songs)
    (add-hook 'emms-player-stopped-hook #'(lambda () (remove-hook 'emms-player-finished-hook 'org-music-play-random-songs)))))

(defun org-music-play-highlighted (start end)
  "Play highlighted text between START and END."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring-no-properties start end)))
        (emms-play-url (org-music--get-media-url regionp "youtube")))))

(defun org-music-enqueue-highlighted (start end)
  "Enqueue highlighted text between START and END."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring-no-properties start end)))
        (emms-add-url (org-music--get-media-url regionp "youtube")))))

(defun org-music--mpv-play (search-query)
  "Enqueue in mpv the top result for SEARCH-QUERY on Youtube."
  (when (not (org-music--mpv-running-p))
      (org-music--mpv-start))
  (shell-command-to-string
   (format
    "echo '{ \"command\": [\"loadfile\", \"ytdl://ytsearch:\\\"%s\\\"\", \"append\"] }' | socat - /tmp/mpvsocket"
    search-query)))

(defun org-music-enqueue-song-at-point ()
  "Enqueue song at point."
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (org-music--search-song-at-point))
        (source (org-music--get-song-source)))
    (org-music--play-cached-song (car (org-music--flatten query)) source t)
    (message "Streaming: %s" song-name)
    (org-music--log-song-state "ENQUEUED")))

(defun org-music-play-song-at-point ()
  "Open song at point."
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (org-music--search-song-at-point))
        (source (org-music--get-song-source)))
    (org-music--play-cached-song (car (org-music--flatten query)) source nil)
    (message "Streaming: %s" song-name)
    (org-music--log-song-state "ENQUEUED")))

(defun org-music-enqueue-list (&optional songs-list)
  "Enqueue SONGS-LIST in active/narrowed/sparse-tree region of org buffer."
  (interactive)
  (let ((songs (or songs-list (org-music--get-org-headings))))
    (mapcar #'(lambda (s)
                (apply #'org-music--play-cached-song (append s (list t))))
            songs)))

(defun org-music-play-list ()
  "Play songs in active/narrowed/sparse-tree region of org buffer."
  (interactive)
  (let ((songs (org-music--get-org-headings)))
    (apply #'org-music--play-cached-song (pop songs))
    (org-music-enqueue-list songs)))

(defun swap (LIST el1 el2)
  "Swap LIST indices EL1 and EL2 in place."
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))

(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length LIST))))
           do (let ((j (random (+ i 1))))
                (swap LIST i j)))
  LIST)

(defun fetch-json (url)
  "Fetch json from  URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (1+ url-http-end-of-headers))
    (json-read)))

(defun fetch-samvayati-moods ()
  "Fetch contextual moods from samvayati."
  (let* ((json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'string))
    (nth 0 (cdadar (fetch-json
                    (format "%s/music?type=mood" org-music-samvayati-root-url))))))

(defun org-music-play-contextual-music ()
  "Play a contextually relevant song."
  (interactive)
  (let* ((moods (fetch-samvayati-moods))
         (play-mood (car (shuffle moods))))
    (message "Playing: %s of Moods: %s" play-mood moods)
    (org-music-play-random-song play-mood)))

(defun org-music-contextual-playlist (playlist-length)
  "Create, play a contextually relevant playlist of PLAYLIST-LENGTH."
  (interactive)
  (let* ((moods (fetch-samvayati-moods))
         (play-mood (car (shuffle moods))))
    (org-music-play-random-song play-mood)
    (cl-loop for i in (number-sequence 1 (1- playlist-length))
             do (let ((play-mood (car (shuffle moods))))
                  (message "Playing: %s of Moods: %s" play-mood moods)
                  (org-music-play-random-song play-mood t)))))

;; Control Music Player on Android via Termux
;; ------------------------------------------
(defun org-music-play-agenda-on-android (search-string)
  "Play `org-agenda' SEARCH-STRING filtered playlist on android."
  ;; filter songs in music library using search terms
  (execute-kbd-macro (kbd (format "C-c a p %s SPC +{:TYPE:\\s-+song}" (replace-regexp-in-string " " " SPC " search-string))))
  ;; write filtered playlist to org file and open
  (org-agenda-write "~/.playlist.org" t)
  ;; get song-name from org playlist's headings, format it to enqueue and play in mpsyt, trigger mpsyt
  (org-music-play-list-on-android)
  (kill-buffer ".playlist.org"))

(defun org-music-play-list-on-android ()
  "Create playlist from org song headings and share via Termux to android music player."
  (org-music--create-m3u-playlist (org-music--get-org-headings))
  (org-music--android-share-playlist))

(defun org-music--create-m3u-playlist (song-entries)
  "Create m3u playlist from SONG-ENTRIES."
  (org-music--write-playlist-to-file
   (mapconcat
    #'(lambda (song)
        (format "#EXTINF:,%s\n%s"
                (car song)
                (apply #'org-music--cache-song (append song (list "android")))))
    song-entries
    "\n")))

(defun org-music--write-playlist-to-file (m3u-playlist)
  "Write M3U-PLAYLIST to file."
  (let ((playlist-file (format "%s%s" org-music-media-directory "orgmusic.m3u")))
    (write-region (format "#EXTM3U\n%s" m3u-playlist) nil playlist-file)))

(defun org-music--android-share-playlist ()
  "Share playlist via termux to an android music player."
  (let ((playlist-file (format "%s%s" org-music-android-media-directory "orgmusic.m3u")))
  (shell-command-to-string (format "termux-open %S" playlist-file))))

(defun org-music--get-youtube-url (search-query)
  "Retrieve URL of the top result for SEARCH-QUERY on Youtube."
  (format "https://youtube.com/watch?v=%s" (org-music--get-youtube-id-of-song (list search-query))))

(defun org-music--get-youtube-id-of-song (song-entry)
  "Retrieve id of top result for SONG-ENTRY on Youtube."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "youtube-dl --get-id ytsearch:\"%s\"" (car song-entry)))))

(defun org-music--android-share-youtube-song (song-id)
  "Share constructed youtube-url based on SONG-ID via termux to play on android youtube player."
  (shell-command-to-string (format "termux-open-url \"https://youtube.com/watch?v=%s\"" song-id)))

(defun org-music--get-nextcloud-url (song-entry)
  "Retrieve URL of top result on Nextcloud for SONG-ENTRY."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "%s \"get_url\" \"%s\"" org-music-next-cloud-script (car song-entry)))))

(defun org-music--get-song (name file-location)
  "Download NAME to FILE-LOCATION from Youtube."
  (interactive)
  (let ((download-command
         (format "youtube-dl -f %s --quiet ytsearch:%S -o %S" org-music-cache-song-format name file-location)))
    (message "%s" download-command)
    (shell-command-to-string download-command)))

(defun org-music--play-cached-song (song-entry &optional source enqueue)
  "Cache SONG-ENTRY from SOURCE. Enqueue song if ENQUEUE true else play."
  (interactive)
  (let ((uri-location (org-music--cache-song song-entry source)))
    (message "location: %s, source: %s" uri-location source)
    (if (equal source "nextcloud")
        (if enqueue
            (emms-add-url uri-location)
          (emms-play-url uri-location))
      (if enqueue
          (emms-add-file uri-location)
      (emms-play-file uri-location)))))

(defun org-music--cache-song (song-name &optional source os)
  "If SONG-NAME not available in local, download from SOURCE. Return local song URI based on OS."
  (interactive)
  (let ((song-file-location
         (format "%s%s.%s" org-music-media-directory song-name org-music-cache-song-format)))
    (message "cache location: %s, source: %s" song-file-location source)
    (if (equal source "nextcloud")
        (org-music--get-nextcloud-url (if (listp song-name) song-name (list song-name)))
       ;; if file doesn't exist, trim cache and download file
      (if (not (file-exists-p song-file-location))
          (progn
            (org-music--trim-cache os)
            (org-music--get-song song-name song-file-location)))
      ;; return song uri based on operating system
      (if (equal os "android")
          (format "%s%s.%s" org-music-android-media-directory song-name org-music-cache-song-format)
        song-file-location))))

(defun org-music--trim-cache (&optional os)
  "Trim media cache if larger than cache-size. Handle different file return ordering based on OS."
  (interactive)
  (let* ((pre-sorted-files
          (sort
           (directory-files (expand-file-name org-music-media-directory) t directory-files-no-dot-files-regexp)
           'file-newer-than-file-p))
         (sorted-files (if (equal os "android")
                           (reverse pre-sorted-files)
                         pre-sorted-files)))
    (message "trim music cache of: %s" (butlast sorted-files org-music-cache-size))
    (mapcar 'delete-file (butlast sorted-files org-music-cache-size))))

;; Org Music Library Metadata Enhancement Methods
;; ----------------------------------------------
(defun org-music--append-outline-to-song-entries ()
  "Add outline parents of all visible song headings to their org entries."
  (interactive)
  (while (not (org-entry-get nil "SEQ"))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil nil " " t)))
      (if (equal "song" (org-entry-get nil "TYPE"))
          (org-music--insert-outline-of-entry outline)))))

(defun org-music--append-outline-to-song-query-property ()
  "Add outline parents of all visible song headings to their query property."
  (interactive)
  (while (not (org-entry-get nil "SEQ"))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil t " - " t)))
      (if (equal "song" (org-entry-get nil "TYPE"))
          (org-entry-put nil "QUERY" outline)))))

(defun org-music--remove-colon-from-org-headings ()
  "Remove colon from non song org headings."
  (interactive)
  (while (or (not (org-entry-get nil "SEQ"))
             (outline-invisible-p))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil t " - " t)))
      (if (not (equal "song" (org-entry-get nil "TYPE")))
          (progn
            (end-of-line)
            (if (equal ":" (string (char-before)))
                (delete-char -1)))))))

(defun org-music--insert-outline-of-entry (outline)
  "Add OUTLINE parents of song heading at point to its org entry."
  (progn
    (org-next-visible-heading 1)
    (insert "\n")
    (forward-line -1)
    (insert outline)
    (beginning-of-line)
    (org-cycle)
    (org-previous-visible-heading 1)))

;;;###autoload
;; Configure Org-Music Mode
(define-minor-mode org-music-mode
  "Play music from org"
  :lighter " 𝄗𝄢"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x p o") 'org-music-play-list)
            (define-key map (kbd "C-x p e") 'org-music-enqueue-list)
            (define-key map (kbd "C-x p <SPC>") '(lambda () "play/pause" (interactive) (emms-pause)))
            (define-key map (kbd "C-x p p") '(lambda () "play previous track in playlist" (interactive) (emms-previous)))
            (define-key map (kbd "C-x p n") '(lambda () "play next track in playlist" (interactive) (emms-next)))
            (define-key map (kbd "C-x p r") '(lambda () "play previous track in playlist" (interactive) (org-music-play-random-song)))
            (define-key map (kbd "C-x p R") '(lambda () "play next track in playlist" (interactive) (org-music-play-random-songs)))
            map)

  ;; define music speed commands
  (defun org-music--speed (keys)
    "Use speed commands if at cursor at beginning of an org-heading line"
    (when (and (bolp) (looking-at org-outline-regexp))
      (cdr (assoc keys
                  '(("o" . (lambda () "play song at point" (org-music-play-song-at-point)))
                    ("e" . (lambda () "enqueue song at point" (org-music-enqueue-song-at-point)))
                    ("s" . (lambda () "play/pause" (message "toggle play/pause") (emms-pause)))
                    ("d" . (lambda () "play next track in playlist" (emms-previous)))
                    ("f" . (lambda () "play previous track in playlist" (emms-next))))))))

    ;; add to org-speed-command-hook
  (add-hook 'org-speed-command-hook 'org-music--speed))

(provide 'org-music)

;;; org-music.el ends here
