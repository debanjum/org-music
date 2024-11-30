;;; -*- lexical-binding: t -*-
;;; org-music.el --- Store and play music from a simple org-mode file

;; Copyright (C) 2017-2023 Debanjum Singh Solanky

;; Author: Debanjum Singh Solanky <debanjum@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (emms "5.0"))
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
;; interactive music library. Pulls song from Youtube by default
;; (using youtube-dl) or from your Nextcloud when specified. Get
;; syncing, versioning, tagging, sharing, semantic, full text search
;; and more straight out of the box. It's plain-text after all!
;;; Code:


(require 'shell)
(require 'org)
(require 'emms)
(require 'url)
(require 'json)

(declare-function emms-add-url "ext:emms")
(declare-function emms-play-url "ext:emms")
(declare-function emms-add-file "ext:emms")
(declare-function emms-play-file "ext:emms")
(declare-function emms-play-playlist "ext:emms")
(declare-function org-agenda-write "ext:org-agenda")
(declare-function org-element-property "ext:org-element")
(declare-function org-element-map "ext:org-element")
(declare-function org-element-parse-buffer "ext:org-element")
(defvar url-http-end-of-headers)

;; Org Music Library Media Control
;; -------------------------------
(defcustom org-music-file "~/Notes/Music.org"
  "Music org file location."
  :group 'org-music
  :type '(file :must-match t))

(defcustom org-music-media-directory "~/Music/OrgMusic/"
  "Media cache location.
Read, write path on unixes. Write path on Android relative to Termux root"
  :group 'org-music
  :type 'directory)

(defcustom org-music-android-media-directory "file:///storage/emulated/0/Music/OrgMusic/"
  "Media cache location on Android. Path relative to Android FS root.
Used to retrieve songs, playlists on android media player."
  :group 'org-music
  :type 'string)

(defcustom org-music-next-cloud-script "~/Scripts/bin/nextcloud.py"
  "Location of Nextcloud script. Used to get nextcloud url of media."
  :group 'org-music
  :type '(file :must-match t))

(defcustom org-music-operating-system "android"
  "Operating system org-music running on."
  :group 'org-music
  :type 'string)

(defcustom org-music-playlist-file "orgmusic.m3u"
  "Name of org-music playlist file."
  :group 'org-music
  :type 'string)

(defcustom org-music-cache-size 100
  "Media cache size."
  :group 'org-music
  :type 'integer)

(defcustom org-music-youtube-downloader "youtube-dl"
  "Youtube downloader to use. Needs to be compatible with youtube-dl CLI flags."
  :group 'org-music
  :type 'string)

(defcustom org-music-cache-song-format "m4a"
  "Format to store songs in cache. See youtube-dl for available formats."
  :group 'org-music
  :type 'string)

(defcustom org-music-cache-download-song-format "mp4"
  "Format to download songs in before audio extraction. See youtube-dl for available formats."
  :group 'org-music
  :type 'string)

(defvar org-music-last-playlist-filter nil
  "Last org filter used to create playlist.")

(defconst org-music--not-on-song-type-heading
  "𝄗𝄢 Error: Can only play a heading of song type (:PROPERTIES: :TYPE: song)"
  "Error message if cursor not on song type heading.")

(defun org-music--flatten (l)
  "Org-Music--Flatten recursive list L."
  (cond ((null l) nil)
   ((atom l) (list l))
   (t (cl-loop for a in l appending (org-music--flatten a)))))

(defun org-music--get-song-properties-of-entry (entry)
  "Extract title, category and (query property else title) of ENTRY."
  (let ((query (org-element-property :QUERY entry))
        (category (org-element-property :CATEGORY entry))
        (type (org-element-property :TYPE entry))
        (title (org-element-property :raw-value entry)))
    (when (equal "song" type)
      (list title (or query title) (or category "youtube")))))

(defun org-music--get-org-headings ()
  "Extract song headings from active/narrowed/sparse-tree region of org buffer."
  (if (use-region-p)
      (narrow-to-region (point) (mark)))
  (let ((headings
         (org-element-map (org-element-parse-buffer "object" t) 'headline 'org-music--get-song-properties-of-entry)))
    (message "%s" headings)
    (widen)
    headings))

(defun org-music--log-song-state (state)
  "Add STATE: `org-current-effective-time` to song's LOGBOOK."
  (let ((opos (point))
        (log-spos (org-log-beginning t))
        (log-time (format-time-string
                   (org-time-stamp-format 'long 'inactive)(org-current-effective-time))))
    (goto-char log-spos)
    (insert state ": " log-time "\n")
    (forward-line -1)
    (org-indent-region (line-beginning-position) (line-end-position))
    (save-buffer)
    (goto-char opos)))

(defun org-music--get-search-query ()
  "Retrieve search query from active/narrowed/sparse-tree region of org buffer.
search query is in QUERY property values or headings of org entries."
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
  (cond ((org-entry-get nil "QUERY"))
        ((nth 4 (org-heading-components)))))

(defun org-music--get-song-source ()
  "Retrieve data source from CATEGORY property value of song's org-entry if exists.
Defaults to Youtube."
  (interactive)
  (cond ((org-entry-get nil "CATEGORY"))
        ("youtube")))

;; Music Player Controls
;; ---------------------
(defun org-music--get-media-url (search-query source)
  "Retrieve media url from SOURCE based on SEARCH-QUERY."
  (if (equal "nextcloud" source)
      (org-music--get-nextcloud-url search-query)
    (org-music--get-youtube-url search-query)))

(defun org-music--emms-enqueue (search-query &optional source)
  "Enqueue media retrieved from SOURCE based on SEARCH-QUERY."
  (emms-add-url (org-music--get-media-url search-query source)))

(defun org-music--emms-play (search-query &optional source)
  "Play media retrieved from SOURCE based on SEARCH-QUERY."
  (emms-play-url (org-music--get-media-url search-query source)))

(defun org-music--get-random-songs (tags-match playlist-length)
  "Get PLAYLIST-LENGTH random songs satisfying TAGS-MATCH from ORG-MUSIC-FILE."
  (with-current-buffer (find-file-noselect org-music-file)
    (save-excursion
      (last
       (org-music--shuffle
        (org-scan-tags
         '(org-music--get-song-properties-of-entry (org-element-at-point))
         (cdr (org-make-tags-matcher (format "%s TYPE=song" tags-match)))
         nil))
       playlist-length))))

(defun org-music-play-random-song (&optional match enqueue)
  "Play (or ENQUEUE) random song satisfying 'MATCH' in the music library."
  (interactive)
  (let ((song (car (org-music--get-random-songs (or match org-music-last-playlist-filter "") 1)))
        (enqueue (or enqueue nil)))
    (apply #'org-music--play-cached-song (append song (list enqueue)))))

(defun org-music-play-random-songs (&optional match)
  "Play random songs satisfying 'MATCH' in the music library."
  (interactive)
  (let ((playlist-filter (or match org-music-last-playlist-filter)))
    (setq org-music-last-playlist-filter playlist-filter)
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

(defun org-music-enqueue-song-at-point ()
  "Enqueue song at point. Download song if necessary."
  (interactive)
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (org-music--search-song-at-point))
        (source (org-music--get-song-source)))
    (if (equal "song" (org-entry-get nil "TYPE"))
        (progn
          (org-music--play-cached-song song-name (car (org-music--flatten query)) source t)
          (message "Streaming: %s" song-name)
          (org-music--log-song-state "ENQUEUED"))
      (message org-music--not-on-song-type-heading))))

(defun org-music-play-song-at-point ()
  "Play song at point. Download song if necessary."
  (interactive)
  (let ((song-name (format "%s" (nth 4 (org-heading-components))))
        (query (org-music--search-song-at-point))
        (source (org-music--get-song-source)))
    (if (equal "song" (org-entry-get nil "TYPE"))
        (progn
          (org-music--play-cached-song song-name (car (org-music--flatten query)) source nil)
          (message "Streaming: %s" song-name)
          (org-music--log-song-state "ENQUEUED"))
      (message org-music--not-on-song-type-heading))))

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
  (let* ((songs (org-music--get-org-headings))
         ;; play the first song
         (first-song (pop songs))
         ;; enqueue rest of the songs using callback once the first song is playing
         (callback (lambda () (org-music-enqueue-list songs))))
    (apply #'org-music--play-cached-song (append first-song (list nil callback)))))

(defun org-music--active-minor-modes ()
  "List active minor modes in current buffer."
  (seq-filter (lambda (mode) (and (boundp mode) (symbol-value mode))) minor-mode-list))

(defun org-music--swap (LIST el1 el2)
  "Swap LIST indices EL1 and EL2 in place."
  (cl-psetf (elt LIST el2) (elt LIST el1)
         (elt LIST el1) (elt LIST el2))
  LIST)

(defun org-music--shuffle (LIST)
  "Shuffle the elements in LIST. Shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length LIST))))
           do (let ((j (random (+ i 1))))
                (org-music--swap LIST i j)))
  LIST)

;; Control Music Player on Android via Termux
;; ------------------------------------------
(defun org-music-play-agenda-on-android (search-string)
  "Play `org-agenda' SEARCH-STRING filtered playlist on android."
  ;; filter songs in music library using search terms
  (execute-kbd-macro (kbd (format "C-c a p %s SPC +{:TYPE:\\s-+song}" (replace-regexp-in-string " " " SPC " search-string))))
  ;; write filtered playlist to org file and open
  (org-agenda-write "~/.playlist.org" t)
  ;; get song-name from org playlist's headings, format it to enqueue and play in mpsyt, trigger mpsyt
  (org-music-play-playlist)
  (kill-buffer ".playlist.org"))

(defun org-music-play-playlist ()
  "Create playlist from org song headings. Share playlist with OS specific player."
  (interactive)
  (org-music--create-m3u-playlist (org-music--get-org-headings))
  (org-music--share-playlist))

(defun org-music--create-m3u-playlist (song-entries &optional stream-p)
  "Create m3u playlist from SONG-ENTRIES.
Stream if STREAM-P, Else Download and Play Cached."
  (org-music--write-playlist-to-file
   (mapconcat
    #'(lambda (song)
        (format "#EXTINF:,%s\n%s"
                (car song)
                (if stream-p
                    (apply #'org-music--get-media-url song)
                  (apply #'org-music--cache-song song))))
    song-entries
    "\n")))

(defun org-music--write-playlist-to-file (m3u-playlist)
  "Write M3U-PLAYLIST to file."
  (let ((playlist-file (format "%s%s"
                               (file-name-as-directory org-music-media-directory)
                               org-music-playlist-file)))
    (write-region (format "#EXTM3U\n%s" m3u-playlist) nil playlist-file)))

(defun org-music--share-playlist ()
  "Share playlist based on operating system.
Share with emms on unixes and android music player via termux on android."
  (if (equal org-music-operating-system "android")
      (shell-command-to-string (format "termux-open \"%s%s\"" org-music-android-media-directory org-music-playlist-file))
    (emms-play-playlist (format "%s%s"
                                (expand-file-name (file-name-as-directory org-music-media-directory))
                                org-music-playlist-file))))

(defun org-music--get-youtube-url (search-query)
  "Retrieve URL of the top result for SEARCH-QUERY on Youtube."
  (format "https://youtube.com/watch?v=%s" (org-music--get-youtube-id-of-song (list search-query))))

(defun org-music--get-youtube-id-of-song (song-entry)
  "Retrieve id of top result for SONG-ENTRY on Youtube."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "%s --no-mtime --get-id ytsearch:\"%s\"" org-music-youtube-downloader (car song-entry)))))

(defun org-music--android-share-youtube-song (song-id)
  "Share constructed youtube-url based on SONG-ID via Termux.
Allows playing song on Android youtube player."
  (shell-command-to-string (format "termux-open-url \"https://youtube.com/watch?v=%s\"" song-id)))

(defun org-music--get-nextcloud-url (song-entry)
  "Retrieve URL of top result on Nextcloud for SONG-ENTRY."
  (replace-regexp-in-string
   "\n$" ""
   (shell-command-to-string
    (format "%s \"get_url\" \"%s\"" org-music-next-cloud-script (car song-entry)))))

(defun org-music--get-song (query file-location &optional callback)
  "Download song satisfying QUERY from Youtube to FILE-LOCATION asynchronously.
Optional CALLBACK function is called with (process, event) when download completes."
  (let* ((download-file-location
          (replace-regexp-in-string
           (format ".%s$" org-music-cache-song-format)
           (format ".%s" org-music-cache-download-song-format)
           file-location))
         (process-name (format "%s-%s" org-music-youtube-downloader (secure-hash 'sha1 query)))
         (process-buffer (format "*%s*" process-name))
         (program-args
          (list "--no-mtime"          ; don't modify file timestamps
                "-f" org-music-cache-download-song-format
                "-x"                  ; extract audio
                "--audio-format" org-music-cache-song-format
                "--quiet"            ; reduce output verbosity
                (format "ytsearch:%s" query)
                "-o" download-file-location))
         ;; Create sentinel function that closes over needed variables
         (sentinel-fn
          (lambda (process event)
            (let ((exit-status (process-exit-status process)))
              (if (= exit-status 0)
                  (progn
                    (message "Successfully downloaded: %s" query)
                    (when callback
                      (funcall callback process event)))
                (message "Failed to download: %s (exit code: %d)" query exit-status))
              ;; Cleanup process buffer
              (when (get-buffer process-buffer)
                (kill-buffer process-buffer))))))

    ;; Create process buffer
    (with-current-buffer (get-buffer-create process-buffer)
      (erase-buffer))

    ;; Start async process and set sentinel
    (let ((proc (apply #'start-process
                      process-name
                      process-buffer
                      org-music-youtube-downloader
                      program-args)))
      (set-process-sentinel proc sentinel-fn)
      proc)))

(defun org-music--play-cached-song (song-name song-entry source enqueue &optional callback)
  "Cache SONG-ENTRY from SOURCE as SONG-NAME.
Enqueue song if ENQUEUE true else play."
  (let* ((playback-fn
          (lambda (uri)
            (if (equal source "nextcloud")
                (if enqueue
                    (emms-add-url uri)
                  (emms-play-url uri))
              (if enqueue
                  (emms-add-file uri)
                (emms-play-file uri)))
            (when callback
              (funcall callback)))))
    ;; Cache song with playback function as callback
    (org-music--cache-song song-name song-entry source playback-fn)))

(defun org-music--cache-song (song-name song-query source callback)
  "If SONG-NAME not available in local, download from SOURCE using SONG-QUERY.
CALLBACK is called with the song URI when ready. For local files, this happens
after download completes. For nextcloud, this happens immediately."
  (let ((song-file-location
         (format "%s%s.%s"
                 (file-name-as-directory org-music-media-directory)
                  song-name
                  org-music-cache-song-format)))
    (message "cache location: %s, source: %s" song-file-location source)
    (if (equal source "nextcloud")
        (let ((uri (org-music--get-nextcloud-url
                   (if (listp song-name) song-name (list song-name)))))
          ;; For nextcloud, call callback immediately with URI
          (when callback (funcall callback uri))
          uri)
      ;; For local files, check if download needed
      (if (file-exists-p song-file-location)
          ;; If file exists, call callback immediately
          (progn
            (set-file-times song-file-location)  ; update access time
            (when callback
              (funcall callback
                      (if (equal org-music-operating-system "android")
                          (format "%s%s.%s"
                                  org-music-android-media-directory
                                  song-name
                                  org-music-cache-song-format)
                        song-file-location)))
            song-file-location)
        ;; If file doesn't exist, download and setup callback
        (progn
          (org-music-trim-cache)
          (org-music--get-song
           song-query
           song-file-location
           (lambda (_process _event)
             (message "Download complete: %s" song-name)
             (when callback
               (funcall callback
                       (if (equal org-music-operating-system "android")
                           (format "%s%s.%s"
                                   org-music-android-media-directory
                                   song-name
                                   org-music-cache-song-format)
                         song-file-location))))))))))

(defun org-music-trim-cache ()
  "Trim media cache if larger than cache-size.
Handle different file return ordering based on OS."
  (interactive)
  (let ((sorted-files
          (reverse
          (sort
           (directory-files (expand-file-name (file-name-as-directory org-music-media-directory)) t "m4a$")
           'file-newer-than-file-p))))
    (message "trim music cache of: %s" (butlast sorted-files org-music-cache-size))
    (mapcar 'delete-file (butlast sorted-files org-music-cache-size))))

;; Org Music Library Metadata Enhancement Methods
;; ----------------------------------------------
(defun org-music-toggle-mark-heading-as-song ()
  "Add property :TYPE: song to heading at point. Remove it, if already present."
  (interactive)
  (if (not (equal "song" (org-entry-get nil "TYPE")))
      (org-set-property "TYPE" "song")
    (org-delete-property "TYPE")))

(defun org-music--append-outline-to-song-entries ()
  "Add outline parents of all visible song headings to their org entries."
  (while (not (org-entry-get nil "SEQ"))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil nil " " t)))
      (if (equal "song" (org-entry-get nil "TYPE"))
          (org-music--insert-outline-of-entry outline)))))

(defun org-music--append-outline-to-song-query-property ()
  "Add outline parents of all visible song headings to their query property."
  (while (not (org-entry-get nil "SEQ"))
    (org-next-visible-heading 1)
    (let ((outline (org-display-outline-path nil t " - " t)))
      (if (equal "song" (org-entry-get nil "TYPE"))
          (org-entry-put nil "QUERY" outline)))))

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

(defun org-music-jump-to-current-song ()
  "Jump to the Org headline of the currently playing song in Emms."
  (interactive)
  (let ((music-file org-music-file))
    (if (file-exists-p music-file)
        (progn
          (find-file music-file)
          (let* ((song-path (emms-track-name
                             (emms-playlist-current-selected-track)))
                 (outline-name (when (and song-path
                                          (string-match ".*/\\(.*\\)\\.m4a" song-path))
                                 (match-string 1 song-path)))
                 (outline-marker (and outline-name
                                      (org-find-exact-headline-in-buffer outline-name))))
            (if outline-marker
                (goto-char outline-marker)
              (message "Song '%s' not found in Org file." outline-name))))
      (message "Music file '%s' does not exist." music-file))))

;;;###autoload
;; Configure Org-Music Mode
(define-minor-mode org-music-mode
  "Play music from org."
  :lighter " 𝄢"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x p o") 'org-music-play-list)
            (define-key map (kbd "C-x p e") 'org-music-enqueue-list)
            (define-key map (kbd "C-x p s") 'org-music--toggle-mark-heading-as-song)
            (define-key map (kbd "C-x p <SPC>") #'(lambda () "play/pause" (interactive) (emms-pause)))
            (define-key map (kbd "C-x p p") #'(lambda () "play previous track in playlist" (interactive) (emms-previous)))
            (define-key map (kbd "C-x p n") #'(lambda () "play next track in playlist" (interactive) (emms-next)))
            (define-key map (kbd "C-x p r") #'(lambda () "play previous random track in playlist" (interactive) (org-music-play-random-song)))
            (define-key map (kbd "C-x p R") #'(lambda () "play next random track in playlist" (interactive) (org-music-play-random-songs)))
            map)

  ;; define music speed commands
  (defun org-music--speed (keys)
    "Use speed commands in org-music-mode if cursor at beginning of org-heading"
    (when (and (member 'org-music-mode (org-music--active-minor-modes))
               (bolp) (looking-at org-outline-regexp))
      (cdr (assoc
            keys
            '(("o" . (lambda () "play song at point" (org-music-play-song-at-point)))
              ("e" . (lambda () "enqueue song at point" (org-music-enqueue-song-at-point)))
              ("s" . (lambda () "play/pause" (message "toggle play/pause") (emms-pause)))
              ("d" . (lambda () "play next track in playlist" (emms-previous)))
              ("f" . (lambda () "play previous track in playlist" (emms-next))))))))

    ;; add to org-speed-command-hook
  (add-hook 'org-speed-command-hook 'org-music--speed))

(provide 'org-music)

;;; org-music.el ends here
