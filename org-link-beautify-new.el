;;; org-link-beautify-new.el --- Beautify Org Links -*- lexical-binding: t; -*-

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "30.1") (org "9.7.17") (nerd-icons "0.0.1") (qrencode "1.3"))
;; Version: 2.0.0
;; Keywords: hypermedia
;; Homepage: https://repo.or.cz/org-link-beautify.git

;; org-link-beautify is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; org-link-beautify is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; (use-package org-link-beautify
;;   :ensure t
;;   :init (org-link-beautify-mode t))

;;; Code:

(require 'org) ; including `ol'
(require 'org-element)
(require 'org-element-ast)
;; (eq (plist-get (cdr (assoc "attachment" org-link-parameters)) :preview) 'org-attach-preview-file)
(unless (assoc "attachment" org-link-parameters)
  (require 'org-attach))
(require 'cl-lib)
(require 'color)
(require 'faces)
(require 'image)
(require 'nerd-icons)
(require 'qrencode)

;;; Customization

(defgroup org-link-beautify nil
  "Customize group of `org-link-beautify-mode'."
  :prefix "org-link-beautify-"
  :group 'org)

(defcustom org-link-beautify-thumbnails-dir 'current-working-directory
  "The directory of generated thumbnails.

By default option value with symbol 'current-working-directory the
thumbnails are generated in source file path’s .thumbnails directory.
This is better for avoiding re-generate preview thumbnails. Or you can
set this option to 'user-home which represent to ~/.cache/thumbnails/."
  :type 'symbol
  :safe #'symbolp
  :group 'org-link-beautify)

(defcustom org-link-beautify-enable-debug-p nil
  "Whether enable org-link-beautify print debug info."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)


;;; overlay keymap keybindings

(defvar org-link-beautify-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-map) ; inherit `image-map' keybindings on preview thumbnail image.
    map))

;;;; goto file in Dired

(declare-function org-attach-reveal "org-attach" ())

(defun org-link-beautify-action-goto-file-in-dired ()
  "Action of opening Dired and goto the link file position."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((file-path (org-element-property :path (org-element-context)))
           (file-name (file-name-nondirectory file-path)))
      (org-attach-reveal)
      (search-forward file-name)
      (dired-move-to-filename) ; move point to beginning of filename.
      (if (and (featurep 'dwim-shell-command) (featurep 'dwim-shell-commands))
          (progn
            (message "Jumped to position of link file, now you can execute `dwim-shell-command' commands")
            (command-execute nil (read-extended-command-1 nil "dwim-shell-commands")))
        (user-error "Jumped to position of link file.
Package `dwim-shell-command' is missing, please install it")))))

(define-key org-link-beautify-keymap (kbd "M-o") #'org-link-beautify-action-goto-file-in-dired)

;;;; browse URL

;; ;; NOTE: It will override all link type handlers.
;; (defun org-link-beautify-action-browse-url (&optional link-str)
;;   "Visit the URL in LINK-STR with `browse-url'."
;;   (interactive)
;;   (let ((link-type (org-element-property :type (org-element-link-parser)))
;;         (link-raw (or link-str (org-element-property :raw-link (org-element-link-parser)))))
;;     (if (member link-type '("http" "https"))
;;         (browse-url link-raw)
;;       ;; `org-link-open-from-string'
;;       (org-link-open link-raw))))
;;
;; (define-key org-link-beautify-keymap (kbd "C-o") #'org-link-beautify-action-browse-url)

;;;; copy file to clipboard

(defun org-link-beautify--copy-file-to-clipboard (file)
  "Copy the FILE on path to clipboard.
The argument FILE must be the absolute path."
  (interactive "P")
  (if current-prefix-arg
      (let ((destination (read-file-name (format "[org-link-beautify] copy file %s to: " (file-name-nondirectory file)))))
        (copy-file file destination)
        (message "[org-link-beautify] Copied file [%s] to [%s]" (file-name-nondirectory file) (file-name-directory file)))
    (cl-case system-type
      (darwin
       (do-applescript
        (format "tell app \"Finder\" to set the clipboard to ( POSIX file \"%s\" )" file)))
      (gnu/linux
       ;; - xclip-copyfile :: command copies files into the X clipboard, recursing into directories.
       ;; - xclip-cutfile :: command Copy the files, but also deletes them afterwards.
       ;; - xclip-pastefile :: command Paste the files out of the clipboard.
       ;; - xclip :: command Copy text or files to the clipboard.
       (if (executable-find "xclip-copyfile")
           (shell-command (format "xclip-copyfile %s" file))
         (user-error "[org-link-beautify] Error: the command-line tool 'xclip-copyfile' is not installed!")))
      ;; TODO:
      (windows-nt ))
    (message "[org-link-beautify] Copied file [%s] to <system clipboard>" (string-truncate-left file (/ (window-width) 2)))))

(defun org-link-beautify-action-copy-file (&optional args)
  "Action of copying the Org link file at point with optional ARGS."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (if (or (region-active-p) mark-active)
        (let* ((region-text (buffer-substring-no-properties (region-beginning) (region-end))))
          (kill-new region-text)
          (deactivate-mark))
      (let ((element (org-element-context)))
        (if (and (eq (car element) 'link)
                 (string-equal (org-element-property :type element) "file"))
            (let ((file-path (expand-file-name (org-element-property :path element))))
              (org-link-beautify--copy-file-to-clipboard file-path))
          (message "[org-link-beautify] No action executed on link"))))))

(define-key org-link-beautify-keymap (kbd "M-w") #'org-link-beautify-action-copy-file)

;;;; display QR Code for URL

(defun org-link-beautify-action-qrcode-for-url (&optional args)
  "Action of displaying QR code for Org link at point in new buffer in ARGS."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (if-let* ((url (org-element-property-raw :raw-link (org-element-context))))
        (if (require 'qrencode nil t)
            (qrencode-string url)
          (package-install 'qrencode)
          (qrencode-string url))
      (if (or (region-active-p) mark-active)
          (org-fill-paragraph t t)
        (org-fill-paragraph)))))

(define-key org-link-beautify-keymap (kbd "M-q") 'org-link-beautify-action-qrcode-for-url)

;;;; convert video to audio

(defun org-link-beautify--convert-video-to-audio (video-file audio-file)
  "Convert the VIDEO-FILE on path to AUDIO-FILE.
The argument VIDEO-FILE should be the absolute path."
  (if (and (executable-find "ffmpeg")
           (file-name-absolute-p video-file)
           (file-name-absolute-p audio-file))
      (let* ()
        (make-process
         :name (format "org-link-beautify - convert video to audio - %s" (file-name-nondirectory video-file))
         :buffer (format " *org-link-beautify - convert video to audio - %s*" (file-name-nondirectory video-file))
         :command (list "ffmpeg" "-i" video-file audio-file)
         :sentinel (lambda (proc event)
                     (when (string-equal event "finished\n")
                       (message "[org-link-beautify] converted video to audio file [%s]\nPlease update your link path to result audio file."
                                (string-truncate-left audio-file (/ (window-width) 2)))))))))

(defun org-link-beautify-action-convert-video-to-audio (&optional args)
  "Action of converting video file to audio file (.mp3 by default) in ARGS."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((element (org-element-context)))
      (if (and (eq (car element) 'link)
               (string-equal (org-element-property :type element) "file"))
          (let* ((video-file-path (expand-file-name (org-element-property :path element)))
                 (audio-file (file-name-with-extension (file-name-sans-extension (file-name-nondirectory video-file-path)) "mp3"))
                 (audio-file-path (expand-file-name
                                   (read-file-name
                                    (format "[org-link-beautify] convert video file %s to: " (file-name-nondirectory video-file-path))
                                    (file-name-directory video-file-path)
                                    audio-file
                                    nil
                                    audio-file)
                                   (file-name-directory video-file-path))))
            (org-link-beautify--convert-video-to-audio video-file-path audio-file-path)
            ;; TODO: modify link file path
            ;; (org-insert-link nil audio-file-path audio-file-path)
            )
        (user-error "[org-link-beautify] not video file link at point")))))

;; (define-key org-link-beautify-keymap (kbd "M-a") 'org-link-beautify-action-convert-video-to-audio)

;;;; convert video to gif

(defun org-link-beautify--convert-video-to-gif (video-file gif-file)
  "Convert the VIDEO-FILE on path to GIF-FILE.
The argument VIDEO-FILE should be the absolute path."
  (if (and (executable-find "ffmpeg")
           (file-name-absolute-p video-file)
           (file-name-absolute-p gif-file))
      (let* ()
        (make-process
         :name (format "org-link-beautify - convert video to gif - %s" (file-name-nondirectory video-file))
         :buffer (format " *org-link-beautify - convert video to gif - %s*" (file-name-nondirectory video-file))
         :command (list "ffmpeg" "-i" video-file gif-file)
         :sentinel (lambda (proc event)
                     (when (string-equal event "finished\n")
                       (message "[org-link-beautify] converted video to gif file [%s]\nPlease update your link path to result gif file."
                                (string-truncate-left gif-file (/ (window-width) 2)))))))))

(defun org-link-beautify-action-convert-video-to-gif (&optional args)
  "Action of converting video file to gif file (.gif) in ARGS."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((element (org-element-context)))
      (if (and (eq (car element) 'link)
               (string-equal (org-element-property :type element) "file"))
          (let* ((video-file-path (expand-file-name (org-element-property :path element)))
                 (gif-file (file-name-with-extension (file-name-sans-extension (file-name-nondirectory video-file-path)) "gif"))
                 (gif-file-path (expand-file-name
                                 (read-file-name
                                  (format "[org-link-beautify] convert video file %s to: " (file-name-nondirectory video-file-path))
                                  (file-name-directory video-file-path)
                                  gif-file
                                  nil
                                  gif-file)
                                 (file-name-directory video-file-path))))
            (org-link-beautify--convert-video-to-gif video-file-path gif-file-path)
            (push (list (concat "file:" gif-file-path) "") org-stored-links)
            ;; TODO: modify link file path
            ;; (org-insert-link nil gif-file-path gif-file-path)
            )
        (user-error "[org-link-beautify] not video file link at point")))))

;; (define-key org-link-beautify-keymap (kbd "M-g") 'org-link-beautify-action-convert-video-to-gif)

;;;; play music file in repeat mode

(defun org-link-beautify-action-play-music-repeat (&optional args)
  "Action of playing music audio file in repeated mode with ARGS."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let* ((element (org-element-context)))
      (if (and (eq (car element) 'link)
               (string-equal (org-element-property :type element) "file"))
          (let* ((music-file-path (expand-file-name (org-element-property :path element)))
                 (music-extension (file-name-extension music-file-path))
                 (repeat-times 50))
            (if (member music-extension org-link-beautify-audio-preview-list)
                (cond
                 ((executable-find "mpg123")
                  (async-shell-command (format "mpg123 --loop %s %S" repeat-times music-file-path)))
                 ((executable-find "ffplay")
                  (async-shell-command (format "ffplay -loop %s %S" repeat-times music-file-path))))
              (cond
               ((executable-find "mpv")
                (async-shell-command (format "mpv --loop-file %s %S" repeat-times music-file-path)))
               ((executable-find "mplayer")
                (async-shell-command (format "mplayer -loop %s %S" repeat-times music-file-path)))
               ((executable-find "ffplay")
                (async-shell-command (format "ffplay -loop %s %S" repeat-times music-file-path))))))
        (user-error "[org-link-beautify] not music audio file link at point")))))

(define-key org-link-beautify-keymap (kbd "M-p") 'org-link-beautify-action-play-music-repeat)

;;; helper functions

(defun org-link-beautify--get-thumbnails-dir-path (file)
  "Return the FILE thumbnail directory's path."
  (if file
      (cl-case org-link-beautify-thumbnails-dir
        (current-working-directory
         (concat (file-name-directory file) ".thumbnails/"))
        (user-home
         (expand-file-name "~/.cache/thumbnails/")))
    (user-error "[org-link-beautify] Error: the paramter `file' of function `org-link-beautify--get-thumbnails-dir-path' is nil")))

(defun org-link-beautify--ensure-thumbnails-dir (thumbnails-dir)
  "Ensure THUMBNAILS-DIR exist, if not ,create it."
  (if (file-exists-p (file-name-parent-directory thumbnails-dir))
      (unless (file-directory-p thumbnails-dir)
        (make-directory thumbnails-dir))
    (if (yes-or-no-p "[org-link-beautify] thumbnails directory parent directory does not exist, create it?")
        (make-directory thumbnails-dir t)
      (warn "[org-link-beautify] thumbnails directory parent directory does not exist"))))

(defun org-link-beautify--notify-generate-thumbnail-failed (source-file &optional _thumbnail-file)
  "Notify that generating THUMBNAIL-FILE for SOURCE-FILE failed."
  (message "[org-link-beautify] Failed to generate thumbnail for file %s" source-file))

(defun org-link-beautify--display-content-block (lines-list)
  "Display LINES-LIST string as a block with beautified frame border."
  (format
   "
┌%s┐
%s
└%s┘
\n"
   (make-string (- fill-column 2) ?─)
   (mapconcat
    (lambda (line)
      (concat "│ "
              (let* ((line-length-max (- fill-column 4))
                     (line-length (length line))
                     (line-text (if (< line-length line-length-max)
                                    line
                                  (let ((ellipsis (truncate-string-ellipsis)))
                                    (concat (truncate-string-to-width line (- line-length-max (length ellipsis))) ellipsis))))
                     (spaces (make-string
                              (if (< (length line-text) line-length-max)
                                  (- line-length-max (length line-text))
                                0)
                              ?\ )))
                (concat line-text spaces))
              " │"))
    lines-list
    "\n")
   (make-string (- fill-column 2) ?─)))

;; TEST:
;; (org-link-beautify--display-content-block '("hello world" "hello, name" "hello"))

(defun org-link-beautify--display-org-content (org-content)
  "Display ORG-CONTENT in `org-mode'."
  (with-temp-buffer
    (let ((org-startup-with-link-previews nil))
      (insert org-content)
      (org-mode)
      ;; (goto-char (line-beginning-position 2))
      ;; FIXME: how to fix org-attach directory issue.
      ;; error: "Need absolute ‘org-attach-id-dir’ to attach in buffers without filename"
      ;; reference `org-attach-dir'
      ;; (when (org-entry-get nil "DIR" org-attach-use-inheritance)
      ;;   (org-link-preview-region t t (point-min) (point-max)))
      (buffer-substring (point-min) (point-max)))))

;;; Previewing process logic

(defun org-link-beautify--preview-process-setup-async-timeout (thumbnail-file proc-buffer timeout)
  "Asynchronous timeout mechanism for previewing process which implemented with `run-with-timer'.

Detect previewing process generated THUMBNAIL-FILE existing, auto kill process associated with PROC-BUFFER in TIMEOUT seconds."
  (let* ((start-time (current-time))
         (check-interval 2)
         (timer nil))
    (setq timer
          (run-with-timer
           check-interval check-interval
           (lambda ()
             (let ((elapsed (float-time (time-subtract (current-time) start-time)))
                   (proc (get-buffer-process (get-buffer proc-buffer))))
               (cond
                ;; previewing thumbnail process successed
                ((file-exists-p thumbnail-file)
                 (cancel-timer timer))
                ;; process timeouted
                ((>= elapsed (or timeout 20))
                 (when proc (delete-process proc))
                 (when (get-buffer proc-buffer) (kill-buffer proc-buffer))
                 (cancel-timer timer))
                ;; process dead
                ((and proc (not (process-live-p proc)))
                 (when (get-buffer proc-buffer) (kill-buffer proc-buffer))
                 (cancel-timer timer)))))))))

;;; Invoke external Python script file or code.

(defcustom org-link-beautify-python-interpreter (executable-find "python3")
  "Specify Python interpreter to run python scripts or code."
  :type 'string
  :safe #'stringp)

(defun org-link-beautify--python-script-run (python-script-file)
  "Run PYTHON-SCRIPT-FILE through shell command."
  (cl-assert (executable-find org-link-beautify-python-interpreter) nil
             "[org-link-beautify] Install \"python\" environment")
  (shell-command-to-string
   (format "%s %s" org-link-beautify-python-interpreter python-script-file)))

(defun org-link-beautify--python-command-to-string (&rest code-lines)
  "Run Python CODE-LINES through shell command."
  (cl-assert (executable-find org-link-beautify-python-interpreter) nil
             "[org-link-beautify] Install \"python\" environment")
  (shell-command-to-string
   (concat org-link-beautify-python-interpreter
           " -c "
           ;; solve double quote character issue.
           "\"" (string-replace "\"" "\\\"" (string-join code-lines "\n")) "\"")))

;; TEST:
;; (org-link-beautify--python-command-to-string
;;  "import numpy as np"
;;  "print(np.arange(6))"
;;  "print(\"blah blah\")"
;;  "print('{}'.format(3))")

;;; Invoke external JavaScript script file or code.

(defcustom org-link-beautify-javascript-interpreter (executable-find "node")
  "Specify JavaScript interpreter to run JavaScript scripts or code."
  :type 'string
  :safe #'stringp)

(defun org-link-beautify--javascript-script-run (javascript-script-file)
  "Run JAVASCRIPT-SCRIPT-FILE through shell command."
  (cl-assert (executable-find org-link-beautify-javascript-interpreter) nil
             "[org-link-beautify] Install \"JavaScript\" environment")
  (shell-command-to-string
   (format "%s %s" org-link-beautify-javascript-interpreter javascript-script-file)))

(defun org-link-beautify--javascript-command-to-string (&rest code-lines)
  "Run JavaScript CODE-LINES through shell command."
  (cl-assert (executable-find org-link-beautify-javascript-interpreter) nil
             "[org-link-beautify] Install \"JavaScript\" environment")
  (shell-command-to-string
   (concat org-link-beautify-javascript-interpreter
           " --eval "
           ;; solve double quote character issue.
           "\"" (string-replace "\"" "\\\"" (string-join code-lines "\n")) "\"")))

;; TEST:
;; (org-link-beautify--javascript-command-to-string
;;  "console.log(\"hello, world!\");"
;;  "console.log(1 + 3);")

;;; Invoke external Ruby script file or code.

(defcustom org-link-beautify-ruby-interpreter (executable-find "ruby")
  "Specify Ruby interpreter to run ruby scripts or code."
  :type 'string
  :safe #'stringp)

(defun org-link-beautify--ruby-script-run (ruby-script-file)
  "Run RUBY-SCRIPT-FILE through shell command."
  (cl-assert (executable-find org-link-beautify-javascript-interpreter) nil
             "[org-link-beautify] Install \"Ruby\" environment")
  (shell-command-to-string
   (format "%s %s" org-link-beautify-ruby-interpreter ruby-script-file)))

(defun org-link-beautify--ruby-command-to-string (&rest code-lines)
  "Run Ruby CODE-LINES through shell command."
  (cl-assert (executable-find org-link-beautify-javascript-interpreter) nil
             "[org-link-beautify] Install \"Ruby\" environment")
  (shell-command-to-string
   (concat org-link-beautify-ruby-interpreter
           " -e "
           ;; solve double quote character issue.
           "\"" (string-replace "\"" "\\\"" (string-join code-lines "\n")) "\"")))

;; TEST:
;; (org-link-beautify--ruby-command-to-string
;;  "puts 'hello, world'"
;;  (format "puts 'hello, %s'" user-full-name))

;;; Iconify for link

(defun org-link-beautify--get-link-description (position)
  "Get the link description at POSITION (fuzzy but faster version)."
  (save-excursion
    (goto-char position)
    (cond
     ((org-element-link-parser)
      (let ((link (org-element-link-parser)))
        (if (org-element-property :contents-begin link)
            (buffer-substring-no-properties
             (org-element-property :contents-begin link)
             (org-element-property :contents-end link))
          (org-element-property :raw-link link))))
     ((org-element-context)
      (let ((link (org-element-link-parser)))
        (org-element-property :raw-link link)))
     (t (save-excursion
          (goto-char position)
          (and (org-in-regexp org-link-bracket-re) (match-string 2)))))))

(defun org-link-beautify--return-warning-face (ov path link)
  "Return warning face if PATH does not exist on OV overlay at LINK element."
  (when (string-equal (org-element-property :type link) "file")
    (if (and (not (file-remote-p path))
             (file-exists-p (expand-file-name path)))
        'org-link
      'error)))

(defun org-link-beautify--return-icon (ov path link)
  "Return icon for PATH on OV overlay at LINK element."
  (let ((type (org-element-property :type link))
        (extension (file-name-extension path)))
    (pcase type
      ;; FIXME: the icon is slant.
      ("file" (nerd-icons-icon-for-file path))
      ("attachment" (nerd-icons-icon-for-file path))
      ("http" (nerd-icons-icon-for-url (concat type ":" path) :face '(:inherit nerd-icons-dsilver :slant normal :weight normal)))
      ("https" (nerd-icons-icon-for-url (concat type ":" path) :face '(:inherit nerd-icons-green :slant normal :weight normal)))
      ("ftp" (nerd-icons-icon-for-url (concat type ":" path) :face '(:inherit nerd-icons-purple :slant normal :weight normal)))
      ;; Org mode internal link types
      ("custom-id" (nerd-icons-mdicon "nf-md-text_box_search_outline" :face '(:inherit nerd-icons-blue :slant normal)))
      ("id" (nerd-icons-mdicon "nf-md-text_search" :face '(:inherit nerd-icons-blue :slant normal)))
      ("coderef" (nerd-icons-codicon "nf-cod-references" :face '(:inherit nerd-icons-cyan :slant normal)))
      ("elisp" (nerd-icons-icon-for-file "file.el" :face '(:inherit nerd-icons-purple :slant normal)))
      ("eshell" (nerd-icons-icon-for-mode 'eshell-mode :face '(:inherit nerd-icons-cyan :slant normal)))
      ("shell" (nerd-icons-icon-for-mode 'shell-mode :face '(:inherit nerd-icons-cyan :slant normal)))
      ("man" (nerd-icons-mdicon "nf-md-file_document_outline" :face '(:inherit nerd-icons-lblue :slant normal)))
      ("woman" (nerd-icons-mdicon "nf-md-file_document_outline" :face '(:inherit nerd-icons-blue :slant normal)))
      ("info" (nerd-icons-icon-for-mode 'Info-mode))
      ("help" (nerd-icons-icon-for-mode 'help-mode))
      ("shortdoc" (nerd-icons-mdicon "nf-md-file_link" :face '(:inherit nerd-icons-blue :slant normal)))
      ;; org-ref link types
      ("cite" (nerd-icons-codicon "nf-cod-references" :face '(:inherit nerd-icons-cyan :slant normal)))
      ;; Org mode external link types
      ("eaf" (nerd-icons-mdicon "nf-md-apps" :face '(:inherit nerd-icons-blue :slant normal))) ; emacs-application-framework (eaf)
      ("eww" (nerd-icons-icon-for-mode 'eww-mode :face '(:inherit nerd-icons-lgreen :slant normal))) ; EWW
      ("chrome" (nerd-icons-mdicon "nf-md-google_chrome" :face '(:inherit nerd-icons-lorange :slant normal))) ; Google Chrome / Chromium
      ("about" (nerd-icons-mdicon "nf-md-firefox" :face '(:inherit nerd-icons-lorange :slant normal))) ; Firefox
      ("edge" (nerd-icons-mdicon "nf-md-microsoft_edge" :face '(:inherit nerd-icons-green :slant normal))) ; Microsoft Edge
      ("mu4e" (nerd-icons-mdicon "nf-md-email_search_outline" :face '(:inherit nerd-icons-blue :slant normal)))
      ("news" (nerd-icons-mdicon "nf-md-newspaper_variant_outline" :face '(:inherit nerd-icons-dgreen :slant normal)))
      ("git" (nerd-icons-mdicon "nf-md-git" :face '(:inherit nerd-icons-lred :slant normal)))
      ("orgit" (nerd-icons-faicon "nf-fa-git" :face '(:inherit nerd-icons-red :slant normal)))
      ("orgit-rev" (nerd-icons-devicon "nf-dev-git_commit" :face '(:inherit nerd-icons-silver :slant normal)))
      ("orgit-log" (nerd-icons-octicon "nf-oct-diff" :face '(:inherit nerd-icons-silver :slant normal)))
      ("pdf" (nerd-icons-faicon "nf-fa-file_pdf" :face '(:inherit nerd-icons-red :slant normal)))
      ("epub" (nerd-icons-mdicon "nf-md-book_open_page_variant_outline" :face '(:inherit nerd-icons-green :slant normal)))
      ("nov" (nerd-icons-icon-for-file "file.epub" :face '(:inherit nerd-icons-green :slant normal))) ; for Emacs package "nov.el" link type `nov:'
      ("grep" (nerd-icons-mdicon "nf-md-selection_search" :face '(:inherit nerd-icons-green :slant normal)))
      ("occur" (nerd-icons-mdicon "nf-md-selection_multiple" :face '(:inherit nerd-icons-green :slant normal)))
      ("rss" (nerd-icons-mdicon "nf-md-rss" :face '(:inherit nerd-icons-lorange :slant normal)))
      ("elfeed" (nerd-icons-mdicon "nf-md-rss" :face '(:inherit nerd-icons-green :slant normal)))
      ("wikipedia" (nerd-icons-mdicon "nf-md-wikipedia" :face '(:inherit nerd-icons-dsilver :slant normal)))
      ("mailto" (nerd-icons-mdicon "nf-md-email_send_outline" :face '(:inherit nerd-icons-lblue :slant normal)))
      ("irc" (nerd-icons-mdicon "nf-md-chat" :face '(:inherit nerd-icons-blue :slant normal)))
      ("wechat" (nerd-icons-mdicon "nf-md-wechat" :face '(:inherit nerd-icons-green :slant normal)))
      ("magnet" (nerd-icons-mdicon "nf-md-magnet" :face '(:inherit nerd-icons-blue :slant normal)))
      ("ref" (nerd-icons-codicon "nf-cod-references" :face '(:inherit nerd-icons-blue :slant normal)))
      ("doi" (nerd-icons-mdicon "nf-md-file_document_plus_outline" :face '(:inherit nerd-icons-green :slant normal)))
      ("org-contact" (nerd-icons-mdicon "nf-md-contacts_outline" :face '(:inherit nerd-icons-purple :slant normal)))
      ("org-bookmark" (nerd-icons-mdicon "nf-md-bookmark_check_outline" :face '(:inherit nerd-icons-blue :slant normal)))
      ("org-ql-search" (nerd-icons-mdicon "nf-md-text_box_search_outline" :face '(:inherit nerd-icons-blue :slant normal)))
      ;; org-media-note link types
      ("video" (nerd-icons-faicon "nf-fa-file_video_o" :face '(:inherit nerd-icons-blue :slant normal)))
      ("audio" (nerd-icons-faicon "nf-fa-file_audio_o" :face '(:inherit nerd-icons-blue :slant normal)))
      ("videocite" (nerd-icons-faicon "nf-fa-file_video_o" :face '(:inherit nerd-icons-blue :slant normal)))
      ("audiocite" (nerd-icons-faicon "nf-fa-file_audio_o" :face '(:inherit nerd-icons-blue :slant normal)))
      ("javascript" (nerd-icons-mdicon "nf-md-language_javascript" :face '(:inherit nerd-icons-yellow :slant normal)))
      ("js" (nerd-icons-mdicon "nf-md-language_javascript" :face '(:inherit nerd-icons-yellow :slant normal)))
      ("vscode" (nerd-icons-mdicon "nf-md-microsoft_visual_studio_code" :face '(:inherit nerd-icons-blue :slant normal))) ; Visual Studio Code
      ("macappstore" (nerd-icons-mdicon "nf-md-apple" :face '(:inherit nerd-icons-blue :slant normal))) ; Mac App Store
      
      ("fuzzy"
       ;; Org internal [[reference][reference]] -> NOT supported by:
       ;; `(org-link-set-parameters link-type :activate-func ...)'
       ;;
       ;; (link (:standard-properties [584419 nil 584470 584517 584519 0 nil nil nil nil nil nil ...] :type "fuzzy" :type-explicit-p nil :path "defcustom org-contacts-identity-properties-list" :format bracket :raw-link "defcustom org-contacts-identity-properties-list" ...))
       
       (when-let* ((_ (string-match "\\([^:]*\\):\\(.*\\)" path))
                   (real-type (match-string 1 path))) ; extract the "real" link type for "fuzzy" type in :path.
         (cond
          (t
           (message "[org-link-beautify] DEBUG: link type not supported, add PR for this link type.
type: %s, path: %s, extension: %s, link-element: %s" type path extension link)
           (nerd-icons-mdicon "nf-md-progress_question" :face '(:inherit nerd-icons-lyellow :slant normal))))))
      (_
       (message "[org-link-beautify] DEBUG: link type not supported, add PR for this link type.
type: %s, path: %s, extension: %s, link-element: %s" type path extension link)
       ;; handle when returned link type is `nil'.
       (nerd-icons-mdicon "nf-md-progress_question" :face '(:inherit nerd-icons-lyellow :slant normal))))))

(defun org-link-beautify-iconify (ov path link)
  "Iconify PATH over OV overlay position for LINK element."
  (when-let* ((begin (org-element-begin link))
              (end (org-element-end link))
              (description (org-link-beautify--get-link-description begin))
              (icon (org-link-beautify--return-icon ov path link)))
    (unless (overlay-get ov 'org-image-overlay)
      (overlay-put ov
                   'before-string (concat
                                   (propertize "[" 'face '(:inherit default :foreground "orange"))
                                   ;; (propertize icon 'face '(:inherit nil :slant normal :weight normal))
                                   icon
                                   (propertize "]" 'face '(:inherit default :foreground "orange"))))
      (overlay-put ov
                   'display (concat
                             (propertize "["
                                         'face `( :inherit default
                                                  :underline t
                                                  :foreground ,(if (face-foreground 'shadow)
                                                                   (color-lighten-name (face-foreground 'shadow) 2)
                                                                 "gray40")))
                             (propertize description
                                         'face (org-link-beautify--return-warning-face ov path link)
                                         'read-only t 'intangible t 'inhibit-isearch nil)
                             (propertize "]"
                                         'face `( :inherit default
                                                  :underline t
                                                  :foreground ,(if (face-foreground 'shadow)
                                                                   (color-lighten-name (face-foreground 'shadow) 2)
                                                                 "gray40")))))
      (overlay-put ov 'keymap  org-link-beautify-keymap))
    t))

;;; General thumbnail generator with Python "thumbnail.py" library.

(defvar org-link-beautify-thumbnailer-script
  (expand-file-name "scripts/thumbnailer.py" (file-name-directory (or load-file-name (buffer-file-name))))
  "The path of general thumbnailer script.

It requires Python package 'thumbnail' installed.
$ pip install thumbnail")

(defun org-link-beautify-thumbnailer (path &optional proc-name proc-buffer)
  "Generate thumbnail image for file of PATH over OV overlay position for LINK element."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (input-file (expand-file-name (org-link-unescape file-path)))
           (search-option (match-string 2 path))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path input-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base input-file))))
           (thumbnail-size 600))
      (unless (file-exists-p thumbnail-file)
        (let* ((proc-name (or proc-name (format "org-link-beautify thumbnailer - %s" (file-name-base input-file))))
               (proc-buffer (or proc-buffer (format " *org-link-beautify thumbnailer - %s*" (file-name-base input-file)))))
          (cl-assert (file-exists-p org-link-beautify-thumbnailer-script) nil
                     "[org-link-beautify] The thumbnailer script file does not exist.\n%s"
                     org-link-beautify-thumbnailer-script)
          (make-process
           :name proc-name
           :command (list org-link-beautify-python-interpreter
                          org-link-beautify-thumbnailer-script
                          input-file
                          thumbnail-file
                          "--type" "thumbnail" ;; firstpage
                          "--size" (number-to-string thumbnail-size))
           :buffer proc-buffer
           :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
           :sentinel (lambda (proc event)
                       (when org-link-beautify-enable-debug-p
                         (message (format "[org-link-beautify] > proc: %s\n> event: %s" proc event)))
                       ;; (when (string= event "finished\n")
                       ;;   (kill-buffer (process-buffer proc))
                       ;;   (kill-process proc))
                       ))))
      ;; return the thumbnail file as result.
      thumbnail-file)))

;;; overlay displaying the preview thumbnail

(defvar org-link-beautify-preview-thumbnail-exclude-list
  '("txt")
  "A list of exclude file extensions for `org-link-beautify-preview-thumbnail'.
The function `org-link-beautify-thumbnailer' invoked Python script `thumbnailer.py'.")

(defun org-link-beautify-overlay-display-image (ov image &optional align)
  "Display IMAGE object on overlay OV in ALIGN position.
The IMAGE object is created by `create-image' from `org--create-inline-image'."
  ;; See bug#59902. We cannot rely on Emacs to update image if the file has changed.
  (image-flush image) ; refresh image in cache if file changed.
  ;; (image--set-property image :mask 'heuristic) ; NOTE: caused image partially displaying vanished
  (overlay-put ov 'display image)
  (overlay-put ov 'face    'default)
  (overlay-put ov 'keymap  org-link-beautify-keymap)
  (when align
    (overlay-put ov
                 'before-string (propertize " "
                                            'face 'default
                                            'display (pcase align
                                                       ("center" `(space :align-to (- center (0.5 . ,image))))
                                                       ("right"  `(space :align-to (- right ,image)))))))
  t)

(defun org-link-beautify-preview-thumbnail (ov path link)
  "Display thumbnail on overlay OV from PATH at element LINK."
  (unless (member (file-name-extension path) org-link-beautify-preview-thumbnail-exclude-list)
    (if-let* ((_ (display-graphic-p))
              (thumbnail-file (org-link-beautify-thumbnailer path))
              ;; ((string-match-p (image-file-name-regexp) thumbnail-file))
              ( (file-exists-p thumbnail-file)))
        (let* ((width (or (org-display-inline-image--width link) 300))
               (align (org-image--align link))
               (image (create-image thumbnail-file nil nil :width width :ascent 100)))
          (if image                        ; Add image to overlay
              (org-link-beautify-overlay-display-image ov image align)
            nil)))))

;;; display file attributes bellow link element.

(defun org-link-beautify-file-attributes (ov path link)
  "Display file attributes on overlay OV from PATH bellow element LINK."
  (when-let* ((link-type (org-element-property :type link))
              (file-attributes (file-attributes (if (equal link-type "attachment") (org-attach-expand path) path)))
              (file-size (file-size-human-readable (file-attribute-size file-attributes))))
    (overlay-put ov 'after-string (concat
                                   (propertize "(" 'face '(:foreground "SlateGray3" :height 0.7))
                                   (propertize file-size 'face '(:foreground "gray" :height 0.7))
                                   (propertize ")" 'face '(:foreground "SlateGray3" :height 0.7))))
    (overlay-put ov 'face 'font-lock-comment-face)))

;;; Preview file: link type

(defun org-link-beautify-preview-file (ov path link)
  "Preview file of PATH over OV overlay position for LINK element.
This function will apply file type function based on file extension."
  (if-let* ((extension (file-name-extension path)))
      (cond
       ;; no file extension, it's directory.
       ((null extension)
        (org-link-beautify-iconify ov path link))
       ;; Images in `org-link-beautify-image-preview-list'
       ((string-match-p (image-file-name-regexp) path)
        (org-link-beautify-preview-file-image ov path link))
       ;; PDF
       ((string-equal extension "pdf")
        (and (org-link-beautify-preview-file-pdf ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; EPUB
       ((string-equal extension "epub")
        (and (org-link-beautify-preview-file-epub ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; Kindle .mobi or .azw3
       ((string-match-p "\\(mobi\\|azw3\\|azw\\)" extension)
        (and (org-link-beautify-preview-file-kindle ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; .fb2(.zip)
       ((string-match-p "\\.fb2\\(\\.zip\\)?" path)
        (and (org-link-beautify-preview-file-fictionbook2 ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; .djvu
       ((string-match-p "\\.djvu" path)
        (and (org-link-beautify-preview-file-djvu ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; Comic
       ((member extension org-link-beautify-comic-preview-list)
        (and (org-link-beautify-preview-file-comic ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; Video
       ((member extension org-link-beautify-video-preview-list)
        (and (org-link-beautify-preview-file-video ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; Audio
       ((member extension org-link-beautify-audio-preview-list)
        (and (org-link-beautify-preview-file-audio ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; Subtitle
       ((member extension org-link-beautify-subtitle-preview-list)
        (org-link-beautify-preview-file-subtitle ov path link))
       ;; Archive
       ((member extension org-link-beautify-archive-preview-list)
        (and (org-link-beautify-preview-file-archive ov path link)
             (org-link-beautify-file-attributes ov path link)))
       ;; Source Code
       ((member extension org-link-beautify-source-code-preview-list)
        (org-link-beautify-preview-file-source-code ov path link))
       ;; file:/path/to/file.html (single HTML file for offline archived webpage)
       ((member extension org-link-beautify-offline-webpage-preview-list)
        (and (org-link-beautify-preview-file-offline-webpage ov path link)
             (org-link-beautify-file-attributes ov path link)))
       (t (or (org-link-beautify-preview-thumbnail ov path link)
              (and (org-link-beautify-iconify ov path link)
                   (org-link-beautify-file-attributes ov path link)))))))

;;; Preview attachment: link type

(declare-function org-attach-expand "org-attach" (file))

(defun org-link-beautify-preview-attachment (ov path link)
  "Preview attachment file of PATH over OV overlay position for LINK element.
This function will apply file type function based on file extension."
  (org-with-point-at (org-element-begin link)
    (and (org-link-beautify-preview-file ov (org-attach-expand path) link)
         (org-link-beautify-file-attributes ov path link))))

;;; file: [image]

(defcustom org-link-beautify-image-preview-list image-file-name-extensions
  "A list of image file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defun org-link-beautify-preview-file-image (ov path link)
  "Preview image file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (file (expand-file-name path))
            ;; ((string-match-p (image-file-name-regexp) file))
            ( (file-exists-p file)))
      (let* ((align (org-image--align link))
             (width (or (org-display-inline-image--width link)
                        (if (listp org-image-actual-width)
                            (car org-image-actual-width)
                          org-image-actual-width)
                        600))
             (image (create-image file nil nil :width width :ascent 100)))
        (if image                     ; Add image to overlay
	          ;; See bug#59902. We cannot rely on Emacs to update image if the file has changed.
            (org-link-beautify-overlay-display-image ov image align)
          nil))))

;;; file: .pdf

(defcustom org-link-beautify-pdf-preview-command
  (cond
   ((executable-find "pdftocairo") "pdftocairo")
   ((executable-find "pdf2svg") "pdf2svg"))
  "The command used to preview PDF file cover."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-pdf-preview-size 300
  "The PDF preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-pdf-preview-default-page-number 1
  "The default PDF preview page number."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-pdf-preview-image-format 'png
  "The format of PDF file preview image."
  :type '(choice
          :tag "The format of PDF file preview image."
          (const :tag "PNG" png)
          (const :tag "JPEG" jpeg)
          (const :tag "SVG" svg))
  :safe #'symbolp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-pdf (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .pdf file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (pdf-page-number (if search-option
                                (string-to-number
                                 (cond
                                  ((string-prefix-p "P" search-option) ; "P42"
                                   (substring search-option 1 nil))
                                  ((string-match "\\([[:digit:]]+\\)\\+\\+\\(.*\\)" search-option) ; "40++0.00"
                                   (match-string 1 search-option))
                                  (t search-option)))
                              (if-let* ((search-option (match-string 2 path)))
                                  (string-to-number
                                   (cond
                                    ((string-prefix-p "P" search-option) ; "P42"
                                     (substring search-option 1 nil))
                                    ((string-match "\\([[:digit:]]+\\)\\+\\+\\(.*\\)" search-option) ; "40++0.00"
                                     (match-string 1 search-option))
                                    (t search-option)))
                                org-link-beautify-pdf-preview-default-page-number)))
           (pdf-file (expand-file-name (org-link-unescape file-path)))
           (pdf-file-name (file-name-nondirectory pdf-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path pdf-file))
           (thumbnail-file (expand-file-name
                            (if (= pdf-page-number 1) ; if have page number ::N specified.
                                (format "%s%s.%s"
                                        thumbnails-dir (file-name-base pdf-file)
                                        (symbol-name org-link-beautify-pdf-preview-image-format))
                              (format "%s%s-P%s.%s"
                                      thumbnails-dir (file-name-base pdf-file) pdf-page-number
                                      (symbol-name org-link-beautify-pdf-preview-image-format)))))
           (thumbnail-size 600)
           (proc-name (format "org-link-beautify pdf preview - %s" pdf-file-name))
           (proc-buffer (format " *org-link-beautify pdf preview - %s*" pdf-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (pcase (file-name-nondirectory org-link-beautify-pdf-preview-command)
          ("pdftocairo"
           (cl-assert (executable-find "pdftocairo") nil
                      "[org-link-beautify] Please install the PDF preview command `pdftocairo'")
           (start-process
            proc-name proc-buffer
            "pdftocairo"
            (pcase org-link-beautify-pdf-preview-image-format
              ('png "-png")
              ('jpeg "-jpeg")
              ('svg "-svg"))
            "-singlefile"
            "-f" (number-to-string pdf-page-number)
            pdf-file (file-name-sans-extension thumbnail-file)))
          ("pdf2svg"
           (cl-assert (executable-find "pdf2svg") nil
                      "[org-link-beautify] Please install the PDF preview command `pdf2svg'")
           (unless (eq org-link-beautify-pdf-preview-image-format 'svg)
             (warn "The pdf2svg only supports convert PDF to SVG format.
Please adjust `org-link-beautify-pdf-preview-command' to `pdftocairo' or
Set `org-link-beautify-pdf-preview-image-format' to `svg'."))
           (start-process
            proc-name proc-buffer
            "pdf2svg" pdf-file thumbnail-file (number-to-string pdf-page-number))))
        (org-link-beautify--preview-process-setup-async-timeout thumbnail-file proc-buffer 15))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed path thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-pdf (ov path link)
  "Preview pdf file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-pdf-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-pdf path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; file: .epub

(defcustom org-link-beautify-epub-preview-command
  (or (expand-file-name "scripts/thumbnailer-epub.py" (file-name-directory (or load-file-name (buffer-file-name))))
      (expand-file-name "scripts/thumbnailer.py" (file-name-directory (or load-file-name (buffer-file-name)))))
  "Whether enable EPUB files cover preview?
You can set this option to nil to disable EPUB preview.

This script require Python package 'Pillow' installed.
$ pip install Pillow"
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-ebook-preview-size 600
  "The EPUB cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-epub (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .epub file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           ;; TODO: currently epub file page number thumbnail is not supported by `org-link-beautify-epub-preview-command'.
           (epub-page-number (if search-option (string-to-number search-option) 1))
           (epub-file (expand-file-name (org-link-unescape file-path)))
           (epub-file-name (file-name-nondirectory epub-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path epub-file))
           (thumbnail-file (expand-file-name
                            (if (or (null epub-page-number) (= epub-page-number 1)) ; if have page number ::N specified.
                                (format "%s%s.png" thumbnails-dir (file-name-base epub-file))
                              (format "%s%s-P%s.png" thumbnails-dir (file-name-base epub-file) epub-page-number))))
           (thumbnail-size (or org-link-beautify-ebook-preview-size 600))
           (proc-name (format "org-link-beautify epub preview - %s" epub-file-name))
           (proc-buffer (format " *org-link-beautify epub preview - %s*" epub-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (pcase (file-name-nondirectory org-link-beautify-epub-preview-command)
          ("thumbnailer-epub.py"           ; for script "scripts/epub-thumbnailer.py"
           (cl-assert (file-exists-p org-link-beautify-epub-preview-command) nil
                      "[org-link-beautify] EPUB preview command not available.\nCheck package installed scripts.")
           (make-process
            :name proc-name
            :command (list org-link-beautify-python-interpreter
                           org-link-beautify-epub-preview-command
                           epub-file
                           thumbnail-file
                           (number-to-string thumbnail-size))
            :buffer proc-buffer
            :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
            :sentinel (lambda (proc event)
                        (when org-link-beautify-enable-debug-p
                          (message "[org-link-beautify] > proc: %s\n> event: %s" proc event)))))
          ("thumbnailer.py"
           (cl-assert (file-exists-p org-link-beautify-epub-preview-command) nil
                      "[org-link-beautify] EPUB preview command not available.\nCheck package installed scripts.")
           (org-link-beautify-thumbnailer file-path proc-name proc-buffer))
          (_ (user-error "This system platform currently not supported by org-link-beautify.\n Please contribute code to support")))
        (org-link-beautify--preview-process-setup-async-timeout thumbnail-file proc-buffer 20))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed epub-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-epub (ov path link)
  "Preview epub file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-epub-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-epub path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; file: .mobi, .azw3

(defcustom org-link-beautify-kindle-preview-command
  (cl-case system-type
    (gnu/linux (executable-find "mobitool"))
    (darwin (executable-find "mobitool")))
  "Whether enable Kindle ebook files cover preview?

Enable Kindle ebook preview by default.

You can set this option to nil to disable EPUB preview.

You can install software `libmobi' to get command `mobitool'."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-kindle-preview-size 300
  "The Kindle cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-kindle (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .mobi & .azw3 file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (kindle-page-number (or (match-string 2 path) 1))
           (kindle-file (expand-file-name (org-link-unescape file-path)))
           (kindle-file-name (file-name-nondirectory kindle-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path kindle-file))
           (thumbnail-file (expand-file-name
                            (if (or (null kindle-page-number) (= kindle-page-number 1)) ; if have page number ::N specified.
                                (format "%s%s.jpg" thumbnails-dir (file-name-base kindle-file))
                              (format "%s%s-P%s.jpg" thumbnails-dir (file-name-base kindle-file) kindle-page-number))))
           (thumbnail-size (or org-link-beautify-ebook-preview-size 600))
           (proc-name (format "org-link-beautify kindle preview - %s" kindle-file-name))
           (proc-buffer (format " *org-link-beautify kindle preview - %s*" kindle-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (pcase (file-name-nondirectory org-link-beautify-kindle-preview-command)
          ("mobitool"
           (cl-assert (executable-find "mobitool") nil "[org-link-beautify] Please install `mobitool' command tool")
           ;; mobitool dumped cover image thumbnail filename can't be specified in command-line argument.
           (let ((mobitool-cover-file (concat thumbnails-dir (file-name-base kindle-file) "_cover.jpg")))
             (unless (file-exists-p mobitool-cover-file)
               (start-process
                proc-name proc-buffer
                "mobitool" "-c" "-o" thumbnails-dir kindle-file))
             ;; then rename [file.extension.jpg] to [file.jpg]
             (when (file-exists-p mobitool-cover-file)
               (rename-file mobitool-cover-file thumbnail-file))))
          (_ (user-error "[org-link-beautify] Error: Can't find command tool to dump kindle ebook file cover")))
        (org-link-beautify--preview-process-setup-async-timeout thumbnail-file proc-buffer 20))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed kindle-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-kindle (ov path link)
  "Preview kindle .mobi or .azw3 file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-kindle-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-kindle path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; FictionBook2 (.fb2, .fb2.zip) file cover preview

(defcustom org-link-beautify-fictionbook2-preview (featurep 'fb2-reader)
  "Whether enable FictionBook2 ebook files covert preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-fictionbook2-preview-size 300
  "The FictionBook2 cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

;; (require 'fb2-reader)
(declare-function fb2-reader--create-image "fb2-reader" (data type &rest props))
(declare-function fb2-reader--extract-image-data "fb2-reader" (book attributes &optional tags))
(declare-function fb2-reader--get-cover "fb2-reader" (book))
(declare-function fb2-reader-parse-file-as-html "fb2-reader" (file))
(declare-function fb2-reader-parse-file-as-xml "fb2-reader" (file))

(defun org-link-beautify-fictionbook2--extract-cover (file-path)
  "Extract cover image data for FictionBook2 at FILE-PATH."
  (if-let* (;; reference `fb2-reader-mode'
            (book (or (fb2-reader-parse-file-as-xml file-path)
                      (fb2-reader-parse-file-as-html file-path)))
            ;; reference `fb2-reader-splash-screen'
            (cover-item (fb2-reader--get-cover book))
            ;; reference `fb2-reader-splash-cover': (fb2-reader-splash-cover book cover-item)
            (attrs (cl-second (cl-third cover-item)))
            (img-data (fb2-reader--extract-image-data book attrs))
            (type (cl-first img-data))
            (data (cl-second img-data))
            ;; reference `fb2-reader--insert-image': (fb2-reader--insert-image data-str type-str nil t)
            (type-symbol (alist-get type '(("image/jpeg" . jpeg) ("image/png" . png)) nil nil 'equal))
            (data-decoded (base64-decode-string data))
            (img-raw (fb2-reader--create-image data-decoded type-symbol))
            (image (create-image data-decoded type-symbol 't :ascent 100)))
      image
    'no-cover))

(defun org-link-beautify-fictionbook2--save-cover (image thumbnail-file)
  "Save FictionBook2 cover IMAGE to THUMBNAIL-FILE."
  ;; `image-save': This writes the original image data to a file.
  (cl-assert (featurep 'fb-reader) nil "[org-link-beautify] Please install Emacs package `fb-reader'")
  (with-temp-buffer
    (insert (plist-get (cdr image) :data))
    (write-region (point-min) (point-max) thumbnail-file)))

(defun org-link-beautify--generate-preview-for-file-fictionbook2 (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .fb2 & .fb2.zip file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (fictionbook2-file (expand-file-name (org-link-unescape file-path)))
           (fictionbook2-file-name (file-name-nondirectory fictionbook2-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path fictionbook2-file))
           (thumbnail-file (cond
                            ((string-match-p "\\.fb2.zip$" path)
                             (expand-file-name
                              (format "%s%s.png" thumbnails-dir (string-replace ".fb2" "" (file-name-base fictionbook2-file)))))
                            ((string-match-p "\\.fb2$" path)
                             (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base fictionbook2-file))))))
           (thumbnail-size (or org-link-beautify-fictionbook2-preview-size 600))
           (proc-name (format "org-link-beautify fictionbook preview - %s" fictionbook2-file-name))
           (proc-buffer (format " *org-link-beautify fictionbook preview - %s*" fictionbook2-file-name)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (let ((cover-image (org-link-beautify-fictionbook2--extract-cover fictionbook2-file)))
          (if (eq cover-image 'no-cover)
              (message "[org-link-beautify] FictionBook2 preview failed to extract cover image")
            (org-link-beautify-fictionbook2--save-cover cover-image thumbnail-file))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed fictionbook2-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-fictionbook2 (ov path link)
  "Preview FictionBook2 .fb2, .fb2.zip file of PATH over OV overlay position for LINK element."
  (if-let* ((org-link-beautify-fictionbook2-preview)
            ( (display-graphic-p))
            (thumbnail-file (org-link-beautify--generate-preview-for-file-fictionbook2 path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; DjVu (.djvu) file cover preview

(defcustom org-link-beautify-djvu-preview nil
  "Whether enable DjVu ebook files covert preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(autoload 'djvu-view-page "djvu")
(declare-function djvu-view-page "djvu" (page &optional doc new))

(defun org-link-beautify-djvu--extract-cover  (file-path)
  "Extract cover image data for DjVu file at FILE-PATH."
  ;; TODO: generate thumbnail for .djvu file.
  ;; (if-let* (; reference `djvu' library
  ;;           ;; (cover-image (djvu-view-page ))
  ;;           )
  ;;     )
  )

(defun org-link-beautify-djvu--save-cover (image thumbnail-file)
  "Save DjVu cover IMAGE to THUMBNAIL-FILE."
  ;; `image-save': This writes the original image data to a file.
  (with-temp-buffer
    (insert (plist-get (cdr image) :data))
    (write-region (point-min) (point-max) thumbnail-file)))

(defun org-link-beautify--generate-preview-for-file-djvu (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .djvu file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (djvu-file (expand-file-name (org-link-unescape file-path)))
           (djvu-file-name (file-name-nondirectory djvu-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path djvu-file))
           (thumbnail-file (cond
                            ((string-match-p "\\.fb2.zip$" path)
                             (expand-file-name
                              (format "%s%s.png" thumbnails-dir (string-replace ".fb2" "" (file-name-base djvu-file)))))
                            ((string-match-p "\\.fb2$" path)
                             (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base djvu-file))))))
           (thumbnail-size 600)
           (proc-name (format "org-link-beautify djvu preview - %s" djvu-file-name))
           (proc-buffer (format " *org-link-beautify djvu preview - %s*" djvu-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (let ((cover-image (org-link-beautify-djvu--extract-cover djvu-file)))
          (if (eq cover-image 'no-cover)
              (message "[org-link-beautify] DjVu preview failed to extract cover image")
            (org-link-beautify-djvu--save-cover cover-image thumbnail-file))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed djvu-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-djvu (ov path link)
  "Preview DjVu .djvu file of PATH over OV overlay position for LINK element."
  (if-let* ((org-link-beautify-djvu-preview)
            ( (display-graphic-p))
            (thumbnail-file (org-link-beautify--generate-preview-for-file-djvu path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; Source Code File

(defcustom org-link-beautify-source-code-preview-command
  (cond
   ((executable-find "silicon") "silicon")
   ((executable-find "germanium") "germanium"))
  "The command used to preview source code file."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-source-code-preview-list
  '(; "txt" "org" "markdown" "md"
    "lisp" "scm" "clj" "cljs"
    "py" "rb" "pl"
    "c" "cpp" "h" "hpp" "cs" "java"
    "js" "css"
    "R" "jl")
  "A list of link types supports source code file preview below the link."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-source-code-preview-max-lines 30
  "The maximum lines number of file for previewing."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--preview-source-code-file (file)
  "Return first 10 lines of FILE."
  (with-temp-buffer
    (condition-case nil
        (progn
          ;; I originally use `insert-file-contents-literally', so Emacs doesn't
          ;; decode the non-ASCII characters it reads from the file, i.e. it
          ;; doesn't interpret the byte sequences as Chinese characters. Use
          ;; `insert-file-contents' instead. In addition, this function decodes
          ;; the inserted text from known formats by calling format-decode,
          ;; which see.
          (insert-file-contents file)
          (org-link-beautify--display-content-block
           ;; This `cl-loop' extract a LIST of string lines from the file content.
           (cl-loop repeat 10
                    unless (eobp)
                    collect (prog1 (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))
                              (forward-line 1)))))
      (file-error
       (message "Unable to read file %S" file)
	   nil))))

(defun org-link-beautify--generate-preview-for-file-source-code (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for source code file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (source-code-file (expand-file-name (org-link-unescape file-path)))
           (source-code-file-name (file-name-nondirectory source-code-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path source-code-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base source-code-file))))
           (thumbnail-size 600)
           (proc-name (format "org-link-beautify code preview - %s" source-code-file-name))
           (proc-buffer (format " *org-link-beautify code preview - %s*" source-code-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (and (file-exists-p thumbnail-file)
                   ;; limit to maximum 30 lines of file.
                   (> (string-to-number (shell-command-to-string (format "cat %s | wc -l" source-code-file)))
                      org-link-beautify-source-code-preview-max-lines))
        (unless (or proc (get-buffer proc-buffer))
          (pcase (file-name-nondirectory org-link-beautify-source-code-preview-command)
            ("silicon"
             (cl-assert (executable-find "silicon") nil "[org-link-beautify] Please install command tool `silicon'")
             (start-process
              proc-name proc-buffer
              "silicon" source-code-file "-o" thumbnail-file
              "--theme" "Dracula"
              "--no-window-controls" "--shadow-blur-radius" "30" "--shadow-color" "#555"
              "--window-title" (file-name-nondirectory file-path)))
            ("germanium"
             (cl-assert (executable-find "germanium") nil "[org-link-beautify] Please install command tool `germanium'")
             (start-process
              proc-name proc-buffer
              "germanium" source-code-file "-o" thumbnail-file
              "--no-line-number" "--no-window-access-bar")))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed fictionbook2-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-source-code (ov path link)
  "Preview source code file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-source-code-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-source-code path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link))
                               (if (> image-height image-width) 300 600))))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (if-let* ((source-code (org-link-beautify--preview-source-code-file path)))
        (prog1 ov
          (overlay-put ov 'after-string source-code)
	        (overlay-put ov 'face    'org-block))
      (org-link-beautify-iconify ov path link))))

;;; file: [comic]

(defcustom org-link-beautify-comic-preview-command
  (cl-case system-type
    (darwin (or (executable-find "qlmanage") org-link-beautify-thumbnailer-script))
    (gnu/linux org-link-beautify-thumbnailer-script))
  "Whether enable CDisplay Archived Comic Book Formats cover preview.
File extensions like (.cbr, .cbz, .cb7, .cba, .cbt etc)."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-comic-preview-list
  '("cbr" "cbz" "cb7" "cba" "cbt")
  "A list of comic file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-comic-preview-size 600
  "The CDisplay Archived Comic Book Formats cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-comic (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for CDisplay Archived Comic Book: .cbz, .cbr etc file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (comic-file (expand-file-name (org-link-unescape file-path)))
           (comic-file-name (file-name-nondirectory comic-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path comic-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base comic-file))))
           (thumbnail-size (or org-link-beautify-comic-preview-size 1080))
           (proc-name (format "org-link-beautify comic preview - %s" comic-file-name))
           (proc-buffer (format " *org-link-beautify comic preview - %s*" comic-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (unless (or proc (get-buffer proc-buffer))
          (cl-case system-type
            (gnu/linux
             (pcase (file-name-nondirectory org-link-beautify-comic-preview-command)
               ("cbconvert"              ; https://github.com/gen2brain/cbconvert
                (cl-assert (executable-find "cbconvert") nil "[org-link-beautify] Please install command tool `cbconvert'")
                (start-process
                 proc-name
                 proc-buffer
                 org-link-beautify-comic-preview-command
                 "cover" comic-file "--output" thumbnails-dir
                 (if org-link-beautify-comic-preview-size
                     "--width")
                 (if org-link-beautify-comic-preview-size
                     (number-to-string thumbnail-size))))
               ("thumbnailer.py" (org-link-beautify-thumbnailer file-path proc-name proc-buffer))))
            (darwin
             ;; for macOS "qlmanage" command
             ;; $ qlmanage -t "ラセン恐怖閣-マリコとニジロー1-DL版.cbz" - 2.0 -s 1080 -o ".thumbnails"
             (pcase (file-name-nondirectory org-link-beautify-comic-preview-command)
               ("qlmanage"
                (cl-assert (executable-find "qlmanage") nil "[org-link-beautify] Please ensure command tool `qlmanage' on macOS")
                (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-base comic-file) ".png")))
                  (make-process
                   :name proc-name
                   :command (list org-link-beautify-comic-preview-command
                                  "-t"
                                  comic-file
                                  "-o" thumbnails-dir
                                  "-s" (number-to-string thumbnail-size))
                   :buffer proc-buffer
                   :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
                   :sentinel (lambda (proc event)
                               (when org-link-beautify-enable-debug-p
                                 (message "[org-link-beautify] proc: %s \n> event: %s" proc event))))
                  ;; then rename [file.extension.png] to [file.png]
                  (when (file-exists-p qlmanage-thumbnail-file)
                    (rename-file qlmanage-thumbnail-file thumbnail-file))))
               ("thumbnailer.py"
                (cl-assert (file-exists-p org-link-beautify-comic-preview-command) nil
                           "[org-link-beautify] Please ensure package installed script %s"
                           org-link-beautify-comic-preview-command)
                (org-link-beautify-thumbnailer file-path proc-name proc-buffer))))
            (t (user-error "This system platform currently not supported by org-link-beautify.\n Please contribute code to support"))))
        (org-link-beautify--preview-process-setup-async-timeout thumbnail-file proc-buffer 20))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed comic-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-comic (ov path link)
  "Preview comic .cbz or .cbr file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-comic-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-comic path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; file: [video]

(defvar org-link-beautify-video-thumbnailer-script
  (expand-file-name "scripts/thumbnailer-video.py" (file-name-directory (or load-file-name (buffer-file-name))))
  "The path of video thumbnailer script.
This script requires:
- FFmpeg
- Python package 'ffmpeg-python'
$ pip install ffmpeg-python")

(defcustom org-link-beautify-video-preview-command
  (cl-case system-type
    ;; for macOS, use `qlmanage' on priority
    (darwin (or (executable-find "qlmanage") (executable-find "ffmpeg")))
    ;; for Linux, use `ffmpegthumbnailer' on priority
    (gnu/linux (or (executable-find "ffmpegthumbnailer") (executable-find "ffmpeg")))
    ;; for general, use `ffmpeg'
    ;; $ ffmpeg -i video.mp4 -ss 00:01:00.000 -vframes 1 -vcodec png -an -f rawvideo -s 119x64 out.png
    (t (or (executable-find "ffmpeg")
           org-link-beautify-video-thumbnailer-script
           org-link-beautify-thumbnailer-script)))
  "The available command to preview video."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-preview-list
  '("avi" "rmvb" "flv" "ogg" "ogv" "mp4" "m4v" "mkv" "mov" "mpeg" "webm" "flv" "ts" "mpg")
  "A list of video file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-preview-size 600
  "The video thumbnail image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-video (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for video .mp4 etc file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (video-file (expand-file-name (org-link-unescape file-path)))
           (video-file-name (file-name-nondirectory video-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path video-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base video-file))))
           (thumbnail-size (or org-link-beautify-video-preview-size 600))
           (proc-name (format "org-link-beautify video preview - %s" video-file-name))
           (proc-buffer (format " *org-link-beautify video preview - %s*" video-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        ;; detect process already running?
        (unless (or proc (get-buffer proc-buffer))
          (pcase (file-name-nondirectory org-link-beautify-video-preview-command)
            ("ffmpeg"
             (cl-assert (executable-find "ffmpeg") nil "[org-link-beautify] Please install command tool `ffmpeg'")
             ;; $ ffmpeg -i video.mp4 -ss 00:00:00.001 -vframes 1 -vcodec png -an -f rawvideo -s 119x64 out.png
             (let ((thumbnail-width thumbnail-size)
                   (thumbnail-height 300))
               (start-process
                proc-name proc-buffer
                "ffmpeg"
                "-i" video-file
                "-ss" "00:00:00.001"
                "-vframes" "1"
                "-vcodec" "png"
                "-an"
                "-f" "rawvideo"
                ;; "-s" (format "%sx%s" thumbnail-width thumbnail-height)
                "-vf" "scale=600:-1" ; use ":-1" to maintain aspect ratio.
                thumbnail-file)))
            ("qlmanage"
             (cl-assert (executable-find "qlmanage") nil "[org-link-beautify] Please ensure command tool `qlmanage' on macOS")
             (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory video-file) ".png")))
               (unless (file-exists-p qlmanage-thumbnail-file)
                 (let ((proc (start-process
                              proc-name proc-buffer
                              "qlmanage" "-x" "-t" "-s" (number-to-string thumbnail-size) video-file "-o" thumbnails-dir))
                       (proc-filter (lambda (proc output)
                                      ;; * No thumbnail created for [FILE PATH]
                                      (when (string-match "\\* No thumbnail created for.*" output)
                                        (message "[org-link-beautify] video preview FAILED on macOS QuickLook generating thumbnail for video %s" video-file-name)))))
                   (set-process-filter proc proc-filter)))
               ;; then rename [file.extension.png] to [file.png]
               (when (file-exists-p qlmanage-thumbnail-file)
                 (rename-file qlmanage-thumbnail-file thumbnail-file))))
            ("ffmpegthumbnailer"
             (cl-assert (executable-find "ffmpegthumbnailer") nil "[org-link-beautify] Please install command tool `ffmpegthumbnailer'")
             (start-process
              proc-name proc-buffer
              "ffmpegthumbnailer" "-f" "-i" video-file "-s" (number-to-string thumbnail-size) "-o" thumbnail-file))
            ("thumbnailer-video.py"
             (cl-assert (file-exists-p org-link-beautify-video-preview-command) nil
                        "[org-link-beautify] Please ensure package installed script %s"
                        org-link-beautify-video-preview-command)
             (make-process
              :name proc-name
              :command (list org-link-beautify-video-thumbnailer-script
                             input-file
                             thumbnail-file
                             (number-to-string thumbnail-size))
              :buffer proc-buffer
              :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
              :sentinel (lambda (proc event)
                          (when org-link-beautify-enable-debug-p
                            (message "[org-link-beautify] > proc: %s\n> event: %s" proc event)))))))
        (org-link-beautify--preview-process-setup-async-timeout thumbnail-file proc-buffer 20))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed video-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-video (ov path link)
  "Preview video file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-video-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-video path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link))
                               (if (< (abs (- image-height image-width)) 10) 200 600))))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; file: [audio]

(defcustom org-link-beautify-audio-preview-command
  (cond
   ((executable-find "qlmanage") 'qlmanage)
   ((executable-find "audiowaveform") 'audiowaveform)
   ((executable-find "ffmpeg") 'ffmpeg)
   ((executable-find "whisper") 'whisper-transcribe)
   ((executable-find "whisper-cli") 'whisper-cpp-transcribe)
   ((executable-find "whisper-server") 'whisper-cpp-server-transcribe))
  "Find available audio preview command."
  :type 'symbol
  :safe #'symbolp
  :group 'org-link-beautify)

(defcustom org-link-beautify-audio-preview-list
  '("mp3" "wav" "flac" "ogg" "m4a" "opus" "dat"
    ;; Audiobook formats
    "m4b")
  "A list of audio file types be supported generating audio wave form image."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-audio-preview-size 600
  "The audio wave form image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-audio (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for audio .mp3 etc file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (audio-file (expand-file-name (org-link-unescape file-path)))
           (audio-file-name (file-name-nondirectory audio-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path audio-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base audio-file))))
           (thumbnail-size (or org-link-beautify-audio-preview-size 300))
           (proc-name (format "org-link-beautify audio preview - %s" audio-file-name))
           (proc-buffer (format " *org-link-beautify audio preview - %s*" audio-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (unless (or proc (get-buffer proc-buffer))
          (cl-case org-link-beautify-audio-preview-command
            (ffmpeg
             (cl-assert (executable-find "ffmpeg") nil "[org-link-beautify] Please install command tool `ffmpeg'")
             (start-process
              proc-name proc-buffer
              "ffmpeg" "-i" audio-file
              "-filter_complex" "[0:a]aformat=channel_layouts=mono,compand=gain=-6,showwavespic=s=600x120:colors=#9cf42f[fg];color=s=600x120:color=#44582c,drawgrid=width=iw/10:height=ih/5:color=#9cf42f@0.1[bg];[bg][fg]overlay=format=auto,drawbox=x=(iw-w)/2:y=(ih-h)/2:w=iw:h=1:color=#9cf42f"
              "-frames:v" "1"
              thumbnail-file))
            (qlmanage
             (cl-assert (executable-find "qlmanage") nil "[org-link-beautify] Please ensure command tool `qlmanage' on macOS")
             (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory audio-file) ".png")))
               (unless (file-exists-p qlmanage-thumbnail-file)
                 (start-process
                  proc-name proc-buffer
                  "qlmanage" "-x" "-t" "-s" (number-to-string thumbnail-size) audio-file "-o" thumbnails-dir))
               ;; then rename [file.extension.png] to [file.png]
               (when (file-exists-p qlmanage-thumbnail-file)
                 (rename-file qlmanage-thumbnail-file thumbnail-file))))
            (audiowaveform
             (cl-assert (executable-find "audiowaveform") nil "[org-link-beautify] Please install command tool `audiowaveform'")
             (start-process
              proc-name proc-buffer
              "audiowaveform" "-i" audio-file "-o" thumbnail-file))
            (whisper-transcribe
             (cl-assert (executable-find "whisper") nil "[org-link-beautify] Please install command tool `whisper'")
             (start-process
              proc-name proc-buffer
              "whisper" "--model" "turbo" "--output_format" "vtt" "--task" "transcribe" audio-file) )
            (whisper-cpp-transcribe
             (cl-assert (executable-find "whisper-cli") nil "[org-link-beautify] Please install command tool `whisper-cli'")
             (start-process
              proc-name proc-buffer
              "whisper-cli" "--model" "~/.config/whisper-cpp/models/ggml-large-v3-turbo.bin" "-f" audio-file "--output-file" output-file)))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed audio-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-audio (ov path link)
  "Preview audio file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-audio-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-audio path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link))
                               (if (> image-height image-width) 300 600))))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; file: [subtitle]

(defcustom org-link-beautify-subtitle-preview-command org-link-beautify-thumbnailer-script
  "The command to preview subtitle file."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

;; https://en.wikipedia.org/wiki/Subtitles
(defcustom org-link-beautify-subtitle-preview-list
  '("ass" "srt" "sub" "vtt" "ssf")
  "A list of subtitle file types support previewing."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-subtitle-preview-size 600
  "The subtitle preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-subtitle (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for subtitle file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (subtitle-file (expand-file-name (org-link-unescape file-path)))
           (subtitle-file-name (file-name-nondirectory subtitle-file)))
      (if-let* ((org-link-beautify-subtitle-preview-command)
                (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path subtitle-file))
                (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base subtitle-file))))
                (thumbnail-size (or org-link-beautify-subtitle-preview-size 200))
                (proc-name (format "org-link-beautify subtitle preview - %s" subtitle-file-name))
                (proc-buffer (format " *org-link-beautify subtile preview - %s*" subtitle-file-name)))
          (prog1 thumbnail-file ; return the thumbnail file as result.
            (unless (file-exists-p thumbnail-file)
              (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
              (pcase (file-name-nondirectory org-link-beautify-subtitle-preview-command)
                ("thumbnailer.py"
                 (cl-assert (file-exists-p org-link-beautify-subtitle-preview-command) nil
                            "[org-link-beautify] Please ensure package installed script %s"
                            org-link-beautify-subtitle-preview-command)
                 (org-link-beautify-thumbnailer file-path proc-name proc-buffer)))))
        (let* ((subtitle-file-context (split-string (shell-command-to-string (format "head -n 20 '%s'" subtitle-file)) "\n"))
               (text (concat "\n" (org-link-beautify--display-content-block subtitle-file-context))))
          ;; return the subtitle file context as result.
          text)))))

(defun org-link-beautify-preview-file-subtitle (ov path link)
  "Preview subtitle file of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-subtitle-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-subtitle path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (if-let* ((text (org-link-beautify--generate-preview-for-file-subtitle path)))
        (prog1 ov
          (overlay-put ov 'after-string text)
	        (overlay-put ov 'face         'default))
      (org-link-beautify-iconify ov path link))))

;;; file: [archive file]

(defcustom org-link-beautify-archive-preview-list
  '("zip" "rar" "7z" "gz" "tar" "tar.gz" "tar.bz2" "xz" "zst")
  "A list of archive file types support previewing."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-archive-preview-command-alist
  '(("zip" . "unzip -l")
    ("rar" . "unrar l")
    ("7z" . "7z l -ba") ; -ba - suppress headers; undocumented.
    ("gz" . "gzip --list")
    ;; ("bz2" . "")
    ("tar" . "tar --list")
    ("tar.gz" . "tar --gzip --list")
    ("tar.bz2" . "tar --bzip2 --list")
    ("xz" . "xz --list")
    ("zst" . "zstd --list"))
  "An alist of archive types supported archive preview inside files list.
Each element has form (ARCHIVE-FILE-EXTENSION COMMAND)."
  :type '(alist :value-type (group string))
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-archive (path)
  "Get the files list preview of archive file at PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (archive-file (expand-file-name (org-link-unescape file-path)))
           (archive-extension (file-name-extension archive-file))
           (extract-command (cdr (assoc archive-extension org-link-beautify-archive-preview-command-alist)))
           (command-name (car (string-split extract-command " ")))
           (execute-command (format "%s '%s'" extract-command archive-file))
           (archive-files-list (progn
                                 (cl-assert (executable-find command-name) nil
                                            "[org-link-beautify] Please install command tool %s" command-name)
                                 (split-string (shell-command-to-string execute-command) "\n")))
           (text (concat "\n" (org-link-beautify--display-content-block archive-files-list))))
      ;; return the files list in archive file as result.
      text)))

(defun org-link-beautify-preview-file-archive (ov path link)
  "Preview archive file link of PATH over OV overlay position for LINK element."
  (if-let* ((text (org-link-beautify--generate-preview-for-file-archive path)))
      (prog1 ov
        (overlay-put ov 'after-string text)
	    (overlay-put ov 'face         'default)
	    (overlay-put ov 'keymap       org-link-beautify-keymap))
    (org-link-beautify-iconify ov path link)))

;;; file: [offline webpage archived single HTML file]

(defcustom org-link-beautify-offline-webpage-preview-command
  (cl-case system-type
    (darwin
     (cond
      ((file-exists-p "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
       "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")))
    (gnu/linux
     (cond
      ((or (executable-find "chrome") (executable-find "google-chrome"))
       (executable-find "google-chrome")))))
  "The command to preview offline webpage file."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-offline-webpage-preview-list
  '("html" "mhtml" "mht" "webarchive")
  "A list of offline webpage archive file types support previewing."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-offline-webpage-viewport-size "1920,928"
  "The webpage offline saved archive file preview screenshot image viewport width."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-offline-webpage (path)
  "Generate screenshot for the webpage offline saved archive file at PATH on OV overlay at LINK element."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           ;; (search-option (match-string 2 path))
           (offline-webpage-file (expand-file-name (org-link-unescape file-path)))
           (offline-webpage-file-name (file-name-nondirectory offline-webpage-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path offline-webpage-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base offline-webpage-file))))
           (thumbnail-size (or (string-to-number (car (string-split org-link-beautify-offline-webpage-viewport-size ","))) 600))
           (proc-name (format "org-link-beautify offline webpage preview - %s" offline-webpage-file-name))
           (proc-buffer (format " *org-link-beautify offline webpage preview - %s*" offline-webpage-file-name))
           (proc (get-buffer-process (get-buffer proc-buffer))))
      (prog1 thumbnail-file ; return the thumbnail file as result.
        (unless (file-exists-p thumbnail-file)
          (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
          (unless (or proc (get-buffer proc-buffer))
            (pcase org-link-beautify-offline-webpage-preview-command
              ;; Google Chrome headless screenshot
              ((or "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" "chrome" "google-chrome")
               (cl-assert (executable-find org-link-beautify-offline-webpage-preview-command) nil
                          "[org-link-beautify] Please install Google Chrome")
               ;; $ google-chrome --headless --screenshot=screenshot.png /path/to/file.html --window-size=width,height --hide-scrollbars
               (start-process
                proc-name proc-buffer
                org-link-beautify-offline-webpage-preview-command
                "--headless"
                (format "--screenshot=%s" thumbnail-file)
                ;; web browser viewport: width,height: "1920,928"
                (format "--window-size=%s" org-link-beautify-offline-webpage-viewport-size)
                "--hide-scrollbars"
                offline-webpage-file))))
          (org-link-beautify--preview-process-setup-async-timeout thumbnail-file proc-buffer 20))
        (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
          (org-link-beautify--notify-generate-thumbnail-failed offline-webpage-file thumbnail-file))))))

(defun org-link-beautify-preview-file-offline-webpage (ov path link)
  "Preview offline webpage archived single file."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-offline-webpage-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-file-offline-webpage path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 800)))
      (progn
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; pdf: & docview: link type

(defun org-link-beautify-preview-pdf (ov path link)
  "Preview pdf: link of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (thumbnail-file (org-link-beautify--generate-preview-for-file-pdf path))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 300)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; epub: link type

(defalias 'org-link-beautify-preview-epub 'org-link-beautify-preview-file-epub
  "Preview epub: link of PATH over OV overlay position for LINK element.")

;;; nov: link type

(defalias 'org-link-beautify-preview-nov 'org-link-beautify-preview-epub
  "Preview nov: link of PATH over OV overlay position for LINK element.")

;;; video: link type

(defalias 'org-link-beautify-preview-video 'org-link-beautify-preview-file-video
  "Preview video: link of PATH over OV overlay position for LINK element.")

;;; audio: link type

(defalias 'org-link-beautify-preview-audio 'org-link-beautify-preview-file-audio
  "Preview audio: link of PATH over OV overlay position for LINK element.")

;;; org-contact: link type

(defcustom org-link-beautify-org-contact-avatar-preview-size 64
  "The org-contacts avatar image size :height."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(declare-function org-contacts-get-avatar-icon "org-contacts" (&optional pom))
(declare-function org-contacts-search-contact "org-contacts" (name))

(defun org-link-beautify--generate-preview-for-org-contacts (name)
  "Get the avatar of org-contact in NAME."
  (let* ((epom (org-contacts-search-contact name)))
    (org-contacts-get-avatar-icon epom)))

;; TEST:
;; (org-link-beautify--generate-preview-for-org-contacts "stardiviner")

(defun org-link-beautify-preview-org-contact (ov path link)
  "Preview org-contct: link of PATH over OV overlay position for LINK element."
  (if-let* ((name path)
            ( (display-graphic-p))
            (image (org-link-beautify--generate-preview-for-org-contacts name))
            (display-height org-link-beautify-org-contact-avatar-preview-size))
      ;; display org-contacts avatar image
      (prog1 ov
        (setf (image-property image :height) display-height)
        (overlay-put ov 'display image)
        (overlay-put ov 'after-string (concat
                                       (propertize "{" 'face '(:foreground "purple2"))
                                       (propertize name 'face 'org-verbatim)
                                       (propertize "}" 'face '(:foreground "purple2")))))
    ;; display text-properties with icon
    (if-let* ((text (org-element-property :title (org-contacts-search-contact name))))
        (overlay-put ov 'after-string (concat
                                       (propertize "{" 'face '(:foreground "purple2"))
                                       (propertize text 'face 'org-verbatim)
                                       (propertize "}" 'face '(:foreground "purple2"))))
      (org-link-beautify-iconify ov path link))))

;;; org-bookmark: link type

(require 'org-bookmarks nil t)

(defun org-link-beautify--generate-preview-for-org-bookmarks (path)
  "Get the bookmark content at title PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((bookmark-title (match-string 1 path))
           (search-option (match-string 2 path))
           ;; reference `org-bookmarks-link-open'
           (bookmark-content (org-bookmarks--get-bookmark-content path))
           (cwd (file-name-directory org-bookmarks-file))
           (text (concat "\n" (org-link-beautify--display-org-content bookmark-content))))
      ;; return the files list in archive file as result.
      text)))

(defun org-link-beautify-preview-org-bookmark (ov path link)
  "Preview org-bookmark: link of PATH over OV overlay position for LINK element."
  (if-let* (((require 'org-bookmarks nil t))
            (text (org-link-beautify--generate-preview-for-org-bookmarks path)))
      (prog1 ov
        (overlay-put ov 'after-string text)
        (overlay-put ov 'face         'org-link))
    (org-link-beautify-iconify ov path link)))

;;; excalidraw: link type

(defun org-link-beautify-preview-excalidraw (ov path link)
  "Preview excalidraw file of PATH over OV overlay position for LINK element."
  ;; TODO: reference `org-excalidraw--shell-cmd-to-svg'
  nil)

;;; geo: link type

(defun org-link-beautify-preview-geography (ov path link)
  "Preview geo: link of PATH over OV overlay position for LINK element."
  ;; TODO
  nil)

;;; git: link type

(defcustom org-link-beautify-git-preview nil
  "Whether enable git: link type preview."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(declare-function vc-git-symbolic-commit "vc-git" (commit &optional force))

(defun org-link-beautify--generate-preview-for-git (ov path link)
  "Get the content on OV overlay of PATH at LINK element."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (commit (match-string 2 path))
           ;; TODO: use `vc-git-*' or `magit' API to get git commit blob content.
           (commit-diff (vc-git-symbolic-commit commit))
           (text (concat "\n" (org-link-beautify--display-content-block commit-diff))))
      text)))

(defun org-link-beautify-preview-git (ov path link)
  "Preview git: link of PATH over OV overlay position for LINK element."
  (if org-link-beautify-git-preview
      (when-let* ((text (org-link-beautify--generate-preview-for-git ov path link)))
        (overlay-put ov 'after-string text)
        (overlay-put ov 'face 'org-block))
    (org-link-beautify-iconify ov path link)))

;;; http[s]: url link type

(defcustom org-link-beautify-url-preview-command nil
  "Find available URL web page screenshot archive command."
  :type `(choice
          :tag "An option to set the command for previewing URL."
          ,(when (or (file-exists-p "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
                     (executable-find "chrome"))
             '(const :tag "Google Chrome --headless --screenshot" 'google-chrome))
          ,(when (executable-find "rinku")
             '(const :tag "Rinku" 'rinku))
          ,(when (executable-find "monolith")
             '(const :tag "CLI tool for saving complete web pages as a single HTML file" 'monolith)))
  :group 'org-link-beautify)

(defcustom org-link-beautify-url-preview-size 800
  "The url web page thumbnail image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-url (ov path link)
  "Generate screenshot archive for the URL PATH web page on OV overlay at LINK element."
  (let* ((type (org-element-property :type link))
         (url (concat type ":" path))
         (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path (buffer-file-name)))
         (thumbnail-file-name (format "org-link-beautify screenshot of sha1 URL %s.png" (sha1 url)))
         (thumbnail-file (expand-file-name thumbnail-file-name thumbnails-dir))
         (html-archive-file (concat (file-name-sans-extension thumbnail-file) ".html"))
         (thumbnail-size (or org-link-beautify-url-preview-size 1000))
         (proc-name (format "org-link-beautify url screenshot - %s" url))
         (proc-buffer (format " *org-link-beautify url screenshot - %s*" url))
         (proc (get-buffer-process (get-buffer proc-buffer))))
    (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
    (if (or (file-exists-p thumbnail-file)
            (file-exists-p html-archive-file))
        (when org-link-beautify-url-preview-command
          (unless (or proc (get-buffer proc-buffer))
            (pcase org-link-beautify-url-preview-command
              ('google-chrome
               (cl-assert (executable-find org-link-beautify-offline-webpage-preview-command) nil
                          "[org-link-beautify] Please install Google Chrome")
               ;; $ google-chrome --headless --screenshot=screenshot.png "https://www.chromestatus.com/"
               (start-process
                proc-name proc-buffer
                org-link-beautify-url-preview-command
                "--headless"
                (format "--screenshot=%s" thumbnail-file)
                url))
              ('rinku
               (cl-assert (executable-find "rinku") nil "[org-link-beautify] Please install `rinku'")
               (let* ((json-response-hash (json-parse-string
                                           (shell-command-to-string
                                            ;; $ rinku --preview --width 600 --height 300 https://soundcloud.com/shehackedyou
                                            (format "%s --preview %s" org-link-beautify-url-preview-command url))))
                      (image-path (gethash "image" json-response-hash))
                      (title (gethash "title" json-response-hash)))
                 (when (derived-mode-p 'org-mode)
                   (org-insert-link nil image-path title))))
              ('monolith
               (cl-assert (executable-find "monolith") nil "[org-link-beautify] Please install `monolith'")
               (let* ((html-archive-file (concat (file-name-sans-extension thumbnail-file) ".html")))
                 (make-process
                  :name proc-name
                  :command (list "monolith" "--no-audio" "--no-video" url "--output" html-archive-file)
                  :buffer proc-buffer
                  :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
                  :sentinel (lambda (proc event)
                              (when (string= event "finished\n")
                                (kill-buffer (process-buffer proc))
                                (kill-process proc))))))))))
    (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
      (org-link-beautify--notify-generate-thumbnail-failed url thumbnail-file))))

(defun org-link-beautify-preview-url (ov path link)
  "Preview http[s]: URL link of PATH over OV overlay position for LINK element."
  (if-let* (( (display-graphic-p))
            (org-link-beautify-url-preview-command)
            (thumbnail-file (org-link-beautify--generate-preview-for-url ov path link))
            ( (file-exists-p thumbnail-file))
            (image (create-image thumbnail-file nil nil :ascent 100))
            (image-width (car (image-size image)))
            (image-height (cdr (image-size image)))
            (display-width (or (let ((org-image-actual-width nil)) (org-display-inline-image--width link)) 800)))
      (prog1 ov
        (setf (image-property image :width) display-width)
        (org-link-beautify-overlay-display-image ov image))
    (org-link-beautify-iconify ov path link)))

;;; Insert Org link without description based on smart detecting file extension.

(defun org-link-beautify-remove-description (orig-func link-raw &optional link-description)
  "Advice function to remove LINK-DESCRIPTION from LINK-RAW around ORIG-FUNC.

This is for link image previewing to get around function `org-link-preview'
\(original named `org-toggle-inline-images'\) parameter `include-linked'."
  (let ((link-type (when (string-match org-link-types-re link-raw) (match-string 1 link-raw)))
        (extension (file-name-extension link-raw)))
    (when (or (member extension org-link-beautify-image-preview-list) ; image files
              (member extension '("pdf" "epub" "mobi" "azw3" "lit" "fb2" "fb2.zip")) ; ebook files
              (member extension org-link-beautify-video-preview-list) ; video files
              (member extension org-link-beautify-audio-preview-list) ; audio files
              (member extension org-link-beautify-comic-preview-list) ; comic files
              (member extension org-link-beautify-archive-preview-list) ; archive files
              (member extension org-link-beautify-subtitle-preview-list) ; subtitle files
              (and (member link-type '("file" "attachment"))
                   (member extension org-link-beautify-offline-webpage-preview-list)) ; offline webpage archive file
              (member link-type '("info" "help" "shortdoc" "man" "woman" "id" "custom-id" "coderef"))
              (member link-type '("elisp" "shell" "js" "javascript" "grep" "occur" "git"))
              (member link-type '("mailto" "rss" "news" "wikipedia" "irc" "magnet" "wechat" "web-browser" "eww" "chrome" "about" "edge"))
              (member link-type '("org-ql-search" "org-contact" "org-bookmark"))
              ;; Emacs package special link types
              ;; NOTE: "epub" "nov" page-number thumbnail generating not supported.
              (member link-type '("pdf" "pdfview" "docview")) ; "epub" "nov"
              (member link-type '("video" "videocite" "audio" "audiocite" "excalidraw"))
              ;; speail meaning link types
              (member link-type '("geo"))
              ;; special application link types
              (member link-type '("vscode" "macappstore")))
      (setq link-description nil)))
  (funcall orig-func link-raw link-description))


;;; minor mode `org-link-beautify-mode'

;;;###autoload
(defun org-link-beautify-enable ()
  "Enable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (pcase link-type
      ("file" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-file)) ; `org-link-preview-file',
      ("attachment"
       (require 'org-attach)
       (org-link-set-parameters link-type :preview #'org-link-beautify-preview-attachment)) ; `org-attach' -> `org-attach-preview-file'
      ("docview" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-pdf)) ; `ol-docview'
      ("pdfview" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-pdf)) ; `org-pdftools'
      ("pdf" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-pdf)) ; `org-pdftools'
      ("epub" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-epub)) ; `nov'
      ("nov" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-nov)) ; `nov'
      ("geo" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-geography)) ; `org-extra-link-types'
      ("http" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("https" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("ftp" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      
      ;; Org mode internal link types
      ("custom-id" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("id" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `org-id'
      ("coderef" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("elisp" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol'
      ("eshell" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-eshell'
      ("shell" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol'
      ("man" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-man'
      ("woman" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `woman'
      ("info" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-info'
      ("help" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `helpful'
      ("shortdoc" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `shortdoc'
      
      ;; Org mode external link types
      ("grep" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("occur" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("mailto" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("news" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("rss" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `org-extra-link-types'
      ("elfeed" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `elfeed-link'
      ("wikipedia" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `org-kiwix'
      ("irc" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-irc'
      ("wechat" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `org-extra-link-types'
      ("magnet" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `org-extra-link-types'
      ("git" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-git)) ; `ol-git-link'
      ("eww" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-eww'
      ("chrome" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("about" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("edge" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("mu4e" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `mu4e-org'
      ("web-browser" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-file-offline-webpage)) ; `org-extra-link-types'
      
      ;; `org-ref' link types
      ("ref" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("cite" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("cite*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("cites" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citeurl" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citetitle" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citetitle*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citeauthor" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citeauthor*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citetext" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citenum" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citedate" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citedate*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citeyear" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citeyear*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citealp" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citealp*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citealt" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citealt*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citep" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citep*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citet" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("citet*" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      
      ;; bibliography
      ("doi" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-doi'
      ("bibtex" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `ol-bibtex'
      ("bibliography" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; `org-ref'
      
      ;; Org mode extensions link types
      ("org-ql-search" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      ("org-contact" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-org-contact))
      ("org-bookmark" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-org-bookmark))
      ("orgit" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-git))
      ("orgit-rev" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-git))
      ("orgit-log" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-git))
      ("excalidraw" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-excalidraw))
      
      ;; org-media-note link types
      ("video"
       (require 'org-media-note)
       (org-link-set-parameters link-type :preview #'org-link-beautify-preview-video))
      ("audio"
       (require 'org-media-note)
       (org-link-set-parameters link-type :preview #'org-link-beautify-preview-audio))
      ("videocite"
       (require 'org-media-note)
       (org-link-set-parameters link-type :preview #'org-link-beautify-preview-video))
      ("audiocite"
       (require 'org-media-note)
       (org-link-set-parameters link-type :preview #'org-link-beautify-preview-audio))
      
      ;; other link types
      ("eaf" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; extension `emacs-application-framework'
      ("javascript" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; Org mode inline source code link
      ("js" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; Org mode inline source code link
      ("vscode" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; Visual Studio Code
      ("macappstore" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; Mac App Store
      ("fuzzy" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)) ; org-mode internal raw link type
      (_ (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))))
  
  ;; remove link description
  (advice-add 'org-link-make-string :around #'org-link-beautify-remove-description))

(declare-function org-attach-preview-file "org-attach" (ov path link))

;;;###autoload
(defun org-link-beautify-disable ()
  "Disable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (pcase link-type
      ("file" (org-link-set-parameters "file" :preview #'org-link-preview-file))
      ("attachment" (org-link-set-parameters "attachment" :preview #'org-attach-preview-file))
      (_ (org-link-set-parameters link-type :preview nil))))
  (advice-remove 'org-link-make-string #'org-link-beautify-remove-description))

(defvar org-link-beautify-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "The `org-link-beautify-mode' minor mode map.")

;;;###autoload
(define-minor-mode org-link-beautify-mode
  "A minor mode to beautify Org Mode links with icons, and inline preview etc."
  :group 'org-link-beautify
  :global t
  :init-value nil
  :lighter " ߷"
  :keymap org-link-beautify-mode-map ; avoid to enable `org-link-beautify-keymap' globally everywhere.
  (if org-link-beautify-mode
      (org-link-beautify-enable)
    (org-link-beautify-disable)))



(provide 'org-link-beautify-new)

;;; org-link-beautify-new.el ends here
