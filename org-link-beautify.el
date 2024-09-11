;;; org-link-beautify.el --- Beautify Org Links -*- lexical-binding: t; -*-

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.0.1") (fb2-reader "0.1.1") (qrencode "1.2"))
;; Version: 1.2.3
;; Keywords: hypermedia
;; homepage: https://repo.or.cz/org-link-beautify.git

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
;;   :hook (org-mode . org-link-beautify-mode))

;;; Code:

(require 'ol)
(require 'org)
(require 'org-element)
(require 'org-element-ast)
(require 'nerd-icons)
(require 'color)
(require 'cl-lib)
(require 'time-stamp)
(require 'qrencode)

;; (require 'fb2-reader)
(declare-function fb2-reader--create-image "fb2-reader" (data type &rest props))
(declare-function fb2-reader--extract-image-data "fb2-reader" (book attributes &optional tags))
(declare-function fb2-reader--get-cover "fb2-reader" (book))
(declare-function fb2-reader-parse-file-as-html "fb2-reader" (file))
(declare-function fb2-reader-parse-file-as-xml "fb2-reader" (file))

(defgroup org-link-beautify nil
  "Customize group of `org-link-beautify-mode'."
  :prefix "org-link-beautify-"
  :group 'org)

(defcustom org-link-beautify-async-preview nil
  "Use async thread to run preview display function.
This will improve package performance without blocking Emacs."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-thumbnails-dir 'source-path
  "The directory of generated thumbnails.

By default the thumbnails are generated in source file path’s
.thumbnails directory. This is better for avoiding re-generate
preview thumbnails. Or you can set this option to ‘'user-home’
which represent to ~/.cache/thumbnails/."
  :type 'symbol
  :safe #'symbolp
  :group 'org-link-beautify)

(defcustom org-link-beautify-display-overlay-info nil
  "Whether display link info with Emacs overlay over link thumbnail.
NOTE: overlays may mess up buffer when you cut text etc."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-image-preview nil
  "Whether enable image files thumbnail preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-image-preview-list
  '("jpg" "jpeg" "png" "gif" "webp")
  "A list of image file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-preview (or (executable-find "ffmpegthumbnailer")
                                               (executable-find "qlmanage")
                                               (executable-find "ffmpeg"))
  "Whether enable video files thumbnail preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-preview-size 512
  "The video thumbnail image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-preview-list
  '("rmvb" "ogg" "ogv" "mp4" "mkv" "mov" "m4v" "webm" "flv")
  "A list of video file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-subtitle-preview t
  "Whether enable subtitle files previewing?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

;;; https://en.wikipedia.org/wiki/Subtitles
(defcustom org-link-beautify-subtitle-preview-list
  '("ass" "srt" "sub" "vtt" "ssf")
  "A list of subtitle file types support previewing."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-audio-preview (or (executable-find "audiowaveform")
                                               (executable-find "qlmanage"))
  "Whether enable audio files wave form preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-audio-preview-list '("mp3" "wav" "flac" "ogg" "m4a" "dat")
  "A list of audio file types be supported generating audio wave form image."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-audio-preview-size 300
  "The audio wave form image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-pdf-preview (or (executable-find "pdftocairo")
                                             (executable-find "pdf2svg"))
  "Whether enable PDF files image preview?
If command \"pdftocairo\" or \"pdf2svg\" is available, enable PDF
preview by default. You can set this option to nil to disable
PDF preview."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-pdf-preview-command 'pdftocairo
  "The command used to preview PDF file cover."
  :type '(choice
          :tag "The command used to preview PDF cover."
          (const :tag "pdftocairo" pdftocairo)
          (const :tag "pdf2svg" pdf2svg))
  :safe #'symbolp
  :group 'org-link-beautify)

;;; TODO: smarter value decided based on screen size.
(defcustom org-link-beautify-pdf-preview-size 512
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

(defcustom org-link-beautify-epub-preview
  (cl-case system-type
    (gnu/linux (executable-find "gnome-epub-thumbnailer"))
    ;; (darwin (executable-find "epub-thumbnailer"))
    (t (expand-file-name "scripts/thumbnailer-ebook.py" (file-name-directory (or load-file-name (buffer-file-name))))))
  "Whether enable EPUB files cover preview?
If command \"gnome-epub-thumbnailer\" is available, enable EPUB
preview by default. You can set this option to nil to disable
EPUB preview."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-kindle-preview
  (cl-case system-type
    (gnu/linux (executable-find "mobitool"))
    (darwin (executable-find "mobitool")))
  "Whether enable Kindle ebook files cover preview?

Enable Kindle ebook preview by default. You can set this option
to nil to disable EPUB preview.

You can install software `libmobi' to get command `mobitool'."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-fictionbook2-preview (featurep 'fb2-reader)
  "Whether enable FictionBook2 ebook files covert preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-ebook-preview-size nil
  "The EPUB cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-comic-preview
  (cl-case system-type
    (darwin (executable-find "qlmanage")))
  "Whether enable CDisplay Archived Comic Book Formats cover preview.
File extensions like (.cbr, .cbz, .cb7, .cba, .cbt etc)."
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-comic-preview-size 500
  "The CDisplay Archived Comic Book Formats cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-text-preview nil
  "Whether enable text files content preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-text-preview-list
  '("org" "txt" "markdown" "md"
    "lisp" "scm" "clj" "cljs"
    "py" "rb" "pl"
    "c" "cpp" "h" "hpp" "cs" "java"
    "r" "jl")
  "A list of link types supports text preview below the link."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-archive-preview nil
  "Whether enable archive inside files list preview?"
  :type 'boolean
  :safe #'booleanp
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

(defcustom org-link-beautify-archive-preview-command (executable-find "7z")
  "The command to list out files inside archive file."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-url-preview nil
  "Whether enable URL link preview?"
  :type 'boolean
  :safe #'booleanp
  :group 'org-link-beautify)

(defcustom org-link-beautify-url-preview-size 512
  "The URL web page preview thumbnail size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defcustom org-link-beautify-enable-debug-p nil
  "Whether enable org-link-beautify print debug info."
  :type 'boolean
  :safe #'booleanp)

;;; Helper functions

;;; Invoke external Python script file or code.
(defcustom org-link-beautify-python-interpreter (executable-find "python3")
  "Specify Python interpreter to run python scripts or code."
  :type 'string
  :safe #'stringp)

(defun org-link-beautify--python-script-run (python-script-file)
  "Run PYTHON-SCRIPT-FILE through shell command."
  (shell-command-to-string
   (format "%s %s" org-link-beautify-python-interpreter python-script-file)))

(defun org-link-beautify--python-command-to-string (&rest code-lines)
  "Run Python CODE-LINES through shell command."
  (shell-command-to-string
   (concat "python -c "
           ;; solve double quote character issue.
           "\"" (string-replace "\"" "\\\"" (string-join code-lines "\n")) "\"")))

;;; e.g.
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
  (shell-command-to-string
   (format "%s %s" org-link-beautify-python-interpreter javascript-script-file)))

(defun org-link-beautify--javascript-command-to-string (&rest code-lines)
  "Run JavaScript CODE-LINES through shell command."
  (shell-command-to-string
   (concat "node --eval "
           ;; solve double quote character issue.
           "\"" (string-replace "\"" "\\\"" (string-join code-lines "\n")) "\"")))

(org-link-beautify--javascript-command-to-string
 "console.log(\"hello, world!\");"
 "console.log(1 + 3);")

;;; Common functions
;; replace the whole Org buffer font-lock function `org-restart-font-lock'
;; with a lightweight `jit-lock-refontify' current headline scope only
;; font-lock function.
(defmacro org-link-beautify--subtree-scope-wrap (body)
  "Wrap the BODY to executed in scope of current subtree to get BEGIN and END position."
  `(save-excursion
     (save-restriction
       (org-narrow-to-subtree)
       (let* ((begin (point-min))
              (end (save-excursion (org-next-visible-heading 1) (point))))
         ,body))))

(defun org-link-beautify--get-element (position)
  "Return the org element of link at the `POSITION'."
  (save-excursion
    (goto-char position)
    ;; Parse link at point, if any. replace (org-element-context) to improve performance.
    (org-element-link-parser)))

(defun org-link-beautify--get-link-description-fast (position)
  "Get the link description at `POSITION' (fuzzy but faster version)."
  (save-excursion
    (goto-char position)
    (and (org-in-regexp org-link-bracket-re) (match-string 2))))

(defun org-link-beautify--warning-face-p (path)
  "Use `org-warning' face if link PATH does not exist."
  (if (and (not (file-remote-p path))
           (file-exists-p (expand-file-name path)))
      'org-link 'org-warning))

(defun org-link-beautify--notify-generate-thumbnail-failed (source-file thumbnail-file)
  "Notify that generating THUMBNAIL-FILE for SOURCE-FILE failed."
  (message
   "[org-link-beautify] For file %s.\nCreate thumbnail %s failed."
   source-file thumbnail-file))

(defun org-link-beautify--add-text-property-marker (start end)
  "Add \\='org-link-beautify on link text-property. between START and END."
  (put-text-property start end 'type 'org-link-beautify))

(defun org-link-beautify--add-overlay-info (thumbnail start end)
  "Display info over the link. Put the THUMBNAIL overlay on START instead of END."
  (when org-link-beautify-display-overlay-info
    (let* ((beginning (- start 1))
           (end (- start 1))
           (filename (file-name-nondirectory thumbnail))
           ;; Insert an overlay new line.
           ;; -> Prepend a "\n" *newline character* before the /overlay/ ~'before-string~ string.
           (str-text-property (concat "\n" "🔗 " (propertize filename 'face 'font-lock-doc-markup-face))))
      ;; Detect whether overlay already exist? To avoid insert duplicated overlays many times.
      ;; (unless (overlay-get (symbol-value 'ov-name) 'before-string) ...)
      (unless (seq-some
               (lambda (element) (not (null element)))
               (mapcar
                (lambda (plist)
                  (plist-get plist 'before-string))
                (mapcar 'overlay-properties
                        (overlays-in beginning end))))
        ;; Make random overlay object symbol names to store different overlays.
        (setq ov-name (make-symbol (concat "org-link-beautify--overlay-" (format "%010d" (random 10000000000)))))
        (set-variable (symbol-value 'ov-name) (make-overlay beginning end))
        ;; display filename
        (overlay-put (symbol-value (symbol-value 'ov-name)) 'before-string str-text-property)
        (overlay-put (symbol-value (symbol-value 'ov-name)) 'evaporate t)))))

;;; TEST:
;; (org-link-beautify--add-overlay-info "/path/to/thumbnail.png" (+ (point) 1) (+ (point) 2))
;; (remove-overlays (point) (+ (point) 1)) ; clear displayed overlays.

(defun org-link-beautify--get-thumbnails-dir-path (file)
  "Return the FILE thumbnail directory's path."
  (cl-case org-link-beautify-thumbnails-dir
    (source-path
     (concat (file-name-directory file) ".thumbnails/"))
    (user-home
     (expand-file-name "~/.cache/thumbnails/"))))

(defun org-link-beautify--ensure-thumbnails-dir (thumbnails-dir)
  "Ensure THUMBNAILS-DIR exist, if not ,create it."
  (unless (file-directory-p thumbnails-dir)
    (make-directory thumbnails-dir)))

(defun org-link-beautify--display-thumbnail (thumbnail thumbnail-size start end &optional border-width border-color)
  "Display THUMBNAIL between START and END with THUMBNAIL-SIZE and in BORDER-WIDTH BORDER-COLOR when exist."
  (when (and (file-exists-p thumbnail)
             (listp (get-text-property start 'display))
             ;; If thumbnail image already displayed, don't re-display thumbnail.
             (not (eq (car (get-text-property start 'display)) 'image)))
    (put-text-property
     start end
     'display (create-image thumbnail nil nil :ascent 100 :max-height thumbnail-size))
    (when border-color
      (put-text-property start end 'face `(:box (:line-width ,(or border-width 1) :color ,border-color))))))

(defun org-link-beautify--display-content-block (lines-list)
  "Display LINES-LIST string as a block with beautified frame border."
  (format
   "
┏━§ ✂ %s
%s
┗━§ ✂ %s
\n"
   (make-string (- fill-column 6) ?━)
   (mapconcat
    (lambda (line)
      (concat "┃" line))
    lines-list
    "\n")
   (make-string (- fill-column 6) ?━)))


;;; Preview functions
(defun org-link-beautify--preview-pdf (path start end &optional search-option)
  "Preview PDF file PATH with optional SEARCH-OPTION on link between START and END."
  (if (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
      (let* ((file-path (match-string 1 path))
             ;; DEBUG:
             ;; (_ (lambda () (message "--> DEBUG: org-link-beautify (pdf): path: %s" path)))
             ;; (_ (lambda () (message "--> DEBUG: org-link-beautify (pdf): search-option: %s" search-option)))
             (pdf-page-number (if search-option
                                  (string-to-number
                                   (cond
                                    ((string-prefix-p "P" search-option) ; "P42"
                                     (substring search-option 1 nil))
                                    ((string-match "\\([[:digit:]]+\\)\\+\\+\\(.*\\)" search-option) ; "40++0.00"
                                     (match-string 1 search-option))
                                    (t search-option)))
                                (if-let ((search-option (match-string 2 path)))
                                    (string-to-number
                                     (cond
                                      ((string-prefix-p "P" search-option) ; "P42"
                                       (substring search-option 1 nil))
                                      ((string-match "\\([[:digit:]]+\\)\\+\\+\\(.*\\)" search-option) ; "40++0.00"
                                       (match-string 1 search-option))
                                      (t search-option)))
                                  org-link-beautify-pdf-preview-default-page-number)))
             (pdf-file (expand-file-name (org-link-unescape file-path)))
             (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path pdf-file))
             (thumbnail-file (expand-file-name
                              (if (= pdf-page-number 1) ; if have page number ::N specified.
                                  (format "%s%s.%s"
                                          thumbnails-dir (file-name-base pdf-file)
                                          (symbol-name org-link-beautify-pdf-preview-image-format))
                                (format "%s%s-P%s.%s"
                                        thumbnails-dir (file-name-base pdf-file) pdf-page-number
                                        (symbol-name org-link-beautify-pdf-preview-image-format)))))
             (thumbnail-size (or org-link-beautify-pdf-preview-size 512)))
        (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
        (unless (file-exists-p thumbnail-file)
          (pcase org-link-beautify-pdf-preview-command
            ('pdftocairo
             ;; DEBUG:
             ;; (message
             ;;  "org-link-beautify: page-number %s, pdf-file %s, thumbnail-file %s"
             ;;  pdf-page-number pdf-file thumbnail-file)
             (start-process
              "org-link-beautify--pdf-preview"
              " *org-link-beautify pdf-preview*"
              "pdftocairo"
              (pcase org-link-beautify-pdf-preview-image-format
                ('png "-png")
                ('jpeg "-jpeg")
                ('svg "-svg"))
              "-singlefile"
              "-f" (number-to-string pdf-page-number)
              pdf-file (file-name-sans-extension thumbnail-file))
             (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
               (org-link-beautify--notify-generate-thumbnail-failed pdf-file thumbnail-file)))
            ('pdf2svg
             (unless (eq org-link-beautify-pdf-preview-image-format 'svg)
               (warn "The pdf2svg only supports convert PDF to SVG format.
Please adjust `org-link-beautify-pdf-preview-command' to `pdftocairo' or
Set `org-link-beautify-pdf-preview-image-format' to `svg'."))
             (start-process
              "org-link-beautify--pdf-preview"
              " *org-link-beautify pdf-preview*"
              "pdf2svg"
              pdf-file thumbnail-file (number-to-string pdf-page-number))
             (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
               (org-link-beautify--notify-generate-thumbnail-failed pdf-file thumbnail-file)))))
        (org-link-beautify--add-overlay-info thumbnail-file start end)
        (org-link-beautify--add-text-property-marker start end)
        (org-link-beautify--add-keymap start end)
        ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
        (if (file-exists-p thumbnail-file)
            (org-link-beautify--display-thumbnail thumbnail-file thumbnail-size start end)
          'error))))

(defun org-link-beautify--preview-epub (path start end &optional search-option)
  "Preview EPUB file at PATH and display SEARCH-OPTION on link between START and END."
  (if (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
      (let* ((file-path (match-string 1 path))
             ;; DEBUG: (_ (lambda () (message "--> DEBUG: ")))
             (epub-page-number (or (match-string 2 path) 1))
             (epub-file (expand-file-name (org-link-unescape file-path)))
             (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path epub-file))
             (thumbnail-file (expand-file-name
                              (if (or (null epub-page-number) (= epub-page-number 1)) ; if have page number ::N specified.
                                  (format "%s%s.png" thumbnails-dir (file-name-base epub-file))
                                (format "%s%s-P%s.png" thumbnails-dir (file-name-base epub-file) epub-page-number))))
             (thumbnail-size (or org-link-beautify-ebook-preview-size 500)))
        (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
        ;; DEBUG:
        ;; (message epub-file)
        (unless (file-exists-p thumbnail-file)
          (cl-case system-type
            (gnu/linux                 ; for Linux "gnome-epub-thumbnailer"
             (start-process
              "org-link-beautify--epub-preview"
              " *org-link-beautify epub-preview*"
              org-link-beautify-epub-preview
              epub-file thumbnail-file
              ;; (if org-link-beautify-ebook-preview-size
              ;;     "--size")
              ;; (if org-link-beautify-ebook-preview-size
              ;;     (number-to-string thumbnail-size))
              )
             (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
               (org-link-beautify--notify-generate-thumbnail-failed epub-file thumbnail-file)))
            (darwin            ; for macOS "epub-thumbnailer" command
             ;; DEBUG
             ;; (message epub-file)
             ;; (message thumbnail-file)
             ;; (message (number-to-string org-link-beautify-ebook-preview-size))
             (make-process
              :name "org-link-beautify--epub-preview"
              :command (list org-link-beautify-epub-preview
                             epub-file
                             thumbnail-file
                             (number-to-string thumbnail-size))
              :buffer " *org-link-beautify epub-preview*"
              :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
              :sentinel (lambda (proc event)
                          (if org-link-beautify-enable-debug-p
                              (message (format "> proc: %s\n> event: %s" proc event))
                            ;; (when (string= event "finished\n")
                            ;;   (kill-buffer (process-buffer proc))
                            ;;   (kill-process proc))
                            )))
             (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
               (org-link-beautify--notify-generate-thumbnail-failed epub-file thumbnail-file)))
            (t (user-error "This system platform currently not supported by org-link-beautify.\n Please contribute code to support"))))
        (org-link-beautify--add-overlay-info thumbnail-file start end)
        (org-link-beautify--add-text-property-marker start end)
        (org-link-beautify--add-keymap start end)
        ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
        (if (file-exists-p thumbnail-file)
            (org-link-beautify--display-thumbnail thumbnail-file thumbnail-size start end)
          'error))))

(defvar org-link-beautify--kindle-cover
  (cond
   ;; for macOS, use `mobitool' from libmobi.
   ((and (eq system-type 'darwin) (executable-find "mobitool")) "mobitool")
   ;; for Linux, use `mobitool' from libmobi.
   ((and (eq system-type 'gnu/linux) (executable-find "mobitool")) "mobitool"))
  "Find available kindle ebook cover dump command.
You can install software `libmobi' to get command `mobitool'.")

(defun org-link-beautify--preview-kindle (path start end &optional search-option)
  "Preview Kindle ebooks at PATH and display SEARCH-OPTION on link between START and END."
  (if (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
      (let* ((file-path (match-string 1 path))
             ;; DEBUG: (_ (lambda () (message "--> DEBUG: ")))
             (kindle-page-number (or (match-string 2 path) 1))
             (kindle-file (expand-file-name (org-link-unescape file-path)))
             (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path kindle-file))
             (thumbnail-file (expand-file-name
                              (if (or (null kindle-page-number) (= kindle-page-number 1)) ; if have page number ::N specified.
                                  (format "%s%s.jpg" thumbnails-dir (file-name-base kindle-file))
                                (format "%s%s-P%s.jpg" thumbnails-dir (file-name-base kindle-file) kindle-page-number))))
             (thumbnail-size (or org-link-beautify-ebook-preview-size 500)))
        (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
        ;; DEBUG:
        ;; (message kindle-file)
        (unless (file-exists-p thumbnail-file)
          (pcase org-link-beautify--kindle-cover
            ("mobitool" ; NOTE: mobitool command-line tool dump covert image filename can't be specified.
             (let ((mobitool-cover-file (concat thumbnails-dir (file-name-base kindle-file) "_cover.jpg")))
               (unless (file-exists-p mobitool-cover-file)
                 (message "[org-link-beautify] preview kindle ebook file %s" kindle-file)
                 (start-process
                  "org-link-beautify--kindle-preview"
                  " *org-link-beautify kindle-preview*"
                  "mobitool" "-c" "-o" thumbnails-dir kindle-file))
               ;; then rename [file.extension.jpg] to [file.jpg]
               (when (file-exists-p mobitool-cover-file)
                 (rename-file mobitool-cover-file thumbnail-file))
               (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
                 (org-link-beautify--notify-generate-thumbnail-failed kindle-file thumbnail-file))))
            (_ (user-error "[org-link-beautify] Error: Can't find command tool to dump kindle ebook file cover"))))
        (org-link-beautify--add-overlay-info thumbnail-file start end)
        (org-link-beautify--add-text-property-marker start end)
        (org-link-beautify--add-keymap start end)
        ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
        (if (file-exists-p thumbnail-file)
            (org-link-beautify--display-thumbnail thumbnail-file thumbnail-size start end)
          'error))))

(defun org-link-beautify--preview-comic (path start end &optional search-option)
  "Preview CDisplay Archived Comic Book file at PATH and display SEARCH-OPTION on link between START and END."
  (if (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
      (let* ((file-path (match-string 1 path))
             ;; DEBUG: (_ (lambda () (message "--> DEBUG: ")))
             (comic-file (expand-file-name (org-link-unescape file-path)))
             (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path comic-file))
             (thumbnail-file (expand-file-name
                              (format "%s%s.png" thumbnails-dir (file-name-base comic-file))))
             (thumbnail-size (or org-link-beautify-comic-preview-size 1080)))
        (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
        ;; DEBUG:
        ;; (message comic-file)
        (unless (file-exists-p thumbnail-file)
          (cl-case system-type
            ;; TODO:
            ;; (gnu/linux
            ;;  (start-process
            ;;   "org-link-beautify--comic-preview"
            ;;   " *org-link-beautify comic-preview*"
            ;;   org-link-beautify-comic-preview
            ;;   comic-file thumbnail-file
            ;;   ;; (if org-link-beautify-comic-preview-size
            ;;   ;;     "--size")
            ;;   ;; (if org-link-beautify-comic-preview-size
            ;;   ;;     (number-to-string thumbnail-size))
            ;;   )
            ;;  (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
            ;;    (org-link-beautify--notify-generate-thumbnail-failed comic-file thumbnail-file)))
            (darwin
             ;; DEBUG
             ;; (message comic-file)
             ;; (message thumbnail-file)
             ;; (message (number-to-string org-link-beautify-comic-preview-size))
             
             ;; for macOS "qlmanage" command
             ;; $ qlmanage -t "ラセン恐怖閣-マリコとニジロー1-DL版.cbz" - 2.0 -s 1080 -o ".thumbnails"
             (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory comic-file) ".png")))
               (make-process
                :name "org-link-beautify--comic-preview"
                :command (list org-link-beautify-comic-preview
                               "-t"
                               comic-file
                               "-o" thumbnails-dir
                               "-s" (number-to-string thumbnail-size))
                :buffer " *org-link-beautify comic-preview*"
                :stderr nil ; If STDERR is nil, standard error is mixed with standard output and sent to BUFFER or FILTER.
                :sentinel (lambda (proc event)
                            (if org-link-beautify-enable-debug-p
                                (message (format "> proc: %s\n> event: %s" proc event))
                              ;; (when (string= event "finished\n")
                              ;;   (kill-buffer (process-buffer proc))
                              ;;   (kill-process proc))
                              )))
               ;; then rename [file.extension.png] to [file.png]
               (when (file-exists-p qlmanage-thumbnail-file)
                 (rename-file qlmanage-thumbnail-file thumbnail-file))
               (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
                 (org-link-beautify--notify-generate-thumbnail-failed comic-file thumbnail-file))))
            (t (user-error "This system platform currently not supported by org-link-beautify.\n Please contribute code to support"))))
        (org-link-beautify--add-overlay-info thumbnail-file start end)
        (org-link-beautify--add-text-property-marker start end)
        (org-link-beautify--add-keymap start end)
        ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
        (if (file-exists-p thumbnail-file)
            (org-link-beautify--display-thumbnail thumbnail-file thumbnail-size start end)
          'error))))

(defun org-link-beautify--fictionbook2-extract-cover (file-path)
  "Extract cover image data for FictionBook2 at FILE-PATH."
  (if-let* ((fb2-file-path file-path)
            ;; `fb2-reader-mode'
            (book (or (fb2-reader-parse-file-as-xml fb2-file-path)
                      (fb2-reader-parse-file-as-html fb2-file-path)))
            ;; `fb2-reader-splash-screen'
            (cover-item (fb2-reader--get-cover book))
            ;; `fb2-reader-splash-cover': (fb2-reader-splash-cover book cover-item)
            (attrs (cl-second (cl-third cover-item)))
            (img-data (fb2-reader--extract-image-data book attrs))
            (type (cl-first img-data))
            (data (cl-second img-data))
            ;; `fb2-reader--insert-image': (fb2-reader--insert-image data-str type-str nil t)
            (type-symbol (alist-get type '(("image/jpeg" . jpeg) ("image/png" . png))))
            (data-decoded (base64-decode-string data))
            (img-raw (fb2-reader--create-image data-decoded type-symbol))
            (image (create-image data-decoded type-symbol 't)))
      image
    'no-cover))

(defun org-link-beautify--fictionbook2-save-cover (image file-path)
  "Save FictionBook2 cover IMAGE to FILE-PATH."
  ;; TODO: how to save image data into image file?
  ;; `image-save': This writes the original image data to a file.
  (with-temp-buffer
    (insert (plist-get (cdr image) :data))
    (write-region (point-min) (point-max) file-path)))

(defun org-link-beautify--preview-fictionbook2 (path start end &optional search-option)
  "Preview FictionBook2 ebooks at PATH and display SEARCH-OPTION on link between START and END."
  (require 'fb2-reader)
  (let* ((fb2-file-path (expand-file-name (org-link-unescape path)))
         ;; (_ (lambda () (message "--> DEBUG: %s" fb2-file-path)))
         (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path fb2-file-path))
         (thumbnail-file-path (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base fb2-file-path))))
         (thumbnail-size (or org-link-beautify-ebook-preview-size 500)))
    (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
    (unless (file-exists-p thumbnail-file-path)
      (let ((cover-image (org-link-beautify--fictionbook2-extract-cover fb2-file-path)))
        (if (eq cover-image 'no-cover)
            (message "[org-link-beautify] FictionBook2 preview failed to extract cover image.")
          (org-link-beautify--fictionbook2-save-cover cover-image thumbnail-file-path))))
    (org-link-beautify--add-overlay-info thumbnail-file start end)
    (org-link-beautify--add-text-property-marker start end)
    (org-link-beautify--add-keymap start end)
    ;; display thumbnail-file-path only when it exist, otherwise it will break org-mode buffer fontification.
    (if (file-exists-p thumbnail-file-path)
        (org-link-beautify--display-thumbnail thumbnail-file-path thumbnail-size start end)
      'error)))

;;; TEST: [M-:] eval bellowing code on FictionBook2 link.
;; (let* ((context (org-element-context))
;;        (beg (org-element-property :begin context))
;;        (end (org-element-property :end context))
;;        (path (org-element-property :path context)))
;;   (org-link-beautify--preview-fictionbook2
;;    path
;;    beg end))

(defvar org-link-beautify--preview-text--noerror)

(defun org-link-beautify--preview-text-file (file lines)
  "Return first LINES of FILE."
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
           (cl-loop repeat lines
                    unless (eobp)
                    collect (prog1 (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))
                              (forward-line 1)))))
      (file-error
       (funcall (if org-link-beautify--preview-text--noerror #'message #'user-error)
		        "Unable to read file %S"
		        file)
	   nil))))

;;; test
;; (org-link-beautify--preview-text-file
;;  (expand-file-name "~/Code/Emacs/org-link-beautify/README.org")
;;  3)

(defun org-link-beautify--preview-text (path start end &optional lines)
  "Preview LINES of TEXT file PATH and display on link between START and END."
  (let* ((text-file (expand-file-name (org-link-unescape path)))
         (preview-lines (or lines 10))
         (preview-content (org-link-beautify--preview-text-file text-file preview-lines)))
    (org-link-beautify--add-text-property-marker (1+ end) (+ end 2))
    (org-link-beautify--add-keymap (1+ end) (+ end 2))
    (put-text-property (1+ end) (+ end 2) 'display (propertize preview-content))
    (put-text-property (1+ end) (+ end 2) 'face '(:inherit org-block)))
  ;; Fix elisp compiler warning: Unused lexical argument `start'.
  (ignore start))

(defun org-link-beautify--preview-archive-file (file command)
  "Return the files list inside of archive FILE with COMMAND."
  (let ((cmd (format "%s '%s'" command file)))
    (org-link-beautify--display-content-block
     ;; split large string content into list of lines.
     (split-string (shell-command-to-string cmd) "\n"))))

(defun org-link-beautify--preview-archive (path command start end)
  "Preview archive PATH content with COMMAND on link between START and END."
  (let* ((archive-file (expand-file-name (org-link-unescape path)))
         (preview-content (org-link-beautify--preview-archive-file archive-file command)))
    (org-link-beautify--add-text-property-marker (1+ end) (+ end 2))
    (org-link-beautify--add-keymap (1+ end) (+ end 2))
    (put-text-property (1+ end) (+ end 2) 'display (propertize preview-content))
    (put-text-property (1+ end) (+ end 2) 'face '(:inherit org-verbatim)))
  ;; Fix elisp compiler warning: Unused lexical argument `start'.
  (ignore start))

(defun org-link-beautify--preview-image (type path start end)
  "Preview image file PATH and display on link TYPE between START and END."
  (let* ((image-file (pcase type
                       ("file" (expand-file-name (org-link-unescape path)))
                       ("image" (expand-file-name (org-link-unescape path)))
                       ("attachment" (expand-file-name (org-link-unescape path) (org-attach-dir)))))
         (thumbnail-size (or (cond
                              ((listp org-image-actual-width)
                               (car org-image-actual-width))
                              ((numberp org-image-actual-width)
                               org-image-actual-width))
                             (org-display-inline-image--width
                              (org-element-lineage
			                   (save-match-data (org-element-context))
			                   'link t)))))
    (put-text-property
     start end
     'display (create-image image-file nil nil :ascent 100 :width thumbnail-size))))

(defvar org-link-beautify--video-thumbnailer
  (cond
   ;; for macOS, use `qlmanage'
   ((and (eq system-type 'darwin) (executable-find "qlmanage")) "qlmanage")
   ;; for Linux, use `ffmpegthumbnailer'
   ((and (eq system-type 'gnu/linux) (executable-find "ffmpegthumbnailer")) "ffmpegthumbnailer")
   ;; for general, use `ffmpeg'
   ;; $ ffmpeg -i video.mp4 -ss 00:01:00.000 -vframes 1 -vcodec png -an -f rawvideo -s 119x64 out.png
   ((executable-find "ffmpeg") "ffmpeg"))
  "Find available video thumbnailer command.")

(defun org-link-beautify--preview-video (type path start end)
  "Preview video file PATH and display on link TYPE between START and END."
  ;; DEBUG: (message "type: %s, path: %s, start: %s, end: %s" type path start end)
  (let* ((video-file (pcase type
                       ("file" (expand-file-name (org-link-unescape path)))
                       ("video" (expand-file-name (org-link-unescape path)))
                       ("attachment" (expand-file-name (org-link-unescape path) (org-attach-dir)))))
         (video-filename (file-name-nondirectory video-file))
         (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path video-file))
         (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base video-file))))
         (thumbnail-size (or org-link-beautify-video-preview-size 512))
         (proc-name (format "org-link-beautify--video-preview - %s" video-filename))
         (proc-buffer (format " *org-link-beautify video-preview - %s*" video-filename))
         (proc (get-process proc-name)))
    (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
    (unless (file-exists-p thumbnail-file)
      ;; detect process already running?
      (unless proc
        (pcase org-link-beautify--video-thumbnailer
          ("qlmanage"
           (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory video-file) ".png")))
             (unless (file-exists-p qlmanage-thumbnail-file)
               (let ((proc (start-process
                            proc-name proc-buffer
                            "qlmanage" "-x" "-t" "-s" (number-to-string thumbnail-size) video-file "-o" thumbnails-dir))
                     (proc-filter (lambda (proc output)
                                    ;; * No thumbnail created for [FILE PATH]
                                    (when (string-match "\\* No thumbnail created for.*" output)
                                      (message
                                       "[org-link-beautify] video preview FAILED on macOS QuickLook generating thumbnail for %s"
                                       video-filename)))))
                 (set-process-filter proc proc-filter)))
             ;; then rename [file.extension.png] to [file.png]
             (when (file-exists-p qlmanage-thumbnail-file)
               (rename-file qlmanage-thumbnail-file thumbnail-file))
             (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
               (org-link-beautify--notify-generate-thumbnail-failed video-file thumbnail-file))))
          ("ffmpegthumbnailer"
           (start-process
            proc-name proc-buffer
            "ffmpegthumbnailer" "-f" "-i" video-file "-s" (number-to-string thumbnail-size) "-o" thumbnail-file)
           (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
             (org-link-beautify--notify-generate-thumbnail-failed video-file thumbnail-file)))
          ("ffmpeg"
           ;; $ ffmpeg -i video.mp4 -ss 00:01:00.000 -vframes 1 -vcodec png -an -f rawvideo -s 119x64 out.png
           (start-process
            proc-name proc-buffer
            "ffmpeg" "-i" video-file "-ss" "00:01:00.000" "-vframes" "1"
            "-vcodec" "png" "-an" "-f" "rawvideo" "-s" (number-to-string thumbnail-size) thumbnail-file)
           (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
             (org-link-beautify--notify-generate-thumbnail-failed video-file thumbnail-file))))))
    (org-link-beautify--add-overlay-info thumbnail-file start end)
    (org-link-beautify--add-text-property-marker start end)
    (org-link-beautify--add-keymap start end)
    ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
    (if (file-exists-p thumbnail-file)
        (org-link-beautify--display-thumbnail
         thumbnail-file thumbnail-size start end
         5 (cl-case (frame-parameter nil 'background-mode)
             (light (color-darken-name (face-background 'default) 10))
             (dark (color-lighten-name (face-background 'default) 5))))
      'error)))

(defun org-link-beautify--preview-subtitle (path start end &optional lines)
  "Preview subtitle file on PATH and display LINES on link between START and END."
  ;; display preview only when it exist, otherwise it will break org-mode buffer fontification.
  (org-link-beautify--preview-text path start end (or lines 20)))

;;; TEST:
;; (org-link-beautify--preview-subtitle
;;  (expand-file-name "/path/to/subtitle.ass")
;;  nil nil
;;  3)

(defvar org-link-beautify--audio-thumbnailer
  (cond
   ;; for macOS, use `qlmanage'
   ((and (eq system-type 'darwin) (executable-find "qlmanage")) "qlmanage")
   ;; for Linux, use `audiowaveform'
   ((and (eq system-type 'gnu/linux) (executable-find "audiowaveform")) "audiowaveform")
   ;; for general, use `ffmpeg'
   ((executable-find "ffmpeg") "ffmpeg"))
  "Find available audio thumbnailer command.")

(defun org-link-beautify--preview-audio (type path start end)
  "Preview audio PATH and display on link TYPE between START and END."
  (let* ((audio-file (pcase type
                       ("file" (expand-file-name (org-link-unescape path)))
                       ("audio" (expand-file-name (org-link-unescape path)))
                       ("attachment" (expand-file-name (org-link-unescape path) (org-attach-dir)))))
         (audio-filename (file-name-nondirectory audio-file))
         (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path audio-file))
         (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base audio-file))))
         (thumbnail-size (or org-link-beautify-audio-preview-size 200))
         (proc-name (format "org-link-beautify--audio-preview - %s" audio-filename))
         (proc-buffer (format " *org-link-beautify audio preview - %s*" audio-filename))
         (proc (get-process proc-name)))
    (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
    (unless (file-exists-p thumbnail-file)
      (unless proc
        (pcase org-link-beautify--audio-thumbnailer
          ("qlmanage"
           (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory audio-file) ".png")))
             (unless (file-exists-p qlmanage-thumbnail-file)
               (start-process proc-name proc-buffer
                              "qlmanage" "-x" "-t" "-s" (number-to-string thumbnail-size) audio-file "-o" thumbnails-dir))
             ;; then rename [file.extension.png] to [file.png]
             (when (file-exists-p qlmanage-thumbnail-file)
               (rename-file qlmanage-thumbnail-file thumbnail-file))
             (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
               (org-link-beautify--notify-generate-thumbnail-failed audio-file thumbnail-file))))
          ("audiowaveform"
           (start-process proc-name proc-buffer
                          "audiowaveform" "-i" audio-file "-o" thumbnail-file)
           (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
             (org-link-beautify--notify-generate-thumbnail-failed audio-file thumbnail-file)))
          ;; TODO: use ffmpeg to generate audio wave form preview image.
          ;; ("ffmpeg"
          ;;  )
          )))
    (org-link-beautify--add-overlay-info thumbnail-file start end)
    (org-link-beautify--add-text-property-marker start end)
    (org-link-beautify--add-keymap start end)
    ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
    (if (file-exists-p thumbnail-file)
        (org-link-beautify--display-thumbnail thumbnail-file thumbnail-size start end)
      'error)))

;;; TEST: eval bellowing code on [[attachment:]] link.
;; (let ((type "attachment") ; or "file"
;;       (path (org-element-property :path (org-element-link-parser)))
;;       (begin (org-element-begin (org-element-context)))
;;       (end (org-element-end (org-element-context))))
;;   (org-link-beautify--preview-audio type path begin end))

(defvar org-link-beautify--url-screenshot-cmd
  (cond
   ((executable-find "webkit2png") "webkit2png")
   ((executable-find "monolith") "monolith"))
  "Find available URL web page screenshot command.")

(defun org-link-beautify--preview-url-archive (url cmd-list)
  "Construct process to run for URL to archive through CMD-LIST."
  (let* ((process-name (format "org-link-beautify--url-screenshot %s" url))
         (process-buffer (format " *org-link-beautify--url-screenshot %s*" url))
         (proc (get-process process-name)))
    (unless proc
      (eval `(start-process ,process-name ,process-buffer ,@cmd-list)))))

(defun org-link-beautify--preview-url (type path start end)
  "Preview web page PATH of TYPE with screenshot between START and END."
  (let* ((url (concat type ":" path))
         (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path (buffer-file-name)))
         (thumbnail-filename (format "org-link-beautify URL screenshot %s.png" (time-stamp-string)))
         (thumbnail-file (expand-file-name thumbnail-filename thumbnails-dir))
         (thumbnail-size (or org-link-beautify-url-preview-size 512)))
    ;; DEBUG: (message url) ; https://elpa.gnu.org/packages/kiwix.html (with `type')
    (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
    (unless (file-exists-p thumbnail-file)
      (pcase org-link-beautify--url-screenshot-cmd
        ;; TODO:
        ("webkit2png"
         (org-link-beautify--preview-url-archive url `("webkit2png" ,url "-o" ,thumbnail-file))
         (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
           (org-link-beautify--notify-generate-thumbnail-failed url thumbnail-file)))
        ("monolith"
         (let* ((thumbnail-file-html (concat (file-name-sans-extension thumbnail-file) ".html"))
                (thumbnail-file thumbnail-file-html)
                (cmd-list `("monolith" "--no-audio" "--no-video" ,url "--output" ,thumbnail-file-html)))
           (message "[org-link-beautify] URL screenshot archive with 'monolith' for %s" url)
           (org-link-beautify--preview-url-archive url cmd-list)
           ;; TODO: convert archived web page to screenshot.
           (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
             (org-link-beautify--notify-generate-thumbnail-failed url thumbnail-file))))))
    (org-link-beautify--add-text-property-marker start end)
    (org-link-beautify--add-keymap start end)
    ;; display thumbnail-file only when it exist, otherwise it will break org-mode buffer fontification.
    (if (file-exists-p thumbnail-file)
        ;; FIXME: can't display thumbnail image of HTML archive file.
        (org-link-beautify--display-thumbnail thumbnail-file thumbnail-size start end)
      'error)))

(defun org-link-beautify--return-icon (type path extension &optional link-element)
  "Return icon for the link PATH smartly based on TYPE, EXTENSION, etc."
  ;; Fix elisp compiler warning: Unused lexical argument `link-element'.
  (ignore link-element)
  ;; (message "DEBUG: (type) %s" type)
  ;; (message "DEBUG: (path) %s" path)
  ;; (message "DEBUG: (link-element) %s" link-element)
  (pcase type
    ("file"
     ;; DEBUG:
     ;; (message "[DEBUG] type: %s, path: %s, extension: %s" type path extension)
     (cond
      ;; FIXME: avoid other remote link like /docker: caused `file-exists-p' suspend Emacs.
      ;; make sure the link prefix is `file'.
      ;; ((not (file-exists-p (expand-file-name path))) ; not exist file!
      ;;  (nerd-icons-codicon "nf-cod-error" :face 'nerd-icons-red-alt))
      ((file-directory-p (expand-file-name path))          ; directory exist?
       (nerd-icons-octicon "nf-oct-file_directory" :face (org-link-beautify--warning-face-p path)))
      ((file-remote-p path)             ; remote file
       (nerd-icons-codicon "nf-cod-remote_explorer" :face 'nerd-icons-lred))
      ;; special file types
      ;; ((equal (file-name-extension path) "ipynb")
      ;;  (nerd-icons-icon-for-file "file.ipynb"))
      ;; other file types
      (t (nerd-icons-icon-for-file path))))
    ("file+sys" (nerd-icons-mdicon "nf-md-file_cog_outline" :face 'nerd-icons-lred))
    ("file+emacs" (nerd-icons-icon-for-mode 'emacs-lisp-mode))
    ("http" (nerd-icons-icon-for-url (concat "http:" path)))
    ("https" (nerd-icons-icon-for-url (concat "https:" path)))
    ("ftp" (nerd-icons-mdicon "nf-md-file_link_outline" :face 'nerd-icons-orange))
    ("telnet" (nerd-icons-mdicon "nf-md-link_box_variant_outline" :face 'nerd-icons-blue))
    ;; Org mode internal link types
    ("custom-id" (nerd-icons-mdicon "nf-md-text_box_search_outline" :face 'nerd-icons-blue))
    ("id" (nerd-icons-mdicon "nf-md-text_search" :face 'nerd-icons-blue))
    ("coderef" (nerd-icons-codicon "nf-cod-references" :face 'nerd-icons-cyan))
    ("attachment" (let ((full-path (expand-file-name path (org-attach-dir))))
                    (if (file-exists-p full-path)
                        (nerd-icons-mdicon "nf-md-attachment" :face 'nerd-icons-lorange)
                      (nerd-icons-mdicon "nf-md-attachment" :face '(:foreground "red")))))
    ("elisp" (nerd-icons-icon-for-file "file.el"))
    ("eshell" (nerd-icons-icon-for-mode 'eshell-mode))
    ("shell" (nerd-icons-icon-for-mode 'shell-mode))
    ("man" (nerd-icons-mdicon "nf-md-file_document_outline" :face 'nerd-icons-lblue))
    ("info" (nerd-icons-mdicon "nf-md-information_outline" :face 'nerd-icons-lblue))
    ("help" (nerd-icons-mdicon "nf-md-help_circle_outline" :face 'nerd-icons-lblue))
    ;; org-ref link types
    ("cite" (nerd-icons-codicon "nf-cod-references" :face 'nerd-icons-cyan))
    ;; Org mode external link types
    ("eaf" (nerd-icons-mdicon "nf-md-apps" :face 'nerd-icons-blue)) ; emacs-application-framework
    ("eww" (nerd-icons-icon-for-mode 'eww-mode))
    ("chrome" (nerd-icons-mdicon "nf-md-google_chrome" :face 'nerd-icons-lorange))
    ("mu4e" (nerd-icons-mdicon "nf-md-email_search_outline" :face 'nerd-icons-blue))
    ("news" (nerd-icons-mdicon "nf-md-newspaper_variant_outline" :face 'nerd-icons-dgreen))
    ("git" (nerd-icons-mdicon "nf-md-git" :face 'nerd-icons-lred))
    ("orgit" (nerd-icons-faicon "nf-fa-git" :face 'nerd-icons-red))
    ("orgit-rev" (nerd-icons-devicon "nf-dev-git_commit" :face 'nerd-icons-silver))
    ("orgit-log" (nerd-icons-octicon "nf-oct-diff" :face 'nerd-icons-silver))
    ("pdf" (nerd-icons-icon-for-file "file.pdf"))
    ("nov" (nerd-icons-icon-for-file "file.epub")) ; for Emacs package "nov.el" link type `nov:'
    ("grep" (nerd-icons-mdicon "nf-md-selection_search" :face 'nerd-icons-green))
    ("occur" (nerd-icons-mdicon "nf-md-selection_multiple" :face 'nerd-icons-green))
    ("rss" (nerd-icons-mdicon "nf-md-rss" :face 'nerd-icons-lorange))
    ("elfeed" (nerd-icons-mdicon "nf-md-rss" :face 'nerd-icons-green))
    ("wikipedia" (nerd-icons-mdicon "nf-md-wikipedia" :face 'nerd-icons-dsilver))
    ("mailto" (nerd-icons-mdicon "nf-md-email_send_outline" :face 'nerd-icons-lblue))
    ("irc" (nerd-icons-mdicon "nf-md-chat" :face 'nerd-icons-blue-alt))
    ("magnet" (nerd-icons-mdicon "nf-md-magnet" :face 'nerd-icons-blue-alt))
    ("ref" (nerd-icons-codicon "nf-cod-references" :face 'nerd-icons-blue))
    ("doi" (nerd-icons-mdicon "nf-md-file_document_plus_outline" :face 'nerd-icons-green))
    ("org-contact" (nerd-icons-mdicon "nf-md-contacts_outline" :face 'nerd-icons-purple-alt))
    ("org-bookmark" (nerd-icons-mdicon "nf-md-bookmark_check_outline" :face 'nerd-icons-blue-alt))
    ("video" (nerd-icons-faicon "nf-fa-file_video_o" :face 'nerd-icons-blue))
    ("audio" (nerd-icons-faicon "nf-fa-file_audio_o" :face 'nerd-icons-blue))
    ;; org-media-note link types
    ("videocite" (nerd-icons-faicon "nf-fa-file_video_o" :face 'nerd-icons-blue-alt))
    ("audiocite" (nerd-icons-faicon "nf-fa-file_audio_o" :face 'nerd-icons-blue-alt))
    ("epub" (nerd-icons-mdicon "nf-md-book_open_page_variant_outline" :face 'nerd-icons-blue-alt))
    ("javascript" (nerd-icons-mdicon "nf-md-language_javascript" :face 'nerd-icons-yellow))
    ("js" (nerd-icons-mdicon "nf-md-language_javascript" :face 'nerd-icons-yellow))
    ("vscode" (nerd-icons-mdicon "nf-md-microsoft_visual_studio_code" :face 'nerd-icons-blue-alt))
    ("macappstores" (nerd-icons-mdicon "nf-md-apple" :face 'nerd-icons-silver))
    
    ;; `org-element-context' will return "fuzzy" type when link not recognized.
    ("fuzzy"
     ;; Org internal [[reference][reference]] -> NOT supported by:
     ;; `(org-link-set-parameters link-type :activate-func ...)'
     ;;
     ;; (link (:standard-properties [584419 nil 584470 584517 584519 0 nil nil nil nil nil nil ...] :type "fuzzy" :type-explicit-p nil :path "defcustom org-contacts-identity-properties-list" :format bracket :raw-link "defcustom org-contacts-identity-properties-list" ...))

     (when-let ((_ (string-match "\\([^:]*\\):\\(.*\\)" path))
                (real-type (match-string 1 path))) ; extract the "real" link type for "fuzzy" type in :path.
       (cond
        ;; FIXME:
        ;; ((string-equal path link-element)
        ;;  (nerd-icons-faicon "nf-fa-searchengin" :face 'nerd-icons-blue-alt))
        (t
         (message "[org-link-beautify] link type not supported, add PR for this link type.
 type: %s, path: %s, extension: %s, link-element: %s" type path extension link-element)
         (nerd-icons-mdicon "nf-md-progress_question" :face 'nerd-icons-lyellow)))))
    (_
     ;; DEBUG
     (message "[org-link-beautify] link type not supported, add PR for this link type.
 type: %s, path: %s, extension: %s, link-element: %s" type path extension link-element)
     ;; handle when returned link type is `nil'.
     (nerd-icons-mdicon "nf-md-progress_question" :face 'nerd-icons-lyellow))))

(defface org-link-beautify-link-decorator-face
  `((t :foreground ,(color-lighten-name (face-foreground 'shadow) 2)
       :underline nil))
  "Face for org-link-beautify link decorator."
  :group 'org-link-beautify)

(defface org-link-beautify-link-description-face
  '((t :inherit 'org-link))
  "Face for org-link-beautify link description."
  :group 'org-link-beautify)

(defface org-link-beautify-link-icon-face
  '((t :foreground "gray" :height 95))
  "Face for org-link-beautify link icon."
  :group 'org-link-beautify)

(defun org-link-beautify--display-icon (start end description icon)
  "Display ICON for link on START and END with DESCRIPTION."
  (put-text-property
   start end
   'display
   (concat
    (propertize "[" 'face 'org-link-beautify-link-decorator-face)
    (propertize description 'face 'org-link-beautify-link-description-face)
    (propertize "]" 'face 'org-link-beautify-link-decorator-face)
    (propertize "⌈" 'face 'org-link-beautify-link-decorator-face)
    (propertize icon 'face `(:inherit ,(or (plist-get (get-text-property 0 'face icon) :inherit)
                                           'org-link-beautify-link-icon-face)
                                      :underline nil))
    (propertize "⌋" 'face 'org-link-beautify-link-decorator-face))))

(defun org-link-beautify--display-not-exist (start end description icon)
  "Display error color and ICON on START and END with DESCRIPTION."
  (put-text-property
   start end
   'display
   (concat
    (propertize "[" 'face '(:inherit nil :underline nil :foreground "black"))
    (propertize description 'face '(:underline t :foreground "red" :strike-through t))
    (propertize "]" 'face '(:inherit nil :underline nil :foreground "black"))
    (propertize "(" 'face '(:inherit nil :underline nil :foreground "black"))
    (propertize icon 'face '(:inherit nil :underline nil :foreground "orange red"))
    (propertize ")" 'face '(:inherit nil :underline nil :foreground "black")))))

(defun org-link-beautify-display (start end path bracket-p)
  "Display icon for the link type based on PATH from START to END."
  ;; DEBUG:
  ;; (message
  ;;  (format "start: %s, end: %s, path: %s, bracket-p: %s" start end path bracket-p))
  ;; detect whether link is normal, skip other links in special places.
  (let ((link-element (org-link-beautify--get-element start))
        ;; DEBUG:
        ;; (link-element-debug (message link-element))
        )
    (when (eq (car link-element) 'link)
      (save-match-data
        (let* ((raw-link (org-element-property :raw-link link-element))
               ;; DEBUG:
               ;; (raw-link-debug (print raw-link))
               (type (org-element-property :type link-element))
               ;; DEBUG:
               ;; (type-debug (message path))
               (path (org-element-property :path link-element))
               ;; DEBUG:
               ;; (type-debug (message type))
               (extension (or (file-name-extension (org-link-unescape path)) "txt"))
               ;; the search part behind link separator "::"
               (search-option (org-element-property :search-option link-element))
               ;; DEBUG: (ext-debug (message extension))
               (description (or (and (org-element-property :contents-begin link-element) ; in raw link case, it's nil
                                     (buffer-substring-no-properties
                                      (org-element-property :contents-begin link-element)
                                      (org-element-property :contents-end link-element)))
                                ;; when description not exist, use raw link for raw link case.
                                raw-link))
               ;; DEBUG: (desc-debug (print description))
               (icon (org-link-beautify--return-icon type path extension link-element))
               ;; DEBUG:
               ;; (icon-debug (print icon))
               )
          ;; Fix elisp compiler warning: Unused lexical argument `bracket-p'.
          (ignore bracket-p)
          (cond
           ;; image thumbnail preview
           ;; [[file:/path/to/image.jpg]]
           ((and org-link-beautify-image-preview
                 (member type '("file" "attachment"))
                 (cond
                  ((member type '("file" "image"))
                   (file-exists-p path))
                  ((string-equal type "attachment")
                   (file-exists-p (expand-file-name (org-link-unescape path) (org-attach-dir)))))
                 (member extension org-link-beautify-image-preview-list))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> image file")
            (when (eq (org-link-beautify--preview-image type path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; video thumbnail preview
           ;; [[file:/path/to/video.mp4]]
           ;; [[video:/path/to/video.mp4]]
           ((and org-link-beautify-video-preview
                 (member type '("file" "video" "attachment"))
                 (cond
                  ((member type '("file" "video"))
                   (file-exists-p path))
                  ((string-equal type "attachment")
                   (file-exists-p (expand-file-name (org-link-unescape path) (org-attach-dir)))))
                 (member extension org-link-beautify-video-preview-list))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> video file")
            (when (eq (org-link-beautify--preview-video type path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; subtitle, closed caption preview
           ;; [[file:/path/to/subtitle.ass]]
           ;; [[file:/path/to/subtitle.srt]]
           ((and org-link-beautify-subtitle-preview
                 (member type '("file"))
                 (file-exists-p path)
                 (member extension org-link-beautify-subtitle-preview-list))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> subtitle file")
            (when (eq (org-link-beautify--preview-subtitle path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; audio wave form preview
           ;; [[file:/path/to/audio.mp3]]
           ;; [[audio:/path/to/audio.mp3]]
           ((and org-link-beautify-audio-preview
                 (member type '("file" "audio" "attachment"))
                 (cond
                  ((member type '("file" "audio"))
                   (file-exists-p path))
                  ((string-equal type "attachment")
                   (file-exists-p (expand-file-name (org-link-unescape path) (org-attach-dir)))))
                 (member extension org-link-beautify-audio-preview-list))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> audio file")
            (when (eq (org-link-beautify--preview-audio type path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; PDF file preview
           ;; [[file:/path/to/filename.pdf]]
           ;; [[pdf:/path/to/filename.pdf::15]]
           ;; [[pdfview:/path/to/filename.pdf::15]]
           ((and org-link-beautify-pdf-preview
                 (or (and (equal type "file") (string= extension "pdf"))
                     (member type '("pdf" "pdfview" "docview" "eaf")))
                 (file-exists-p path))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> PDF file")
            ;; (message "org-link-beautify: PDF file previewing [%s], link-type: [%s], search-option: [%s] (type: %s)," path type search-option (type-of search-option))
            (when (eq (org-link-beautify--preview-pdf
                       (if (equal type "eaf") (replace-regexp-in-string "pdf::" "" path) path)
                       start end
                       search-option)
                      'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; EPUB file cover preview
           ((and org-link-beautify-epub-preview
                 (equal type "file")
                 (file-exists-p path)
                 (string= extension "epub"))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> epub file")
            (when (eq (org-link-beautify--preview-epub path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))

           ;; kindle ebook file cover preview
           ((and org-link-beautify-kindle-preview
                 (equal type "file")
                 (file-exists-p path)
                 (member extension '("mobi" "azw3")))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> epub file")
            (when (eq (org-link-beautify--preview-kindle path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; CDisplay Archived Comic Book Formats cover preview
           ((and org-link-beautify-comic-preview
                 (equal type "file")
                 (file-exists-p path)
                 (member extension '("cbr" "cbz" "cb7" "cba" "cbt")))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> comic file")
            (when (eq (org-link-beautify--preview-comic path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))

           ;; FictionBook2 (.fb2, .fb2.zip) file cover preview
           ((and org-link-beautify-fictionbook2-preview
                 (equal type "file")
                 (file-exists-p path)
                 (or (string= extension "fb2")
                     (string= extension "zip")))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> FictionBook2 (.fb2, .fb2.zip) file")
            (when (eq (org-link-beautify--preview-fictionbook2 path start end) 'error)
              ;; Display icon if thumbnail not available.
              (org-link-beautify--add-text-property-marker start end)
              (org-link-beautify--add-keymap start end)
              (org-link-beautify--display-icon start end description icon)))
           
           ;; text content preview
           ((and org-link-beautify-text-preview
                 (equal type "file")
                 (file-exists-p path)
                 (member extension org-link-beautify-text-preview-list))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> text file")
            (org-link-beautify--preview-text path start end))
           
           ;; compressed archive file preview
           ((and org-link-beautify-archive-preview
                 (equal type "file")
                 (file-exists-p path)
                 (member extension (mapcar 'car org-link-beautify-archive-preview-command-alist)))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> archive file")
            ;; (if (null extension)
            ;;     (user-error "[org-link-beautify] archive file preview> extension: %s" extension))
            ;; (message "[org-link-beautify] archive file preview> path: %s" path)
            (let ((command (cdr (assoc extension org-link-beautify-archive-preview-command-alist))))
              (org-link-beautify--preview-archive path command start end)))
           
           ;; file does not exist
           ((and (equal type "file") (not (file-exists-p path)))
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> file")
            ;; (message path)
            (org-link-beautify--add-text-property-marker start end)
            (org-link-beautify--display-not-exist start end description icon))
           
           ;; URL
           ((and org-link-beautify-url-preview (org-url-p (concat type path)))
            (org-link-beautify--preview-url type path start end))
           
           ;; general icons
           (t
            ;; DEBUG:
            ;; (user-error "[org-link-beautify] cond -> t")
            ;; (message "start: %d, end: %d, description: %s, icon: %s" start end description icon)
            (org-link-beautify--add-text-property-marker start end)
            (org-link-beautify--add-keymap start end)
            (org-link-beautify--display-icon start end description icon))))))))

(defun org-link-beautify-display-async (start end path bracket-p)
  "Run `org-link-beautify-display' in async thread to avoid suspend Emacs."
  ;; DEBUG: (message "[org-link-beautify] running preview function in async thread for %s" path)
  (make-thread
   (lambda () (org-link-beautify-display start end path bracket-p))
   (make-temp-name "org-link-beautify-display-thread-")))

;;; hook on headline expand
(defun org-link-beautify-headline-cycle (&optional state)
  "Function to be executed on `org-cycle-hook' STATE."
  (pcase state
    ('subtree (org-link-beautify--refontify state))
    ('children (org-link-beautify--refontify state))
    ('folded (org-link-beautify--clear state))
    ('overview (org-link-beautify--clear state))
    (_ (ignore))))

(defun org-link-beautify--refontify (&optional state)
  "Re-fontify current entry when STATE is children or subtree with `jit-lock-refontify'."
  ;; replace the whole Org buffer font-lock function `org-restart-font-lock'
  ;; with a lightweight `jit-lock-refontify' current headline scope only
  ;; font-lock function.
  (when (or (eq state 'children) (eq state 'subtree))
    (org-link-beautify--subtree-scope-wrap
     (jit-lock-refontify begin end))))

;;; toggle org-link-beautify text-properties
(defun org-link-beautify--clear-text-properties (&optional begin end)
  "Clear all org-link-beautify text-properties between BEGIN and END.
If BEGIN and END is ommited, the default value is `point-min' and `point-max'."
  (let ((point (or begin (point-min)))
        (bmp (buffer-modified-p)))
    (while (setq point (next-single-property-change point 'display))
      (when (and (< point (or end (point-max)))
                 (get-text-property point 'display)
                 (eq (get-text-property point 'type) 'org-link-beautify))
        (remove-text-properties
	     point (setq point (next-single-property-change point 'display))
	     '(display t))))
    (set-buffer-modified-p bmp)))

(defun org-link-beautify--clear (&optional state)
  "Clear the text-properties of `org-link-beautify' under STATE headline subtree."
  (cond
   ((eq state 'folded)
    ;; clear in current folded headline
    (org-link-beautify--subtree-scope-wrap
     (org-link-beautify--clear-text-properties begin end)))
   ((eq state 'overview)
    ;; clear whole buffer
    (org-link-beautify--clear-text-properties))
   (t
    ;; clear whole buffer when minor mode disabled.
    (org-link-beautify--clear-text-properties))))

(defvar org-link-beautify-keymap (make-sparse-keymap))

(defun org-link-beautify--add-keymap (start end)
  "Add keymap on link text-property. between START and END."
  (put-text-property start end 'keymap org-link-beautify-keymap))

(defun org-link-beautify-open-at-point ()
  "Execute `org-open-at-point' only in `org-mode'."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-open-at-point)))

(define-key org-link-beautify-keymap (kbd "RET") 'org-link-beautify-open-at-point)
(define-key org-link-beautify-keymap [mouse-1] 'org-link-beautify-open-at-point)
(define-key org-link-beautify-keymap (kbd "<mouse-1>") 'org-link-beautify-open-at-point)

(defun org-link-beautify-copy-file-to-clipboard (file)
  "Copy the FILE on path to clipboard.
The argument FILE must be the absolute path."
  (cl-case system-type
    (darwin
     (do-applescript
      (format "tell app \"Finder\" to set the clipboard to ( POSIX file \"%s\" )" file)))
    ;; TODO:
    (gnu/linux )
    ;; TODO:
    (windows-nt ))
  (message "Copied file [%s] to system clipboard."
           (string-truncate-left file (/ (window-width) 2))))

(defun org-link-beautify-copy-file (&optional args)
  "Copy the Org link file at point in ARGS."
  (interactive "P")
  (when (derived-mode-p 'org-mode)
    (if (or (region-active-p) mark-active)
        (let ((region-text (buffer-substring-no-properties
                            (region-beginning) (region-end))))
          (kill-new region-text)
          (deactivate-mark))
      (let ((element (org-element-context)))
        (if (and (eq (car element) 'link)
                 (string-equal (org-element-property :type element) "file"))
            (let ((file-path (expand-file-name (org-element-property :path element))))
              (org-link-beautify-copy-file-to-clipboard file-path))
          (message "[org-link-beautify] No action executed on link."))))))

(define-key org-link-beautify-keymap (kbd "M-w") 'org-link-beautify-copy-file)

(defun org-link-beautify-display-qrcode-for-url (&optional args)
  "Display QR code for Org link at point in new buffer in ARGS."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (if-let ((url (org-element-property-raw :raw-link (org-element-context))))
        (if (require 'qrencode nil t)
            (qrencode-string url)
          (package-install 'qrencode)
          (qrencode-string url))
      (if (or (region-active-p) mark-active)
          (org-fill-paragraph t t)
        (org-fill-paragraph)))))

(define-key org-link-beautify-keymap (kbd "M-q") 'org-link-beautify-display-qrcode-for-url)

(defun org-link-beautify-goto-file-in-dired ()
  "Open Dired and goto the link file position."
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

;;;###autoload
(defun org-link-beautify-enable ()
  "Enable `org-link-beautify'."
  (when (display-graphic-p)
    (dolist (link-type (mapcar #'car org-link-parameters))
      (if org-link-beautify-async-preview
          (org-link-set-parameters link-type :activate-func #'org-link-beautify-display-async)
        (org-link-set-parameters link-type :activate-func #'org-link-beautify-display)))
    (add-hook 'org-cycle-hook #'org-link-beautify-headline-cycle)
    (org-restart-font-lock)
    ;; Support mouse left click on image to open link.
    (make-local-variable 'image-map)
    (define-key image-map (kbd "<mouse-1>") 'org-open-at-point)))

;;;###autoload
(defun org-link-beautify-disable ()
  "Disable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (org-link-set-parameters link-type :activate-func t))
  (remove-hook 'org-cycle-hook #'org-link-beautify-headline-cycle)
  (org-link-beautify--clear))

;;;###autoload
(define-minor-mode org-link-beautify-mode
  "A minor mode to beautify Org Mode links with icons, and inline preview etc."
  :group 'org-link-beautify
  :global nil
  :init-value nil
  :lighter nil
  :keymap org-link-beautify-keymap
  (if org-link-beautify-mode
      (org-link-beautify-enable)
    (org-link-beautify-disable)))



(provide 'org-link-beautify)

;;; org-link-beautify.el ends here
