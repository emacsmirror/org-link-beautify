;;; org-link-beautify.el --- Beautify Org Links -*- lexical-binding: t; -*-

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "29.1") (org "9.7.14") (nerd-icons "0.0.1"))
;; Version: 2.0.0
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

(require 'org) ; including `ol'
(require 'org-element)
(require 'org-element-ast)
(require 'cl-lib)
(require 'color)

(require 'nerd-icons)
(require 'qrencode)

;; (require 'fb2-reader)
(declare-function fb2-reader--create-image "fb2-reader" (data type &rest props))
(declare-function fb2-reader--extract-image-data "fb2-reader" (book attributes &optional tags))
(declare-function fb2-reader--get-cover "fb2-reader" (book))
(declare-function fb2-reader-parse-file-as-html "fb2-reader" (file))
(declare-function fb2-reader-parse-file-as-xml "fb2-reader" (file))

;;; Customization
(defgroup org-link-beautify nil
  "Customize group of `org-link-beautify-mode'."
  :prefix "org-link-beautify-"
  :group 'org)

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

(defcustom org-link-beautify-enable-debug-p nil
  "Whether enable org-link-beautify print debug info."
  :type 'boolean
  :safe #'booleanp)


;;; overlay keymap keybindings

(defvar org-link-beautify-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-map) ; inherit `image-map' keybindings on preview thumbnail image.
    map))

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

(defun org-link-beautify--copy-file-to-clipboard (file)
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
  (message "Copied file [%s] to system clipboard." (string-truncate-left file (/ (window-width) 2))))

(defun org-link-beautify-action-copy-file (&optional args)
  "Action of copying the Org link file at point with optional ARGS."
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
              (org-link-beautify--copy-file-to-clipboard file-path))
          (message "[org-link-beautify] No action executed on link."))))))

(define-key org-link-beautify-keymap (kbd "M-w") #'org-link-beautify-action-copy-file)

(defun org-link-beautify-action-qrcode-for-url (&optional args)
  "Action of displaying QR code for Org link at point in new buffer in ARGS."
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

(define-key org-link-beautify-keymap (kbd "M-q") 'org-link-beautify-action-qrcode-for-url)

;;; helper functions

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

(defun org-link-beautify--notify-generate-thumbnail-failed (source-file thumbnail-file)
  "Notify that generating THUMBNAIL-FILE for SOURCE-FILE failed."
  (message "[org-link-beautify] For file %s.\nCreate thumbnail %s failed." source-file thumbnail-file))

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
  (shell-command-to-string
   (format "%s %s" org-link-beautify-python-interpreter javascript-script-file)))

(defun org-link-beautify--javascript-command-to-string (&rest code-lines)
  "Run JavaScript CODE-LINES through shell command."
  (shell-command-to-string
   (concat "node --eval "
           ;; solve double quote character issue.
           "\"" (string-replace "\"" "\\\"" (string-join code-lines "\n")) "\"")))

;; TEST:
;; (org-link-beautify--javascript-command-to-string
;;  "console.log(\"hello, world!\");"
;;  "console.log(1 + 3);")

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
      ("file" (if-let (extension (file-name-extension path))
                  (nerd-icons-icon-for-extension extension)
                (nerd-icons-icon-for-dir path)))
      ("attachment" (if-let (extension (file-name-extension path))
                        (nerd-icons-icon-for-extension extension)
                      (nerd-icons-icon-for-dir path)))
      ("http" (nerd-icons-icon-for-url (concat type ":" path)))
      ("https" (nerd-icons-icon-for-url (concat type ":" path)))
      ;; Org mode internal link types
      ("custom-id" (nerd-icons-mdicon "nf-md-text_box_search_outline" :face 'nerd-icons-blue))
      ("id" (nerd-icons-mdicon "nf-md-text_search" :face 'nerd-icons-blue))
      ("coderef" (nerd-icons-codicon "nf-cod-references" :face 'nerd-icons-cyan))
      ("elisp" (nerd-icons-icon-for-file "file.el"))
      ("eshell" (nerd-icons-icon-for-mode 'eshell-mode))
      ("shell" (nerd-icons-icon-for-mode 'shell-mode))
      ("man" (nerd-icons-mdicon "nf-md-file_document_outline" :face 'nerd-icons-lblue))
      ("woman" (nerd-icons-mdicon "nf-md-file_document_outline" :face 'nerd-icons-blue-alt))
      ("info" (nerd-icons-mdicon "nf-md-information_outline" :face 'nerd-icons-lblue))
      ("help" (nerd-icons-mdicon "nf-md-help_circle_outline" :face 'nerd-icons-lblue))
      ("shortdoc" (nerd-icons-mdicon "nf-md-file_link" :face 'nerd-icons-blue))
      ;; org-ref link types
      ("cite" (nerd-icons-codicon "nf-cod-references" :face 'nerd-icons-cyan))
      ;; Org mode external link types
      ("eaf" (nerd-icons-mdicon "nf-md-apps" :face 'nerd-icons-blue)) ; emacs-application-framework
      ("eww" (nerd-icons-icon-for-mode 'eww-mode)) ; EWW
      ("chrome" (nerd-icons-mdicon "nf-md-google_chrome" :face 'nerd-icons-lorange))
      ("edge" (nerd-icons-mdicon "nf-md-microsoft_edge" :face 'nerd-icons-green))
      ("mu4e" (nerd-icons-mdicon "nf-md-email_search_outline" :face 'nerd-icons-blue))
      ("news" (nerd-icons-mdicon "nf-md-newspaper_variant_outline" :face 'nerd-icons-dgreen))
      ("git" (nerd-icons-mdicon "nf-md-git" :face 'nerd-icons-lred))
      ("orgit" (nerd-icons-faicon "nf-fa-git" :face 'nerd-icons-red))
      ("orgit-rev" (nerd-icons-devicon "nf-dev-git_commit" :face 'nerd-icons-silver))
      ("orgit-log" (nerd-icons-octicon "nf-oct-diff" :face 'nerd-icons-silver))
      ("pdf" (nerd-icons-icon-for-file "file.pdf"))
      ("epub" (nerd-icons-mdicon "nf-md-book_open_page_variant_outline" :face 'nerd-icons-blue-alt))
      ("nov" (nerd-icons-icon-for-file "file.epub")) ; for Emacs package "nov.el" link type `nov:'
      ("grep" (nerd-icons-mdicon "nf-md-selection_search" :face 'nerd-icons-green))
      ("occur" (nerd-icons-mdicon "nf-md-selection_multiple" :face 'nerd-icons-green))
      ("rss" (nerd-icons-mdicon "nf-md-rss" :face 'nerd-icons-lorange))
      ("elfeed" (nerd-icons-mdicon "nf-md-rss" :face 'nerd-icons-green))
      ("wikipedia" (nerd-icons-mdicon "nf-md-wikipedia" :face 'nerd-icons-dsilver))
      ("mailto" (nerd-icons-mdicon "nf-md-email_send_outline" :face 'nerd-icons-lblue))
      ("irc" (nerd-icons-mdicon "nf-md-chat" :face 'nerd-icons-blue-alt))
      ("wechat" (nerd-icons-mdicon "nf-md-wechat" :face 'nerd-icons-green))
      ("magnet" (nerd-icons-mdicon "nf-md-magnet" :face 'nerd-icons-blue-alt))
      ("ref" (nerd-icons-codicon "nf-cod-references" :face 'nerd-icons-blue))
      ("doi" (nerd-icons-mdicon "nf-md-file_document_plus_outline" :face 'nerd-icons-green))
      ("org-contact" (nerd-icons-mdicon "nf-md-contacts_outline" :face 'nerd-icons-purple-alt))
      ("org-bookmark" (nerd-icons-mdicon "nf-md-bookmark_check_outline" :face 'nerd-icons-blue-alt))
      ("org-ql-search" (nerd-icons-mdicon "nf-md-text_box_search_outline" :face 'nerd-icons-blue-alt))
      ("video" (nerd-icons-faicon "nf-fa-file_video_o" :face 'nerd-icons-blue))
      ("audio" (nerd-icons-faicon "nf-fa-file_audio_o" :face 'nerd-icons-blue))
      ;; org-media-note link types
      ("videocite" (nerd-icons-faicon "nf-fa-file_video_o" :face 'nerd-icons-blue-alt))
      ("audiocite" (nerd-icons-faicon "nf-fa-file_audio_o" :face 'nerd-icons-blue-alt))
      ("javascript" (nerd-icons-mdicon "nf-md-language_javascript" :face 'nerd-icons-yellow))
      ("js" (nerd-icons-mdicon "nf-md-language_javascript" :face 'nerd-icons-yellow))
      ("vscode" (nerd-icons-mdicon "nf-md-microsoft_visual_studio_code" :face 'nerd-icons-blue-alt)) ; Visual Studio Code
      ("macappstores" (nerd-icons-mdicon "nf-md-apple" :face 'nerd-icons-silver)) ; Mac App Store
      
      ("fuzzy"
       ;; Org internal [[reference][reference]] -> NOT supported by:
       ;; `(org-link-set-parameters link-type :activate-func ...)'
       ;;
       ;; (link (:standard-properties [584419 nil 584470 584517 584519 0 nil nil nil nil nil nil ...] :type "fuzzy" :type-explicit-p nil :path "defcustom org-contacts-identity-properties-list" :format bracket :raw-link "defcustom org-contacts-identity-properties-list" ...))
       
       (when-let* ((_ (string-match "\\([^:]*\\):\\(.*\\)" path))
                   (real-type (match-string 1 path))) ; extract the "real" link type for "fuzzy" type in :path.
         (cond
          (t
           (message "[org-link-beautify] link type not supported, add PR for this link type.
type: %s, path: %s, extension: %s, link-element: %s" type path extension link)
           (nerd-icons-mdicon "nf-md-progress_question" :face 'nerd-icons-lyellow)))))
      (_
       (message "[org-link-beautify] link type not supported, add PR for this link type.
type: %s, path: %s, extension: %s, link-element: %s" type path extension link)
       ;; handle when returned link type is `nil'.
       (nerd-icons-mdicon "nf-md-progress_question" :face 'nerd-icons-lyellow)))))

(defun org-link-beautify-iconify (ov path link)
  "Iconify PATH over OV overlay position for LINK element."
  (when-let* ((begin (org-element-begin link))
              (end (org-element-end link))
              (description (org-link-beautify--get-link-description begin))
              (icon (org-link-beautify--return-icon ov path link)))
    (overlay-put ov
                 'display (concat
                           (propertize "[" 'face `(:inherit nil :underline nil :foreground ,(color-lighten-name (face-foreground 'shadow) 2)))
                           (propertize description 'face (org-link-beautify--return-warning-face ov path link))
                           (propertize "]" 'face `(:inherit nil :underline nil :foreground ,(color-lighten-name (face-foreground 'shadow) 2)))))
    (overlay-put ov
                 'after-string (concat
                                (propertize "[" 'face '(:inherit nil :underline nil :foreground "orange"))
                                icon
                                (propertize "]" 'face '(:inherit nil :underline nil :foreground "orange"))))))

;;; Preview file: link type

(defun org-link-beautify-preview-file (ov path link)
  "Preview file of PATH over OV overlay position for LINK element.
This function will apply file type function based on file extension."
  (let ((extension (file-name-extension path)))
    (cond
     ((null extension) ; no file extension, it's directory.
      (org-link-beautify-iconify ov path link))
     ((string-match-p (image-file-name-regexp) path) ; `org-link-beautify-image-preview-list'
      (org-link-beautify-preview-file-image ov path link))
     ((string-equal extension "pdf")
      (org-link-beautify-preview-file-pdf ov path link))
     ((string-equal extension "epub")
      (org-link-beautify-preview-file-epub ov path link))
     ((string-match-p "\\(mobi\\|azw3\\)" extension)
      (org-link-beautify-preview-file-kindle ov path link))
     ((string-match-p "\\.fb2\\(\\.zip\\)?" path)
      (org-link-beautify-preview-file-fictionbook2 ov path link))
     ((member extension org-link-beautify-comic-preview-list)
      (org-link-beautify-preview-file-comic ov path link))
     ((member extension org-link-beautify-video-preview-list)
      (org-link-beautify-preview-file-video ov path link))
     ((member extension org-link-beautify-audio-preview-list)
      (org-link-beautify-preview-file-audio ov path link))
     ((member extension org-link-beautify-subtitle-preview-list)
      (org-link-beautify-preview-file-subtitle ov path link))
     ((member extension org-link-beautify-archive-preview-list)
      (org-link-beautify-preview-file-archive ov path link))
     ((member extension org-link-beautify-source-code-preview-list)
      (org-link-beautify-preview-file-source-code ov path link))
     (t (org-link-beautify-iconify ov path link)))))

(defun org-link-beautify-preview-attachment (ov path link)
  "Preview attachment file of PATH over OV overlay position for LINK element.
This function will apply file type function based on file extension."
  (org-with-point-at (org-element-begin link)
    (org-link-beautify-preview-file ov (org-attach-expand path) link)))

;;; file: [image]

(defcustom org-link-beautify-image-preview-list
  '("jpg" "jpeg" "png" "gif" "webp")
  "A list of image file types be supported with thumbnails."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defun org-link-beautify-preview-file-image (ov path link)
  "Preview image file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (require 'image)
    (when-let* ((file (expand-file-name path))
                ;; ((string-match-p (image-file-name-regexp) file))
                ((file-exists-p file)))
      (let* ((width (org-display-inline-image--width link))
             (align (org-image--align link))
             (image (org--create-inline-image file width)))
        (when image                     ; Add image to overlay
	      ;; See bug#59902. We cannot rely on Emacs to update image if the file has changed.
          (image-flush image) ; refresh image in cache if file changed.
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
          t)))))

;;; file: .pdf

(defcustom org-link-beautify-pdf-preview-command
  (cond
   ((executable-find "pdftocairo") "pdftocairo")
   ((executable-find "pdf2svg") "pdf2svg"))
  "The command used to preview PDF file cover."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-pdf-preview-size 600
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
          ("pdftocairo"
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
            path (file-name-sans-extension thumbnail-file)))
          ("pdf2svg"
           (unless (eq org-link-beautify-pdf-preview-image-format 'svg)
             (warn "The pdf2svg only supports convert PDF to SVG format.
Please adjust `org-link-beautify-pdf-preview-command' to `pdftocairo' or
Set `org-link-beautify-pdf-preview-image-format' to `svg'."))
           (start-process
            "org-link-beautify--pdf-preview"
            " *org-link-beautify pdf-preview*"
            "pdf2svg"
            path thumbnail-file (number-to-string pdf-page-number)))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed path thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-pdf (ov path link)
  "Preview pdf file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-pdf path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width org-link-beautify-pdf-preview-size)))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; file: .epub

(defcustom org-link-beautify-epub-preview-command
  (let ((script (expand-file-name "scripts/thumbnailer-ebook.py" (file-name-directory (or load-file-name (buffer-file-name))))))
    (cl-case system-type
      (gnu/linux (if (executable-find "gnome-epub-thumbnailer")
                     "gnome-epub-thumbnailer"
                   script))
      (darwin (if (executable-find "epub-thumbnailer")
                  "epub-thumbnailer"
                script))
      (t script)))
  "Whether enable EPUB files cover preview?
If command \"gnome-epub-thumbnailer\" is available, enable EPUB
preview by default. You can set this option to nil to disable
EPUB preview."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-ebook-preview-size 300
  "The EPUB cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-epub (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .epub file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (epub-page-number (or (match-string 2 path) 1))
           (epub-file (expand-file-name (org-link-unescape file-path)))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path epub-file))
           (thumbnail-file (expand-file-name
                            (if (or (null epub-page-number) (= epub-page-number 1)) ; if have page number ::N specified.
                                (format "%s%s.png" thumbnails-dir (file-name-base epub-file))
                              (format "%s%s-P%s.png" thumbnails-dir (file-name-base epub-file) epub-page-number))))
           (thumbnail-size (or org-link-beautify-ebook-preview-size 600)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (pcase org-link-beautify-epub-preview-command
          ("epub-thumbnailer"           ; for macOS "epub-thumbnailer" command
           (make-process
            :name "org-link-beautify--epub-preview"
            :command (list org-link-beautify-epub-preview-command
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
                          ))))
          ("gnome-epub-thumbnailer"                 ; for Linux "gnome-epub-thumbnailer"
           (start-process
            "org-link-beautify--epub-preview"
            " *org-link-beautify epub-preview*"
            org-link-beautify-epub-preview-command
            epub-file thumbnail-file
            (when org-link-beautify-ebook-preview-size "--size")
            (when org-link-beautify-ebook-preview-size (number-to-string thumbnail-size))
            ))
          (_ (user-error "This system platform currently not supported by org-link-beautify.\n Please contribute code to support"))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed epub-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-epub (ov path link)
  "Preview epub file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-epub path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-ebook-preview-size 300))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; file: .mobi, .azw3

(defcustom org-link-beautify-kindle-preview-command
  (cl-case system-type
    (gnu/linux (executable-find "mobitool"))
    (darwin (executable-find "mobitool")))
  "Whether enable Kindle ebook files cover preview?

Enable Kindle ebook preview by default. You can set this option
to nil to disable EPUB preview.

You can install software `libmobi' to get command `mobitool'."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defvar org-link-beautify--kindle-cover-command
  (cond
   ;; for macOS, use `mobitool' from libmobi.
   ((and (eq system-type 'darwin) (executable-find "mobitool")) "mobitool")
   ;; for Linux, use `mobitool' from libmobi.
   ((and (eq system-type 'gnu/linux) (executable-find "mobitool")) "mobitool"))
  "Find available kindle ebook cover dump command.
You can install software `libmobi' to get command `mobitool'.")

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
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path kindle-file))
           (thumbnail-file (expand-file-name
                            (if (or (null kindle-page-number) (= kindle-page-number 1)) ; if have page number ::N specified.
                                (format "%s%s.jpg" thumbnails-dir (file-name-base kindle-file))
                              (format "%s%s-P%s.jpg" thumbnails-dir (file-name-base kindle-file) kindle-page-number))))
           (thumbnail-size (or org-link-beautify-ebook-preview-size 600)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (pcase org-link-beautify--kindle-cover-command
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
               (rename-file mobitool-cover-file thumbnail-file))))
          (_ (user-error "[org-link-beautify] Error: Can't find command tool to dump kindle ebook file cover"))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed kindle-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-kindle (ov path link)
  "Preview kindle .mobi or .azw3 file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-kindle path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-kindle-preview-size 300))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

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
            (image (create-image data-decoded type-symbol 't)))
      image
    'no-cover))

(defun org-link-beautify-fictionbook2--save-cover (image file-path)
  "Save FictionBook2 cover IMAGE to FILE-PATH."
  ;; TODO: how to save image data into image file?
  ;; `image-save': This writes the original image data to a file.
  (with-temp-buffer
    (insert (plist-get (cdr image) :data))
    (write-region (point-min) (point-max) file-path)))

(defun org-link-beautify--generate-preview-for-file-fictionbook2 (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for .fb2 & .fb2.zip file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (fictionbook2-file (expand-file-name (org-link-unescape file-path)))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path fictionbook2-file))
           (thumbnail-file (cond
                            ((string-match-p "\\.fb2.zip$" path)
                             (expand-file-name
                              (format "%s%s.png" thumbnails-dir (string-replace ".fb2" "" (file-name-base fictionbook2-file)))))
                            ((string-match-p "\\.fb2$" path)
                             (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base fictionbook2-file))))))
           (thumbnail-size (or org-link-beautify-fictionbook2-preview-size 600)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (let ((cover-image (org-link-beautify-fictionbook2--extract-cover fictionbook2-file)))
          (if (eq cover-image 'no-cover)
              (message "[org-link-beautify] FictionBook2 preview failed to extract cover image.")
            (org-link-beautify-fictionbook2--save-cover cover-image thumbnail-file))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed fictionbook2-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-fictionbook2 (ov path link)
  "Preview FictionBook2 .fb2, .fb2.zip file of PATH over OV overlay position for LINK element."
  (when org-link-beautify-fictionbook2-preview
    (if (not (display-graphic-p))
        (prog1 nil
          (message "Your Emacs does not support displaying images!"))
      (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-fictionbook2 path))
                ((file-exists-p thumbnail-file))
                (image (create-image thumbnail-file nil nil :width (or org-link-beautify-fictionbook2-preview-size 300))))
          (progn
            (overlay-put ov 'display image)
	        (overlay-put ov 'face    'default)
	        (overlay-put ov 'keymap  org-link-beautify-keymap))
        (org-link-beautify-iconify ov path link)))))

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
  '("org" "txt" "markdown" "md"
    "lisp" "scm" "clj" "cljs"
    "py" "rb" "pl"
    "c" "cpp" "h" "hpp" "cs" "java"
    "r" "jl")
  "A list of link types supports source code file preview below the link."
  :type 'list
  :safe #'listp
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
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path source-code-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base source-code-file))))
           (thumbnail-size 600)
           (proc-name (format "org-link-beautify--code-preview - %s" source-code-file))
           (proc-buffer (format " *org-link-beautify code preview - %s*" source-code-file))
           (proc (get-process proc-name)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (unless proc
          (pcase org-link-beautify-source-code-preview-command
            ("silicon"
             (start-process
              proc-name proc-buffer
              "silicon" source-code-file "-o" thumbnail-file
              "--no-window-controls" "--shadow-blur-radius" "30" "--shadow-color" "#555"))
            ("germanium"
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
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-source-code path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file)))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (if-let* ((source-code (org-link-beautify--preview-source-code-file path)))
          (progn
            (overlay-put ov 'after-string source-code)
	        (overlay-put ov 'face    'org-block))
        (org-link-beautify-iconify ov path link)))))

;;; file: [comic]

(defcustom org-link-beautify-comic-preview-command
  (cl-case system-type
    (darwin (executable-find "qlmanage")))
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

(defcustom org-link-beautify-comic-preview-size 500
  "The CDisplay Archived Comic Book Formats cover preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-comic (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for CDisplay Archived Comic Book: .cbz, .cbr etc file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (comic-file (expand-file-name (org-link-unescape file-path)))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path comic-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base comic-file))))
           (thumbnail-size (or org-link-beautify-comic-preview-size 1080)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        (cl-case system-type
          ;; TODO:
          (gnu/linux
           ;; (start-process
           ;;  "org-link-beautify--comic-preview"
           ;;  " *org-link-beautify comic-preview*"
           ;;  org-link-beautify-comic-preview-command
           ;;  comic-file thumbnail-file
           ;;  ;; (if org-link-beautify-comic-preview-size
           ;;  ;;     "--size")
           ;;  ;; (if org-link-beautify-comic-preview-size
           ;;  ;;     (number-to-string thumbnail-size))
           ;;  ))
           (darwin
            ;; for macOS "qlmanage" command
            ;; $ qlmanage -t "ラセン恐怖閣-マリコとニジロー1-DL版.cbz" - 2.0 -s 1080 -o ".thumbnails"
            (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory comic-file) ".png")))
              (make-process
               :name "org-link-beautify--comic-preview"
               :command (list org-link-beautify-comic-preview-command
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
                (rename-file qlmanage-thumbnail-file thumbnail-file))))
           (t (user-error "This system platform currently not supported by org-link-beautify.\n Please contribute code to support"))))
        (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
          (org-link-beautify--notify-generate-thumbnail-failed comic-file thumbnail-file))
        ;; return the thumbnail file as result.
        thumbnail-file))))

(defun org-link-beautify-preview-file-comic (ov path link)
  "Preview comic .cbz or .cbr file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-comic path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-comic-preview-size 300))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; file: [video]

(defcustom org-link-beautify-video-preview-command
  (cond
   ;; for macOS, use `qlmanage'
   ((and (eq system-type 'darwin) (executable-find "qlmanage")) "qlmanage")
   ;; for Linux, use `ffmpegthumbnailer'
   ((and (eq system-type 'gnu/linux) (executable-find "ffmpegthumbnailer")) "ffmpegthumbnailer")
   ;; for general, use `ffmpeg'
   ;; $ ffmpeg -i video.mp4 -ss 00:01:00.000 -vframes 1 -vcodec png -an -f rawvideo -s 119x64 out.png
   ((executable-find "ffmpeg") "ffmpeg"))
  "The available command to preview video."
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defcustom org-link-beautify-video-preview-list
  '("mp4" "webm" "mkv" "mov" "mpeg" "ogg" "ogv" "rmvb" "rm" "m4v" "flv")
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
           (video-filename (file-name-nondirectory video-file))
           (thumbnails-dir (org-link-beautify--get-thumbnails-dir-path video-file))
           (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base video-file))))
           (thumbnail-size (or org-link-beautify-video-preview-size 600))
           (proc-name (format "org-link-beautify--video-preview - %s" video-filename))
           (proc-buffer (format " *org-link-beautify video-preview - %s*" video-filename))
           (proc (get-process proc-name)))
      (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
      (unless (file-exists-p thumbnail-file)
        ;; detect process already running?
        (unless proc
          (pcase org-link-beautify-video-preview-command
            ("ffmpeg"
             ;; $ ffmpeg -i video.mp4 -ss 00:01:00.000 -vframes 1 -vcodec png -an -f rawvideo -s 119x64 out.png
             (start-process
              proc-name proc-buffer
              "ffmpeg" "-i" video-file "-ss" "00:01:00.000" "-vframes" "1"
              "-vcodec" "png" "-an" "-f" "rawvideo" "-s" (number-to-string thumbnail-size) thumbnail-file))
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
                 (rename-file qlmanage-thumbnail-file thumbnail-file))))
            ("ffmpegthumbnailer"
             (start-process
              proc-name proc-buffer
              "ffmpegthumbnailer" "-f" "-i" video-file "-s" (number-to-string thumbnail-size) "-o" thumbnail-file)))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed video-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-video (ov path link)
  "Preview video file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-video path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width 400)))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; file: [audio]

(defcustom org-link-beautify-audio-preview (or (executable-find "audiowaveform")
                                               (executable-find "qlmanage"))
  "Whether enable audio files wave form preview?"
  :type 'string
  :safe #'stringp
  :group 'org-link-beautify)

(defvar org-link-beautify-audio-preview-command
  (cond
   ;; for general, use `ffmpeg'
   ((executable-find "ffmpeg") "ffmpeg")
   ;; for macOS, use `qlmanage'
   ((and (eq system-type 'darwin) (executable-find "qlmanage")) "qlmanage")
   ;; for Linux, use `audiowaveform'
   ((and (eq system-type 'gnu/linux) (executable-find "audiowaveform")) "audiowaveform"))
  "Find available audio preview command.")

(defcustom org-link-beautify-audio-preview-list '("mp3" "wav" "flac" "ogg" "m4a" "opus" "dat")
  "A list of audio file types be supported generating audio wave form image."
  :type 'list
  :safe #'listp
  :group 'org-link-beautify)

(defcustom org-link-beautify-audio-preview-size 300
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
          (pcase org-link-beautify-audio-preview-command
            ("ffmpeg"
             (start-process
              proc-name proc-buffer
              "ffmpeg" "-i" audio-file
              "-filter_complex" "[0:a]aformat=channel_layouts=mono,compand=gain=-6,showwavespic=s=600x120:colors=#9cf42f[fg];color=s=600x120:color=#44582c,drawgrid=width=iw/10:height=ih/5:color=#9cf42f@0.1[bg];[bg][fg]overlay=format=auto,drawbox=x=(iw-w)/2:y=(ih-h)/2:w=iw:h=1:color=#9cf42f"
              "-frames:v" "1"
              thumbnail-file))
            ("qlmanage"
             (let ((qlmanage-thumbnail-file (concat thumbnails-dir (file-name-nondirectory audio-file) ".png")))
               (unless (file-exists-p qlmanage-thumbnail-file)
                 (start-process
                  proc-name proc-buffer
                  "qlmanage" "-x" "-t" "-s" (number-to-string thumbnail-size) audio-file "-o" thumbnails-dir))
               ;; then rename [file.extension.png] to [file.png]
               (when (file-exists-p qlmanage-thumbnail-file)
                 (rename-file qlmanage-thumbnail-file thumbnail-file))))
            ("audiowaveform"
             (start-process
              proc-name proc-buffer
              "audiowaveform" "-i" audio-file "-o" thumbnail-file)))))
      (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
        (org-link-beautify--notify-generate-thumbnail-failed audio-file thumbnail-file))
      ;; return the thumbnail file as result.
      thumbnail-file)))

(defun org-link-beautify-preview-file-audio (ov path link)
  "Preview audio file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-audio path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-audio-preview-size 300))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; file: [subtitle]

(defcustom org-link-beautify-subtitle-preview-command nil
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

(defcustom org-link-beautify-subtitle-preview-size 300
  "The subtitle preview image size."
  :type 'number
  :safe #'numberp
  :group 'org-link-beautify)

(defun org-link-beautify--generate-preview-for-file-subtitle (path)
  "Generate THUMBNAIL-FILE with THUMBNAIL-SIZE for subtitle file of PATH."
  (when (string-match "\\(.*?\\)\\(?:::\\(.*\\)\\)?\\'" path)
    (let* ((file-path (match-string 1 path))
           (search-option (match-string 2 path))
           (subtitle-file (expand-file-name (org-link-unescape file-path))))
      (if org-link-beautify-subtitle-preview-command
          (let* ((thumbnails-dir (org-link-beautify--get-thumbnails-dir-path subtitle-file))
                 (thumbnail-file (expand-file-name (format "%s%s.png" thumbnails-dir (file-name-base subtitle-file))))
                 (thumbnail-size (or org-link-beautify-subtitle-preview-size 200)))
            (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
            (unless (file-exists-p thumbnail-file)
              (pcase org-link-beautify-subtitle-preview-command
                ;; TODO:
                ))
            ;; return the thumbnail file as result.
            thumbnail-file)
        (let* ((subtitle-file-context (split-string (shell-command-to-string (format "head -n 20 '%s'" subtitle-file)) "\n"))
               (text (concat "\n" (org-link-beautify--display-content-block subtitle-file-context))))
          ;; return the subtitle file context as result.
          text)))))

(defun org-link-beautify-preview-file-subtitle (ov path link)
  "Preview subtitle file of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if org-link-beautify-subtitle-preview-command
        (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-subtitle path))
                  ((file-exists-p thumbnail-file))
                  (image (create-image thumbnail-file nil nil :width (or org-link-beautify-subtitle-preview-size 300))))
            (progn
              (overlay-put ov 'display image)
	          (overlay-put ov 'face    'default)
	          (overlay-put ov 'keymap  org-link-beautify-keymap))
          (org-link-beautify-iconify ov path link))
      (when-let* ((text (org-link-beautify--generate-preview-for-file-subtitle path)))
        (overlay-put ov 'after-string text)
	    (overlay-put ov 'face         'default)))))

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
           (command (cdr (assoc archive-extension org-link-beautify-archive-preview-command-alist)))
           (execute-command (format "%s '%s'" command archive-file))
           (archive-files-list (split-string (shell-command-to-string execute-command) "\n"))
           (text (concat "\n" (org-link-beautify--display-content-block archive-files-list))))
      ;; return the files list in archive file as result.
      text)))

(defun org-link-beautify-preview-file-archive (ov path link)
  "Preview archive file link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((text (org-link-beautify--generate-preview-for-file-archive path)))
        (progn
          (overlay-put ov 'after-string text)
	      (overlay-put ov 'face         'default)
	      (overlay-put ov 'keymap       org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; pdf: & docview: link type

(defun org-link-beautify-preview-pdf (ov path link)
  "Preview pdf: link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-pdf path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file)))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; epub: link type

(defun org-link-beautify-preview-epub (ov path link)
  "Preview epub: link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-epub path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file)))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; nov: link type

(defun org-link-beautify-preview-nov (ov path link)
  "Preview nov: link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-epub path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file)))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; video: link type

(defun org-link-beautify-preview-video (ov path link)
  "Preview video: link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-video path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-video-preview-size 600))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; audio: link type

(defun org-link-beautify-preview-audio (ov path link)
  "Preview audio: link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-file-audio path))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-audio-preview-size 300))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))

;;; org-contact: link type

(defun org-link-beautify--generate-preview-for-org-contacts (name)
  "Get the avatar of org-contact in NAME."
  (let* ((epom (org-contacts-search-contact name)))
    (org-contacts-get-avatar-icon epom)))

;;; TEST:
;; (org-link-beautify--generate-preview-for-org-contacts "stardiviner")

(defun org-link-beautify-preview-org-contact (ov path link)
  "Preview org-contct: link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((name path)
              (image (org-link-beautify--generate-preview-for-org-contacts name)))
        (progn
          (overlay-put ov 'display image)
          (overlay-put ov 'after-string (concat
                                         (propertize "{" 'face '(:foreground "purple2"))
                                         (propertize name 'face 'org-verbatim)
                                         (propertize "}" 'face '(:foreground "purple2")))))
      (if-let* ((text (org-element-property :title (org-contacts-search-contact name))))
          (overlay-put ov 'after-string (concat
                                         (propertize "{" 'face '(:foreground "purple2"))
                                         (propertize text 'face 'org-verbatim)
                                         (propertize "}" 'face '(:foreground "purple2"))))
        (org-link-beautify-iconify ov path link)))))

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
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* (((require 'org-bookmarks nil t))
              (text (org-link-beautify--generate-preview-for-org-bookmarks path)))
        (progn
          (overlay-put ov 'after-string text)
          (overlay-put ov 'face         'org-link))
      (org-link-beautify-iconify ov path link))))

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

(defun org-link-beautify-preview-git (ov path link)
  "Preview git: link of PATH over OV overlay position for LINK element."
  (org-link-beautify-iconify ov path link))

;;; http[s]: url link type

(defcustom org-link-beautify-url-preview-command
  (cond
   ((executable-find "webkit2png") "webkit2png")
   ((executable-find "monolith") "monolith"))
  "Find available URL web page screenshot archive command."
  :type 'string
  :safe #'stringp
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
         (thumbnail-filename (format "org-link-beautify screenshot of sha1 URL %s.png" (sha1 url)))
         (thumbnail-file (expand-file-name thumbnail-filename thumbnails-dir))
         (html-archive-file (concat (file-name-sans-extension thumbnail-file) ".html"))
         (thumbnail-size (or org-link-beautify-url-preview-size 1000))
         (process-name (format "org-link-beautify--url-screenshot %s" url))
         (process-buffer (format " *org-link-beautify--url-screenshot %s*" url))
         (proc (get-process process-name)))
    (org-link-beautify--ensure-thumbnails-dir thumbnails-dir)
    (unless (or (file-exists-p thumbnail-file)
                (file-exists-p html-archive-file))
      (unless proc
        (pcase org-link-beautify-url-preview-command
          ("webkit2png"
           (start-process
            process-name process-buffer
            "webkit2png" url "-o" thumbnail-file))
          ("monolith"
           (let* ((html-archive-file (concat (file-name-sans-extension thumbnail-file) ".html")))
             (start-process
              process-name process-buffer
              "monolith" "--no-audio" "--no-video" url "--output" html-archive-file))))))
    (when (and org-link-beautify-enable-debug-p (not (file-exists-p thumbnail-file)))
      (org-link-beautify--notify-generate-thumbnail-failed url thumbnail-file))))

(defun org-link-beautify-preview-url (ov path link)
  "Preview http[s]: URL link of PATH over OV overlay position for LINK element."
  (if (not (display-graphic-p))
      (prog1 nil
        (message "Your Emacs does not support displaying images!"))
    (if-let* ((thumbnail-file (org-link-beautify--generate-preview-for-url ov path link))
              ((file-exists-p thumbnail-file))
              (image (create-image thumbnail-file nil nil :width (or org-link-beautify-url-preview-size 600))))
        (progn
          (overlay-put ov 'display image)
	      (overlay-put ov 'face    'default)
	      (overlay-put ov 'keymap  org-link-beautify-keymap))
      (org-link-beautify-iconify ov path link))))


;;; minor mode `org-link-beautify-mode'

;;;###autoload
(defun org-link-beautify-enable ()
  "Enable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (pcase link-type
      ("file" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-file)) ; `org-link-preview-file',
      ("attachment" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-attachment)) ; `org-attach-preview-file'
      ("pdf" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-pdf))
      ("docview" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-pdf))
      ("pdfview" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-pdf))
      ("epub" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-epub))
      ("nov" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-epub))
      ("video" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-video))
      ("audio" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-audio))
      ("org-contact" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-org-contact))
      ("org-bookmark" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-org-bookmark))
      ("excalidraw" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-excalidraw))
      ("geo" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-geography))
      ("git" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-git))
      ("http" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-url))
      ("https" (org-link-set-parameters link-type :preview #'org-link-beautify-preview-url))
      ("mu4e" (org-link-set-parameters link-type :preview #'org-link-beautify-iconify))
      (_ (org-link-set-parameters link-type :preview #'org-link-beautify-iconify)))))

;;;###autoload
(defun org-link-beautify-disable ()
  "Disable `org-link-beautify'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (org-link-set-parameters link-type :preview nil)
    (org-link-set-parameters "file" :preview #'org-link-preview-file)
    (org-link-set-parameters "attachment" :preview #'org-attach-preview-file)))

(defvar org-link-beautify-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "The `org-link-beautify-mode' minor mode map.")

;;;###autoload
(define-minor-mode org-link-beautify-mode
  "A minor mode to beautify Org Mode links with icons, and inline preview etc."
  :group 'org-link-beautify
  :global nil
  :init-value nil
  :lighter " ߷"
  :keymap org-link-beautify-mode-map ; NOTE: avoid to enable `org-link-beautify-keymap' globally everywhere.
  (if org-link-beautify-mode
      (org-link-beautify-enable)
    (org-link-beautify-disable)))



(provide 'org-link-beautify)

;;; org-link-beautify.el ends here
