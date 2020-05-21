;;; org-link-beautify.el --- Beautify org links -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-05-21 10:34:03 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25") (cl-lib "0.5"))
;; Package-Version: 0.1
;; Keywords: org link
;; homepage: https://github.com/stardiviner/org-link-beautify

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



;;; Code:

(defgroup org-link-beautify nil
  "Customize group of org-link-beautify-mode."
  :prefix "org-link-beautify-"
  :group 'org)

(defun org-link-beautify--get-element (position)
  "Return the org element of link at the `POSITION'."
  (save-excursion (goto-char position) (org-element-context)))

(defun org-link-beautify--get-link-description-fast (position)
  "Get the link description at `POSITION' (fuzzy but faster version)."
  (save-excursion
    (goto-char position)
    (and (org-at-regexp-p org-link-bracket-re) (match-string 2))))

(defun org-link-beautify--propertize (start end desc icon)
  "Construct link display with link description and icon."
  (put-text-property
   start end
   'display
   (propertize
    (concat
     (propertize "[" 'face '(:inherit nil :underline nil :foreground "orange"))
     (propertize desc 'face '(:underline t :foreground "dark cyan"))
     (propertize "]" 'face '(:inherit nil :underline nil :foreground "orange"))
     (propertize "(" 'face '(:inherit nil :underline nil :foreground "orange"))
     (propertize icon 'face '(:inherit nil :underline nil :foreground "gray"))
     (propertize ")" 'face '(:inherit nil :underline nil :foreground "orange"))))))

(defun org-link-beautify--warning (path)
  (if (and (not (file-remote-p path))
           (file-exists-p (expand-file-name path)))
      'org-link 'org-warning))

(defun org-link-beautify (start end path bracketp)
  "Display icon for the Org link type."
  ;; (message
  ;;  (format "start: %s, end: %s, path: %s, bracketp: %s" start end path bracketp))
  (let* ((link-element (org-link-beautify--get-element start))
         ;; (debug0 (message link-element))
         (raw-link (org-element-property :raw-link link-element))
         ;; (debug1 (message raw-link))
         (type (org-element-property :type link-element))
         (ext (file-name-extension (org-link-unescape path)))
         (description (or (and (org-element-property :contents-begin link-element) ; in raw link case, it's nil
                               (buffer-substring-no-properties
                                (org-element-property :contents-begin link-element)
                                (org-element-property :contents-end link-element)))
                          ;; when description not exist, use raw link for raw link case.
                          raw-link))
         ;; (debug2 (message description))
         (icon (pcase type
                 ("file"
                  (if (file-directory-p path)
                      (all-the-icons-icon-for-dir
                       "path"
                       :face (org-link-beautify--warning path)
                       :v-adjust 0)
                    (all-the-icons-icon-for-file
                     (format ".%s" ext)
                     :face (org-link-beautify--warning path)
                     :v-adjust 0)))
                 ("file+sys" (all-the-icons-material "apps"))
                 ("file+emacs" (all-the-icons-icon-for-mode 'emacs-lisp-mode))
                 ("http" (all-the-icons-icon-for-url path))
                 ("https" (all-the-icons-icon-for-url path))
                 ("ftp" )
                 ("attachment" )
                 ("id" )
                 ("elisp" (all-the-icons-icon-for-mode 'emacs-lisp-mode))
                 ("shell" (all-the-icons-icon-for-mode 'shell-mode))
                 ("eww" (all-the-icons-icon-for-mode 'eww-mode))
                 ("mu4e" (all-the-icons-icon-for-mode 'mu4e-headers-mode))
                 ("git" (all-the-icons-octicon "git-branch"))
                 ("orgit" (all-the-icons-octicon "git-branch"))
                 ("orgit-rev" (all-the-icons-octicon "git-commit"))
                 ("orgit-log" (all-the-icons-icon-for-mode 'magit-log-mode))
                 ("pdfview" (all-the-icons-icon-for-file ".pdf"))
                 ("grep" (all-the-icons-icon-for-mode 'grep-mode))
                 ("occur" (all-the-icons-icon-for-mode 'occur-mode))
                 ("man" (all-the-icons-icon-for-mode 'Man-mode))
                 ("info" (all-the-icons-icon-for-mode 'Info-mode))
                 ("help" (all-the-icons-icon-for-mode 'Help-Mode))
                 ("rss" (all-the-icons-material "rss_feed"))
                 ("elfeed" (all-the-icons-material "rss_feed"))
                 ("telnet")
                 ("wikipedia" (all-the-icons-icon-for-file ".wiki"))
                 ("mailto" (all-the-icons-material "email"))
                 ("doi" )
                 )))
    (org-link-beautify--propertize start end description icon)))

(org-link-set-parameters
 "file"
 :activate-func #'org-link-beautify)

(defun org-link-unset-parameters (type &rest parameters)
  "Unset link parameters."
  ;; TODO
  )

(defun org-link-beautify-enable ()
  "Enable org-link-beautify."
  (dolist (link-type (mapcar 'car org-link-parameters))
    (org-link-set-parameters link-type :activate-func #'org-link-beautify)))

(defun org-link-beautify-disable ()
  (dolist (type (mapcar 'car org-link-parameters))
    ;; FIXME
    (org-link-unset-parameters type)))

(define-minor-mode org-link-beautify-mode
  "A minor mode that beautify Org links with colors and icons."
  :init-value nil
  :lighter nil
  :group 'org-link-beautify
  (if org-link-beautify-mode (org-link-beautify-enable) (org-link-beautify-disable)))

(define-global-minor-mode global-org-link-beautify-mode org-link-beautify-mode
  org-link-beautify-mode)



(provide 'org-link-beautify)

;;; org-link-beautify.el ends here
