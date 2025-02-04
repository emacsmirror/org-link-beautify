;;; org-link-beautify.el --- Beautify Org Links -*- lexical-binding: t; -*-

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "29.1") (nerd-icons "0.0.1") (qrencode "1.3"))
;; Version: 2.0.1
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
;;   :init (org-link-beautify-mode t))

;;; Code:

(if (version<= (org-version) "9.7.14")
    (require 'org-link-beautify-old)
  (require 'org-link-beautify-new))



(provide 'org-link-beautify)

;;; org-link-beautify.el ends here
