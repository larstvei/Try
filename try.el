;;; try.el --- Try out Emacs packages. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>

;; Contains code from GNU Emacs <https://www.gnu.org/software/emacs/>,
;; released under the GNU General Public License version 3 or later.

;; Try is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; Try is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with Try. If not, see <http://www.gnu.org/licenses/>.

(require 'package)
(require 'url)

(defvar try-tmp-dir "/tmp/")

(defun try-raw-link-p (url)
  "Returns non-nil if this looks like an URL to a .el file."
  (string-match-p "[^:]*://\\([^?\r\n]+\\).*\.el?$" url))

(defun try-raw-link (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (condition-case nil
        (progn
          (eval-region (search-forward-regexp "^$") (point-max))
          (let ((str (car (last (split-string url "/")))))
            (message "Trying %s!" str)))
      ((debug error)
       (message "Could not parse %s" url) nil))))

(defun try-compose (f g)
  "Compose two functions."
  (lambda (&rest x) (funcall f (apply g x))))

(defun try ()
  "Try out a package from your `package-archives' or pass a URL
to a raw .el file. Packages are stored in `try-tmp-dir' and raw
.el files are not stored at all."
  (interactive)
  (let ((url-or-package (completing-read
                         "url or package: "
                         (mapcar (try-compose 'symbol-name 'car)
                                 package-archive-contents))))
    (cond ((try-raw-link-p url-or-package) (try-raw-link url-or-package))
          ((assq (intern url-or-package) package-archive-contents)
           (let ((package-user-dir try-tmp-dir)
                 (package-alist nil))
             (package-install (intern url-or-package))
             (message "Trying %s!" url-or-package)))
          (t (message (concat "Couldn't find a sensible way to try this. "
                              "Try running `package-refresh-contents'!"))))))

(provide 'try)
