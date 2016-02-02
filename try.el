;;; try.el --- Try out Emacs packages. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Lars Tveito.

;; Author: Lars Tveito <larstvei@ifi.uio.no>
;; URL: http://github.com/larstvei/try
;; Created: 13th November 2014
;; Keywords: packages
;; Version: 0.0.1
;; Package-Requires: ((emacs "24"))

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

;;; Commentary:

;; # Try

;; Try is a packages that allow you to try out Emacs packages without
;; installing them. If you pass a URL to a plain-text `.el`-file it evaluates
;; the content, without storing the file.

;; Packages from ELPA will temporarily be stored in `/tmp/` by default.

;; ## Installation

;; You can install Try using elpa.

;; It's available on [marmalade](http://marmalade-repo.org/) and
;; [melpa](http://melpa.milkbox.net/):

;; M-x package-install try

;; ## Usage

;; To try out a package you can run

;; M-x try RET some-package

;; Or if you want to try out some package from the web, just paste in the URL

;; M-x try RET https://url.com/to/some/file.el

;; ### Example

;; If you for instance have [melpa](http://melpa.org/) in your
;; `package-archives` you can try
;; [multiple cursors](https://github.com/magnars/multiple-cursors.el) by
;; running:

;; M-x try RET multiple-cursors RET

;; If you on the other hand want to test out a single `.el`-file from somewhere
;; you can simply insert an URL. Trying out
;; [Leuven-theme](https://github.com/fniessen/emacs-leuven-theme) can be done
;; by running:

;; M-x try RET https://raw.githubusercontent.com/fniessen/emacs-leuven-theme/master/leuven-theme.el RET

;; Unfortunately, you won't be able to try Try with `M-x try RET try`.

;;; Code:
(require 'package)
(require 'url)

(defvar try-tmp-dir (make-temp-file "try" t)
  "The default place packages to try out is /tmp/.")

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

(defun try-package-exists-p (package-name)
  "Returns non-nil if the package is available for download."
  (assq package-name package-archive-contents))

(defun try-compose (f g)
  "Compose two functions."
  (lambda (&rest x) (funcall f (apply g x))))

(defun try-complete (archive)
  "Complete from available package names."
  (let* ((f (try-compose #'symbol-name #'car))
         (pkgs (mapcar f archive)))
    (completing-read "url or package: " pkgs)))

(defun try-and-refresh ()
  "Refreshes package-list before calling `try'."
  (interactive)
  (package-refresh-contents) (try))

;;;###autoload
(defun try ()
  "Try out a package from your `package-archives' or pass a URL
to a raw .el file. Packages are stored in `try-tmp-dir' and raw
.el files are not stored at all."
  (interactive)
  ;; Completions for packages.
  (let* ((url-or-package (try-complete package-archive-contents))
         (package-symbol (intern url-or-package)))
    (cond ((try-raw-link-p url-or-package) (try-raw-link url-or-package))
          ((try-package-exists-p package-symbol)
           (let ((package-user-dir try-tmp-dir)
                 (package-alist nil))
             (package-install package-symbol)
             (message "Trying %s!" url-or-package)))
          (t (message (concat "Couldn't find a sensible way to try this. "
                              "Try running `package-refresh-contents'!"))))))

(provide 'try)

;;; try.el ends here.
