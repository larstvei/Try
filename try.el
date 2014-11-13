;;; -*- lexical-binding: t -*-
(require 'package)
(require 'url)

(defvar try-tmp-dir "/tmp/")

(defun try-newest-package-installed-p (package)
  "Return true if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((local-pkg-desc (or (assq package package-alist)
                               (assq package package--builtins)))
           (newest-pkg-desc (assq package package-archive-contents)))
      (and local-pkg-desc newest-pkg-desc
           (version-list-= (package-desc-vers (cdr local-pkg-desc))
                           (package-desc-vers (cdr newest-pkg-desc)))))))


(defun try-raw-link-p (url)
  (string-match-p "[^:]*://\\([^?\r\n]+\\).*\.el?$" url))

(defun try-raw-link (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (condition-case nil
        (progn
          (eval-region (search-forward-regexp "^$") (point-max))
          (message "Trying!"))
      ((debug error)
       (message "Could not parse %s" url) nil))))

(defun try-compose (f g)
  (lambda (&rest x) (funcall f (apply g x))))

(defun try-package (package)
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (if (try-newest-package-installed-p package)
      (message "Already installed.")
    (message "Trying %s!" (symbol-name package))
    t))

(defun try ()
  "Takes an URL to a .el-file, and evaluates it."
  (interactive)
  (let ((url-or-package (completing-read
                         "url or package: "
                         (mapcar (try-compose 'symbol-name 'car)
                                 package-archive-contents))))
    (cond ((try-raw-link-p url-or-package) (try-raw-link url-or-package))
          ((try-package (intern url-or-package)))
          (t (message "Couldn't find a sensible way to try this.")))))

(provide 'try)
