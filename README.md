# Try

Try is a packages that allow you to try out Emacs packages without
installing them. If you pass a URL to a plain-text `.el`-file it evaluates
the content, without storing the file.

Packages from ELPA will temporarily be stored in `/tmp/` by default.

## Installation

You can install Try using elpa.

It's available on [marmalade](http://marmalade-repo.org/) and
[melpa](http://melpa.milkbox.net/):

<kbd> M-x package-install try </kbd>

## Usage

To try out a package you can run

<kbd> M-x try RET some-package </kbd>

Or if you want to try out some package from the web, just paste in the URL

<kbd> M-x try RET https://url.com/to/some/file.el </kbd>

### Example

If you for instance have [melpa](http://melpa.org/) in your
`package-archives` you can try
[multiple cursors](https://github.com/magnars/multiple-cursors.el) by
running:

<kbd> M-x try RET multiple-cursors RET </kbd>

If you on the other hand want to test out a single `.el`-file from somewhere
you can simply insert an URL. Trying out
[Leuven-theme](https://github.com/fniessen/emacs-leuven-theme) can be done
by running:

<kbd> M-x try RET https://raw.githubusercontent.com/fniessen/emacs-leuven-theme/master/leuven-theme.el RET </kbd>

Unfortunately, you won't be able to try Try with `M-x try RET try`.
