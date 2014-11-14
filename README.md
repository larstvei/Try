# Try

Try is a packages that allow you to try out Emacs packages without
installing them. If you pass a URL to a plain-text `.el`-file it evaluates
the content, without storing the file.

Packages from ELPA will temporarily be stored in `/tmp/` by default.

## Usage

Remember to make packages from your `package-archives` available for
download with

```
M-x package-refresh-contents
```

### Example

If you for instance have [melpa](http://melpa.org/) in your
`package-archives` you can try
[multiple cursors](https://github.com/magnars/multiple-cursors.el) by
running:

```
M-x try RET multiple-cursors RET
```

If you on the other hand want to test out a single `.el`-file from somewhere
you can simply insert an URL. Trying out
[Leuven-theme](https://github.com/fniessen/emacs-leuven-theme) can be done
by running:

```
M-x try RET https://raw.githubusercontent.com/fniessen/emacs-leuven-theme/master/leuven-theme.el RET
```

Unfortunately, you won't be able to try Try with `M-x try RET try`.
