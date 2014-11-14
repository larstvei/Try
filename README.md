# Try

Try is a packages that allow you to try out Emacs packages without
installing them. If you pass a URL to a plain-text `.el`-file it evaluates
the content, without storing the file.

Remember to make packages from your `package-archives` available for
download with `M-x package-refresh-contents`.

Packages from ELPA will temporarily be stored in `/tmp/` by default.

Unfortunately, you won't be able to try Try with `M-x try RET try`.
