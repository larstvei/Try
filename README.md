#Try

This is a package that allow you to try out other Emacs packages, without
installing them. If you install a package (using `package-install`) you have
to remember to delete this, if you realize you don't need that package. A
common way around this is to copy the code into the `*scratch*`-buffer and
evaluate the buffer. This extension essentially automates this process, by
excepting a URL to a plain-text `.el`-file, downloading the content and
evaluate it.

Unfortunately, you won't be able to try Try with `M-x try`.
