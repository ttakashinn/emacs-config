# emacs-config

My emacs config backup

When deploy on the new device, follow the below steps.

1. Clone this repository into ~/.emacs.d/custom (git clone .... ~/.emacs.d/custom)
2. Copy init.el to ~/.emacs.d/init.el
3. Start emacs and wait for it to clone and build packages.

Re-build packages:
1. Delete all packages in .emacs.d/straight/repos and .emacs.d/straight/build
2. Start emacs and wait for it to re-clone and re-build packages.
