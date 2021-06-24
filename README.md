# emacs-config

My emacs config backup

### Deploy on the new device:

1. Clone this repository into `~/.emacs.d/config` (`git clone .... ~/.emacs.d/config`)
2. Copy (or create symlink) `init.el` to `~/.emacs.d/init.el`
3. Start emacs and wait for it to clone and build packages.

### Re-build packages:

1. Delete all packages in `.emacs.d/straight/repos` and `.emacs.d/straight/build`
2. Start emacs and wait for it to re-clone and re-build packages.
