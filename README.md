# emacs-config

My emacs config backup

### Deploy on the new device:

1. Clone this repository into `~/.emacs.d/config` (`git clone .... ~/.emacs.d/config`)
2. Create symlink `~/.emacs.d/init.el` that link to `~/.emacs.d/config/init.el` (or copy it)
3. Create symlink `~/.emacs.d/early-init.el` that link to `~/.emacs.d/config/early-init.el` (or copy it)
4. Start emacs and wait for it to clone and build packages.

### Re-build packages:

1. Delete all packages in `.emacs.d/straight/repos` and `.emacs.d/straight/build`
2. Start emacs and wait for it to re-clone and re-build packages.

### Screenshot

<img width="814" alt="dashboard" src="https://user-images.githubusercontent.com/9399414/123299377-a7392f80-d543-11eb-9e83-f388c8d8f024.png">
