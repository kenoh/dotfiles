my configuration dotfiles :)

# Stowing

Using directory structure suitable for usage with [GNU Stow](https://www.gnu.org/software/stow/), like:
```Shell
cd ~/dotfiles
./stow.sh emacs
./stow.sh vim
```

## Ad `stow.sh`

So, in order to not create links to topmost path available (e.g., for `~/.config/emacs/init.el` we don't want the symlink to be created at `~/.config`, rather at `~/.config/emacs`) we use this script that prepopulates the unwanted locations with directories and therefore the subsequent run of the actual `stow` will not try to create any symlinks at those places.

In order to have the directories pre-created, they should be specified in the `stow-skel-dirs` file.
