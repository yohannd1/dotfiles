# YohananDiamond's Dotfiles

![A screenshot of my desktop](img/screenshot.png)

This repository contains most of my config files, including also my
scripts and my Arch Linux environment.

## Installation

This repo is currently very unstable to simply do a clone and then use.
I really recommend taking a look at files that interest you instead of
simply copying everything because not only it is very unsafe due to most
scripts here being pretty buggy, but also because I believe it's better
to only add to your system things that you understand, instead of making
it confusing with the imense amount of bloat that would come with
getting all scripts from a repository.

If you want, you can use the `dotsync` in the `scripts` folder to do the
thing, but, as said before, it's still pretty incomplete, so I can't
assure backups will work well.

## Folder structure

* `config`: config files, mostly from `~` and `~/.config`;

* `local`: contains directories that have their files symlinked to
  `~/.local/<dirname>`;

## Inspirations and "Steals"

The repos below are the ones from where I remember ~~stealing code~~
taking inspiration to make my dotfiles. There are lots of more people
from where I took inspiration - specially from r/unixporn and r/vim -
and I can't possibly remember them all, but I'd like to thank them. It
has been a pretty cool experience to make, well, pretty much my own
operating system, even though I didn't code the Linux kernel or
something.

* https://github.com/denysdovhan/dotfiles
* https://github.com/LukeSmithxyz/voidrice
* https://github.com/jdhao/nvim-config
* https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc

## Commit Conventions

### Type I: Complex

* Header: a summary of the changes, sometimes not very useful

* Description: a list of changes and/or an argument

  The changes list consists on list items with the format:

  ```
  * Namespace#subItem: description... ... 72chars wide.................
    continuing the line...
  ```

  Where `Namespace` is the type of change - program name (like `Zsh`,
  `Neovim`) or a category (like `Meta` or `Script`) - and `subItem` is
  the subcategory, like a script name or something like (it's nto always
  used).

  Every item should end with a dot, not a semicolon or something else.
  That is because sometimes I write more than one sentence in a single
  one.

### Type II: Lazy

Sometimes I don't care, so there might be some ultra lazy commit
messages, maybe even without description. It might also just be that
there aren't any big changes to be documented.
