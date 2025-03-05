**Welcome to my dotfiles repo!**

This repository contains most of my the config files I've decided to
make public. It also contains a whole lot of scripts.

## Dependencies

- https://github.com/yohannd1/dotcfg

## Installation

This repo is unstable, as I keep changing it constantly. I really
recommend taking a look only at files that interest you instead of
simply copying everything because not only it is very unsafe due to most
scripts here being pretty buggy, but also because I believe it's better
to only add to your system things that you understand.

And it might also brick things! No guarantee, so please backup your
system.

If you want to do it anyway, though (or if you're me! I do this to
bootstrap the dotfiles on a new machine), you can do:

```bash
curl https://raw.githubusercontent.com/yohannd1/dotfiles/master/install.bash > install.bash
cat install.bash # check it out before running...
bash install.bash https $path_to_dotfiles
```

## Folder structure

* `config`: config files, mostly symlinked to `~` and `~/.config`.

* `desktop`: .desktop files so some apps (mostly browsers) can respect my program choices.

* `lib`: personal libraries for use with some scripting languages.

* `patches`: some patches I usually would like to apply on a system. I
  currently have no way to automate this.

* `pkgbuilds`: some pkgbuilds to install packages I use... kinda like a
    personal AUR I guess

* `scripts`: executable files, added to PATH via `env.sh`.

* `share`: other generic resources

* `.trash`: old files that I don't want to delete right now, but aren't
  being used in the repo. Might be unavailable if empty (due to how
  git works).

## Inspiration / stuff I "stole"

The repos below are the ones I remember ~~stealing code~~ taking
inspiration from to build my dotfiles. There are lots of more places
that inspired me, though - specially from r/unixporn and r/vim - and
even though I can't possibly remember them all, I'd like to thank them.
It has been a pretty cool experience to make, well, pretty much my own
"operating system" (obviously not from scratch...).

* https://github.com/denysdovhan/dotfiles

* https://github.com/LukeSmithxyz/voidrice

* https://github.com/jdhao/nvim-config

* https://gist.github.com/sooop/8dc424e13c6fe2e2a663

  (It's Steve Losh (sjl)'s vimrc. The [original
  file](https://bitbucket.org/sjl/dotfiles/src/tip/vim/vimrc) and repo
  were deleted.)
