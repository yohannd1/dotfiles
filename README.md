**Welcome to my dotfiles repo!**

This repository contains most of my the config files I've decided to
make public. It also contains a whole lot of scripts.

## dependencies

- https://github.com/yohannd1/dotcfg

## installation

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

## folder structure

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

## inspirations & & stuff I "stole"

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

## milestones

- Find a good compiled language for more complicated scripts
  - Must be easy to compile
  - Tried out `dlang`, will try `nim`
- Make a system-agnostic local package builder
  - Initial requirement is to generate pacman & apt packages.
- Be more explicit about dependencies (see TODO file...)

## script naming

Since there is pretty much one global namespace for all scripts (the
`$PATH`, I mean), I like to prefix some scripts. I'm thinking of a
prefix scheme that is very short (for small command names) but still
understandable, so here's an attempt:

- `d.` for dotfiles-local problems (currently it's mostly `dotf.`);
- `s.` for "service-providers" - a series of wrappers that decide which
    underlying command to run based on the WM/DE (currently it's mostly
    `dotf.wrap.` - what a mouthful...);
- `@` for wrapper scripts for apps that have trouble launching or I like
    to set up particularly (e.g. steam, since I like it to start up
    silently and log into a specific file);
- `,` for local scripts (they aren't here lol);
