# My Shell Rc

My .rc config

A set of cool commands/alias for Linux and Mac OS.

Contains:
* [The 42 Header](#42-header)
* [A cool ls alias](#ls)
* [An alias for Sublime Text on Mac OS](#sublime-text)
* [A GCC alias with errors flags](#gcc)
* [Some useful alias for git](#git)
* [Save and Go commands](#save-go)

## Installation

**Copy** the content of the file _rc.sh_ in your rc file.<br />
_(**.zshrc**, .bashrc, .bash\_aliases, .profile, etc...)_

## Content

You can change the commands name by changing the alias/function name.

More commands will be added in the future.

### 42 Header

**h**<br />
The `h` command update the 42 header of a file.

If the file don't exist, it is created.

The `h` take one required argument: the file name<br />
it takes also a second optional argument: your name

_Warning: don't work for `shell` files or `Makefile`_

### Ls

**l**<br />
The `l` command is an alias for the _ls_ command with a colorful and easy to read output.

![ls](/captures/ls.png)

### Sublime Text

**s**<br />
The `s` command open Sublime Text.

Can take file name _(existing or not)_ as arguments.

### GCC

**g**<br />
The `g` command is an alias for `gcc -Wall -Werror -Wextra`.

### Git

**t**<br />
The `t` command is an alias for `git status -sb`.<br />
Show the git status in short format.

**a**<br />
The `a` command is an alias for `git add --all` and `t`.<br />
Add file passed as arguments to the git index.<br />
If no arguments is passed, add all files to the index.

**r**<br />
The `r` command is an alias for `git reset HEAD -q` and `t`.<br />
Remove the file from the git index. _(don't remove the file itself)_<br />
_It's the opposite of the `a` command._

**c**<br />
The `c` command is an alias for `git commit -m`.

**p**<br />
The `p` command is an alias for `git push origin master --tags`.

**amend**<br />
The `amend` command is an alias for `git commit --amend --no-edit`.<br />
Add the current status to the last commit.

### Save Go

Save Go allow you to create '_alias_' for working directories.

`save` command save the current _path_ and `go` command move to it.

`save` and `go` take an optional argument: **name**<br />
so you can save infinity of working dir.

Save Go is cross terminal/terminal tab.<br />
