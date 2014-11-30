# My Shell Rc

My .rc config

A set of cool commands/alias for Linux and Mac OS.

Contains:
* [#ls](A cool ls alias)
* [#sublime-text](An alias for Sublime Text on Mac OS)
* [#gcc](A GCC alias with errors flags)
* [#git](Some useful alias for git)
* [#save-go](Save and Go commands)

## Installation

**Copy** the content of the file _rc.sh_ in your rc file.<br />
_(**.zshrc**, .bashrc, .bash\_aliases, .profile, etc...)_

## Content

You can change the commands name by changing the alias/function name.

More commands will be added in the future.

### Ls

The `l` command is an alias for the _ls_ command with a colorful and easy to read output.

![ls](/captures/ls.png)

### Sublime Text

The `s` command open Sublime Text.

Can take file name _(existing or not)_ as arguments.

### GCC

The `g` command is an alias for `gcc -Wall -Werror -Wextra`.

### Git

The `t` command is an alias for `git status -sb`.<br />
Show the git status in short format.

The `a` command is an alias for `git add --all` and `t`.<br />
Add file passed as arguments to the git index.<br />
If no arguments is passed, add all files to the index.

The `r` command is an alias for `git reset HEAD -q` and `t`.<br />
Remove the file from the git index. _(don't remove the file itself)_<br />
_It's the opposite of the `a` command._

The `c` command is an alias for `git commit -m`.

The `p` command is an alias for `git push origin master --tags`.

The `amend` command is an alias for `git commit --amend --no-edit`.<br />
Add the current status to the last commit.

### Save Go

Save Go allow you to create '_alias_' for working directories.

`save` command save the current _path_ and `go` command move to it.

`save` and `go` take an optional argument: **name**<br />
so you can save infinity of working dir.

Save Go is cross terminal/terminal tab.<br />
