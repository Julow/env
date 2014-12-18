# My Shell Rc

My .rc config

A set of cool commands/alias for Linux and Mac OS.

Contains:
* [Alias for grep](#f) (f)
* [Timeout util](#timeout) (timeout)
* [The 42 Header](#42-header) (h)
* [A cool ls alias](#ls) (l)
* [A recursive norminette](#norminette) (n)
* [An alias for Sublime Text on Mac OS](#sublime-text) (s)
* [A GCC alias with errors flags](#gcc) (g)
* [Some useful alias for git](#git) (t, a, r, c, p, amend)
* [Save and Go commands](#save-go) (save, go, saved)

## Installation

**Copy** the content of the file _rc.sh_ in your rc file.<br />
_(**.zshrc**, .bashrc, .bash\_aliases, .profile, etc...)_

## Content

You can change the commands name by changing the alias/function name.

More commands will be added in the future.

### F

#### Command: f

The `f` command is an alias for `grep -r --color=always -C 3`.

Follow `f` by a _string to search_ or the flag `-E` to use regex. 

### Timeout

#### Command: timeout

`timeout` take a shell command in arguments.

The `timeout` command call a shell command and kill it after 10 sec

### 42 Header

#### Command: h

The `h` command update the 42 header of a file.

If the file don't exist, it is created.

The `h` take one required argument: the file name<br />
it takes also a second optional argument: your name

_Warning: don't work for `shell` files or `Makefile`_

### Ls

#### Command: l

The `l` command is an alias for the _ls_ command with a colorful and easy to read output.

![ls](/captures/ls.png)

### Norminette

#### Command: n

The `n` command is an alias for the _norminette_ command with colors.

`n` take an optional argument: A file or a dir name

`n` check all _.c_ and _.h_ files recursively.

### Sublime Text

#### Command: s

The `s` command open Sublime Text.

Can take file name _(existing or not)_ as arguments.

### GCC

#### Command: g

The `g` command is an alias for `gcc -Wall -Werror -Wextra`.

### Git

#### Command: t

The `t` command is an alias for `git status -sb`.<br />
Show the git status in short format.

#### Command: a

The `a` command is an alias for `git add --all` and `t`.<br />
Add file passed as arguments to the git index.<br />
If no arguments is passed, add all files to the index.

#### Command: r

The `r` command is an alias for `git reset HEAD -q` and `t`.<br />
Remove the file from the git index. _(don't remove the file itself)_<br />
_It's the opposite of the `a` command._

#### Command: c

The `c` command is an alias for `git commit -m`.

#### Command: p

The `p` command is an alias for `git push origin master --tags`.

#### Command: amend

The `amend` command is an alias for `git commit --amend --no-edit`.<br />
Add the current status to the last commit.

### Save Go

Save Go allow you to create '_alias_' for working directories.

`save` command save the current _path_ and `go` command move to it.

`save` and `go` take an optional argument: **name**<br />
so you can save infinity of working dir.

Save Go is cross terminal/terminal tab.<br />
