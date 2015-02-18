# My Shell Rc

My .bashrc config

A set of cool commands/alias for Linux and Mac OS.

## Installation

**Copy** the **.bashrc** file.<br />
Can have a different name depending on your shell.<br />
_(**.zshrc**, .profile, .cshrc, .shinit, etc...)_

## Content

### PS1

A colored prompt containing:
- Last command status
- Git branch
- Git status
- Git rev diff count

### Timeout

`timeout` take a shell command in arguments.

The `timeout` command call a shell command and kill it after 10 sec

### Save Go

Save Go allow you to create '_alias_' for working directories.

`save` command save the current _path_ and `go` command move to it.

`save` take following options:
- `save -g (name)` Like `go`.
- `save -i (name)` Print the path of `(name)`
- `save -l` List saves
- `save -r (name)` Remove save
- `save (name)` Create save `(name)`
- `save -h` Print more help

The argument `(name)` is optionnal.

Save Go is cross terminal/tab.

### Ls

The `l` command is an alias for the _ls_ command with a colorful and easy to read output.

![ls](/captures/ls.png)

### Rc

The `rc` command is just an alias for `source ~/.bashrc`

### Grep

The `f` command is an alias for `grep -r --color=always -C 3`.

Follow `f` by a _string to search_ or the flag `-E` to use regex. 

### 42 Header

The `h` command update the 42 header of a file.

If the file don't exist, it is created.

The `h` take one required argument: the file name<br />
it takes also a second optional argument: your name

_Warning: don't work for `shell` files or `Makefile`_

### Norminette

The `n` command is an alias for the _norminette_ command with colors.

`n` take an optional argument: A file or a dir name

`n` check all _.c_ and _.h_ files recursively.

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
