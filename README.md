# My Shell Rc

Contains a set of aliases/functions for bash and zsh

Scripts:
- [.juloorc](#juloorc)
- [save-go.sh](#save-gosh)
- [prompt.sh](#promptsh)
- [git_aliases.sh](#git_aliasessh)
- [extra_aliases.sh](#extra_aliasessh)
- [42_aliases.sh](#42_aliasessh)

## Install

First you need to download it

```shell
# Add the scripts you want (coma separated) into the `{}`
# Available: .juloorc, prompt.sh, save-go.sh, git_aliases.sh,
#  extra_aliases.sh, 42_aliases.sh
curl "https://raw.githubusercontent.com/Julow/My-Shell-Rc/master/"{.juloorc,prompt.sh,save-go.sh,git_aliases.sh} > "$HOME/.juloorc"
```

_All the downloaded scripts are saved in a single file: `~/.juloorc`_

Then add this line to your `.bashrc`, `.zshrc` or any shell startup file

```shell
# Load .juloorc
if [[ -f "$HOME/.juloorc" ]]; then
	source "$HOME/.juloorc"
fi
```

To enable it now

```shell
source ~/.juloorc
```

## Content

### `.juloorc`

```shell
rc                  # Reload ~/.juloorc
l[l] [args ...]     # Cool and easy to read ls
s [args ...]        # Sublime Text
```

### `save-go.sh`

Save directory and go to it

```shell
save [name]         # Save a path
go [name]           # Go to a save
saved               # Print save list
```

### `prompt.sh`

A cool prompt with:
- SHLVL (if > 1 only)
- Last command status
- Hostname
- Current working directory
- Git status (including untracked)
- Git revisions (+ and -)

### `git_aliases.sh`

Aliases for git

```shell
t                     # Git status in small format
c <msg> [args ...]    # git commit -m
p [[remote] branch]   # Git push with default arguments (origin master)
pl [[remove] branch]  # Git pull with default arguments (origin master)
amend                 # Amend last commit, no edit
a [files ...]         # git add -A + Print status
r [files ...]         # Cancel an add + Print status (Do not remove any file)
```

### `extra_aliases.sh`

```shell
ps                  # Override ps, show process on other ttys
d <file1> <file2>   # diff with colors
timeout <cmd ...>   # Execute 'cmd' and kill it after 10 seconds
f <search> [dir]    # Recursive grep with colors
```

### `42_aliases.sh`

```shell
n [files ...]       # norminette with colors
h <file> [username] # Add/replace the 42 header in a file
ok                  # Check auteur/Makefile/norminette
```
