# My Shell Rc

Contains a set of aliases for bash and zsh

Content:
- **save-go** 'Bookmark' for directories
- A cool **prompt**
- Aliases for **git**
- A cool **l** _(ls)_ alias
- And more cool aliases

## Install

First you need to download it

```shell
# Add the scripts you want (coma separated) into the `{}`
# Available: .juloorc (base), prompt.sh, save-go.sh, git_aliases.sh,
#  extra_aliases.sh, 42_aliases.sh
curl "https://raw.githubusercontent.com/Julow/My-Shell-Rc/master/"{.juloorc,prompt.sh,save-go.sh,git_aliases.sh} > "$HOME/.juloorc"
```

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
