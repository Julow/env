# env

### install.sh

```sh
bash install.sh #<args>
```

Args are of the form `<conf_name>:<param>`

Default args are: `alias:base,git,save-go rc:.bashrc?,.zshrc? brew:? tools prompt`

Args:
* `alias` Load alias files in `aliases/`
* `prompt` Load a cool prompt
* `rc` Put `source "<path to _init.sh>"` into any file
	Args: <file path: absolute or relative to $HOME><`?`: do nothing if file does not exists>
* `tools` Put `tools/` dir into the `$PATH`
* `brew` Export some variable for a working homebrew
	Arg: <brew dir: absolute or relative to $HOME><`?`: do nothing if brew dir does not exists>
* `kill_itune` Kill itune daemon (mac os)
* `default` Load default

Output to `_init.sh`

### docker

A simple Dockerfile + a script

### aliases/tools

Custom aliases/tools
