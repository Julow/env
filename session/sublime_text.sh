# Install some packages

PACKAGES="
	https://github.com/Julow/Juloo-Sublime-Package.git
	https://github.com/euler0/sublime-glsl.git
	https://github.com/P233/Syntax-highlighting-for-Sass
	https://github.com/asbjornenge/Docker.tmbundle
	https://github.com/sabhiram/sublime-clipboard-diff
	https://github.com/dempfi/ayu
	https://github.com/alek-sys/sublimetext_indentxml
	https://github.com/brandonwamboldt/sublime-nginx
	https://github.com/SublimeText-Markdown/MarkdownEditing
	https://github.com/zyxar/Sublime-CMakeLists
"

function get_sublime_dir
{
	for dir in \
		"$HOME/Library/Application Support/Sublime Text 3" \
		"$HOME/Library/Application Support/Sublime Text 2" \
		"$HOME/Library/Application Support/Sublime Text" \
		"$HOME/.config/sublime-text-3" \
		"$HOME/.config/sublime-text-2" \
		"$HOME/.config/sublime-text"
	do
		if [[ -d "$dir" ]]; then
			echo "$dir"
			return
		fi
	done
}

SUBLIME_DIR="$(get_sublime_dir)"

if [[ -z "$SUBLIME_DIR" ]]; then
	exit 100
fi

PACKAGES_DIR="$SUBLIME_DIR/Packages"

RET=101

for repo in $PACKAGES; do
	PACKAGE_NAME="${repo##*/}"
	PACKAGE_NAME="${PACKAGE_NAME%.git}"
	PACKAGE_DST="$PACKAGES_DIR/$PACKAGE_NAME"
	if ! [[ -d "$PACKAGE_DST" ]]
	then
		echo "Install $PACKAGE_NAME"
		git clone -q "$repo" "$PACKAGE_DST"
		RET=0
	fi
done

exit $RET
