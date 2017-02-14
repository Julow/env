# Install some packages

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
		if [[ -d "$dir" ]]
		then
			echo "$dir"
			return
		fi
	done
}

SUBLIME_DIR="$(get_sublime_dir)"

if [[ -z "$SUBLIME_DIR" ]]
then
	echo "No sublime text found"
	exit 1
fi

PACKAGES_DIR="$SUBLIME_DIR/Packages"

for repo in \
	"https://github.com/Julow/Juloo-Sublime-Package.git" \
	"https://github.com/euler0/sublime-glsl.git"
do
	PACKAGE_NAME="${repo##*/}"
	PACKAGE_NAME="$PACKAGES_DIR/${PACKAGE_NAME%.git}"
	if ! [[ -d "$PACKAGE_NAME" ]]
	then
		echo "Install $repo"
		git clone -q "$repo" "$PACKAGE_NAME"
	fi
done
