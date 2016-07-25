function install_line()
{
	if grep "$1" "$2" > /dev/null 2> /dev/null; then
		echo "$2 already up to date"
	else
		echo "Update $2"
		echo "
$1" >> "$2"
	fi
}
