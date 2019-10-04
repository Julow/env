# .Xdefaults

cat > "$HOME/.Xdefaults" <<"EOF"
! Font and colors

*.faceName: Fira Code:style=Medium:antialias=true
*.allowBoldFonts: false
*.scaleHeight: 1.01
*.faceSize1: 8
*.faceSize2: 10
*.faceSize3: 12
*.faceSize: 12
*.faceSize4: 14
*.faceSize5: 16
*.faceSize6: 18

! Solarized, generated by terminal.sexy
*.color0:       #002b36
*.color8:       #657b83
*.color1:       #dc322f
*.color9:       #dc322f
*.color2:       #859900
*.color10:      #859900
*.color3:       #b58900
*.color11:      #b58900
*.color4:       #268bd2
*.color12:      #268bd2
*.color5:       #6c71c4
*.color13:      #6c71c4
*.color6:       #2aa198
*.color14:      #2aa198
*color7:       #dfded0
*color15:      #eeeada

! Dark theme
! *.background:	#000000
! *.foreground:	#c3c9c9
! *.cursorColor:	#93a1a1

! Light theme
*.foreground:	#2d393c
*.background:	#fdf6e3
*.cursorColor:	#586e75

! Xterm specific

XTerm.vt100.translations: #override \n\
	Alt <Key> minus: smaller-vt-font() \n\
	Alt <Key> equal: larger-vt-font()

XTerm.vt100.saveLines: 1000
EOF
