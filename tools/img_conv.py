#!/usr/bin/env python
# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    img_conv.py                                        :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/09/08 12:13:02 by jaguillo          #+#    #+#              #
#    Updated: 2016/06/27 10:56:57 by jaguillo         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# from PILLOW import Image
from PIL import Image
from sys import argv, exit
from os import path

# main

if len(argv) <= 1:
	print ("Usage: %s <format> <file1> [file2 ... ]" % argv[0])
	exit(1)

i = 2
while i < len(argv):
	src = argv[i]
	dst = "%s.%s" % (path.splitext(src)[0], argv[1])
	try:
		img = Image.open(src)
		try:
			img.convert("RGBA")
			img.save(dst)
		except Exception as e:
			print ("\033[31mError:\033[0m %s: %s" % (dst, str(e)))
	except Exception as e:
		print ("\033[31mError:\033[0m %s: %s" % (src, str(e)))
	i += 1
