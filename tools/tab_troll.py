#!/usr/bin/python

import random
from sys import stdout

ANIMALS = ["Agneau",
	"Alpaga",
	"Antilope",
	"Autruche",
	"Brebis",
	"Canard",
	"Carpe",
	"Castor",
	"Ecureuil",
	"Fourmilier",
	"Gerboise",
	"Hippopotame",
	"Kangourou",
	"Mouton",
	"Poulpe",
	"Ragondin",
	"Renard",
	"Sanglier",
	"Taupe"]

ACTIONS = [
	{"chance": 5, "values": ANIMALS},
	{"chance": 10, "values": ["\x7F", "\x7F\x7F", "\x7F\x7F\x7F\x7F"]},
	{"chance": 4242, "values": ["	"]}
]

r = random.randint(0, 100)

for a in ACTIONS:
	if r > a["chance"]:
		r -= a["chance"]
	else:
		stdout.write(random.choice(a["values"]))
		break
