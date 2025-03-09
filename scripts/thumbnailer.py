#!/usr/bin/env python3

## installation requirements:
# pip install thumbnail

## usage:
# python thumbnailer.py input_file thumbnail.jpg [thumbnail_size]

import sys
from thumbnail import generate_thumbnail

print(sys.argv)
input_file = sys.argv[1]
thumbnail_file = sys.argv[2]
if len(sys.argv) == 4:
    thumbnail_size = int(sys.argv[3])
elif len(sys.argv) == 3:
    thumbnail_size = 300

options = {
	'trim': False,
	# 'height': 300,
	'width': thumbnail_size,
	'quality': 100,
	'type': 'thumbnail' # "thumbnail", "firstpage"
}
generate_thumbnail(input_file, thumbnail_file, options)
