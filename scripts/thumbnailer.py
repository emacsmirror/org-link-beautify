#!/usr/bin/env python3

import sys
from thumbnail import generate_thumbnail

input_file = sys.argv[1]
thumbnail_file = sys.argv[2]
thumbnail_size = int(sys.argv[3]) or 300

options = {
	'trim': False,
	# 'height': 300,
	'width': thumbnail_size,
	'quality': 100,
	'type': 'thumbnail' # "thumbnail", "firstpage"
}
generate_thumbnail(input_file, thumbnail_file, options)
