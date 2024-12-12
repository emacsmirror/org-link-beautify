#!/usr/bin/env python3

import requests
import ffmpeg
import sys

input_filename = sys.argv[1]
output_filename = sys.argv[2]

def generate_thumbnail(input_filename, output_filename):
    probe = ffmpeg.probe(input_filename)
    time = float(probe['streams'][0]['duration']) // 2
    width = probe['streams'][0]['width']
    try:
        (
            ffmpeg
            .input(input_filename, ss=time)
            .filter('scale', width, -1)
            .output(output_filename, vframes=1)
            .overwrite_output()
            .run(capture_stdout=True, capture_stderr=True)
        )
    except ffmpeg.Error as e:
        print(e.stderr.decode(), file=sys.stderr)
        sys.exit(1)

generate_thumbnail(input_filename, output_filename)
