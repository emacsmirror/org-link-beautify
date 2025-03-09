#!/usr/bin/env python3

## Installation requirements:
# $ pip install ffmpeg-python

## command usage:
## Generate thumbnail from first frame
# $ python thumbnailer-video.py input.mp4 thumbnail.jpg
#
## Generate thumbnail at 30 seconds
# $ python thumbnailer-video.py input.mp4 thumbnail.jpg -t 30
#
## Generate thumbnail with specific width (preserving aspect ratio)
# $ python thumbnailer-video.py input.mp4 thumbnail.jpg -w 640

import argparse
import ffmpeg
import sys
from pathlib import Path

def get_video_info(input_file):
    """Get video metadata using ffprobe"""
    try:
        probe = ffmpeg.probe(input_file)
        video_info = next(s for s in probe['streams'] if s['codec_type'] == 'video')
        return {
            'width': int(video_info['width']),
            'height': int(video_info['height']),
            'duration': float(video_info.get('duration', 0))
        }
    except ffmpeg.Error as e:
        print(f"Error reading video metadata: {e.stderr.decode()}", file=sys.stderr)
        sys.exit(1)

def generate_thumbnail(input_file, output_file, timestamp=0, width=None):
    """Generate thumbnail from video
    
    Args:
        input_file: Path to input video
        output_file: Path to output thumbnail
        timestamp: Time in seconds to capture frame (default: 0 for first frame)
        width: Output width in pixels (maintains aspect ratio)
    """
    try:
        # Get video info
        video_info = get_video_info(input_file)
        
        # Calculate output width while maintaining aspect ratio
        if width:
            height = -1  # Auto calculate height
        else:
            width = video_info['width']
            height = -1

        # Build ffmpeg command
        stream = (
            ffmpeg
            .input(input_file, ss=timestamp)
            .filter('scale', width, height)
            .output(output_file, vframes=1)
            .overwrite_output()
        )
        
        # Execute ffmpeg command
        stream.run(capture_stdout=True, capture_stderr=True)
        
    except ffmpeg.Error as e:
        print(f"Error generating thumbnail: {e.stderr.decode()}", file=sys.stderr)
        sys.exit(1)

def main():
    parser = argparse.ArgumentParser(description='Generate video thumbnail')
    parser.add_argument('input', help='Input video file')
    parser.add_argument('output', help='Output thumbnail file')
    parser.add_argument('-t', '--timestamp', type=float, default=0,
                       help='Timestamp in seconds (default: 0 for first frame)')
    parser.add_argument('-w', '--width', type=int,
                       help='Output width in pixels (maintains aspect ratio)')
    
    args = parser.parse_args()
    
    # Validate input file exists
    if not Path(args.input).is_file():
        print(f"Error: Input file '{args.input}' does not exist", file=sys.stderr)
        sys.exit(1)
        
    generate_thumbnail(args.input, args.output, args.timestamp, args.width)

if __name__ == '__main__':
    main()
