#!/usr/bin/env python3

## installation requirements:
# $ pip install thumbnail

## usage:
# $ python thumbnailer.py input_file thumbnail.jpg [options]

import argparse
from pathlib import Path
import sys
from typing import Dict, Any
from thumbnail import generate_thumbnail

def parse_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description='Generate thumbnails from input files')
    parser.add_argument('input', type=str, help='Input file path')
    parser.add_argument('output', type=str, help='Output thumbnail path')
    parser.add_argument('-s', '--size', type=int, default=300,
                       help='Thumbnail width in pixels (default: 300)')
    parser.add_argument('--trim', action='store_true',
                       help='Trim whitespace from thumbnail')
    parser.add_argument('-q', '--quality', type=int, default=100,
                       help='Output image quality (1-100)')
    parser.add_argument('-t', '--type', choices=['thumbnail', 'firstpage'],
                       default='thumbnail', help='Type of output')
    return parser.parse_args()

def validate_inputs(input_path: str, output_path: str) -> None:
    """Validate input and output paths."""
    if not Path(input_path).exists():
        sys.exit(f"Error: Input file '{input_path}' does not exist")
    
    output_dir = Path(output_path).parent
    if not output_dir.exists():
        sys.exit(f"Error: Output directory '{output_dir}' does not exist")

def get_thumbnail_options(args: argparse.Namespace) -> Dict[str, Any]:
    """Build thumbnail generation options from arguments."""
    return {
        'trim': args.trim,
        'width': args.size,
        'quality': args.quality,
        'type': args.type
    }

def main() -> None:
    """Main entry point."""
    args = parse_args()
    
    validate_inputs(args.input, args.output)
    
    options = get_thumbnail_options(args)
    
    try:
        generate_thumbnail(args.input, args.output, options)
    except Exception as e:
        sys.exit(f"Error generating thumbnail: {e}")

if __name__ == '__main__':
    main()
