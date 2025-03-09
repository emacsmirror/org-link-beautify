#!/usr/bin/env python3

# Installation requirements:
# $ pip3 install Pillow

# This script is from "epub-thumbnailer" project command script.

from io import BytesIO
from pathlib import Path
from typing import Optional, List, Callable
from urllib.request import urlopen
import sys
import zipfile
import xml.etree.ElementTree as ET

from PIL import Image

class EpubCoverExtractor:
    """Extract and resize cover images from EPUB files."""
    
    def __init__(self, input_path: str, output_path: Path, size: int):
        self.input_path = input_path
        self.output_path = output_path
        self.size = size
        self.epub: Optional[zipfile.ZipFile] = None
    
    def load_epub(self) -> None:
        """Load EPUB file from path or URL."""
        if Path(self.input_path).is_file():
            file_data = Path(self.input_path).read_bytes()
        else:
            file_data = urlopen(self.input_path).read()
        self.epub = zipfile.ZipFile(BytesIO(file_data), "r")

    def get_cover_from_manifest(self) -> Optional[str]:
        """Extract cover path from EPUB manifest."""
        if not self.epub:
            return None
            
        try:
            container = self.epub.read("META-INF/container.xml")
            container_tree = ET.fromstring(container)
            rootfile = container_tree.find(".//{urn:oasis:names:tc:opendocument:xmlns:container}rootfile")
            if rootfile is None or (rootfile_path := rootfile.get("full-path")) is None:
                return None
            
            content = self.epub.read(rootfile_path)
            content_tree = ET.fromstring(content)
            
            # Find cover image reference in manifest
            images = []
            for item in content_tree.findall(".//{http://www.idpf.org/2007/opf}item"):
                if item.get("id", "").lower().startswith("cover"):
                    return item.get("href")
                if item.get("media-type", "").startswith("image/"):
                    images.append(item.get("href"))
                    
            return images[0] if images else None
            
        except Exception:
            return None

    def get_cover_by_guide(self) -> Optional[str]:
        """Extract cover path from EPUB guide."""
        if not self.epub:
            return None
            
        try:
            container = self.epub.read("META-INF/container.xml")
            container_tree = ET.fromstring(container)
            rootfile = container_tree.find(".//{urn:oasis:names:tc:opendocument:xmlns:container}rootfile")
            if rootfile is None or (rootfile_path := rootfile.get("full-path")) is None:
                return None
                
            content = self.epub.read(rootfile_path)
            content_tree = ET.fromstring(content)
            
            # Check guide reference
            guide_ref = content_tree.find(".//{http://www.idpf.org/2007/opf}reference[@type='cover']")
            if guide_ref is not None:
                return guide_ref.get("href")
                
        except Exception:
            return None
        
        return None

    def get_cover_by_filename(self) -> Optional[str]:
        """Find cover image by common filename patterns."""
        if not self.epub:
            return None
            
        cover_patterns = [
            "cover.jpg", "cover.jpeg", "cover.png",
            "Cover.jpg", "Cover.jpeg", "Cover.png"
        ]
        
        images = []
        for filename in self.epub.namelist():
            if any(pattern in filename for pattern in cover_patterns):
                images.append(self.epub.getinfo(filename))
                
        return max(images, key=lambda f: f.file_size).filename if images else None

    def extract_and_save_cover(self, cover_path: str) -> bool:
        """Extract, resize and save cover image."""
        if not cover_path or not self.epub:
            return False
            
        try:
            cover_data = self.epub.read(cover_path)
            image = Image.open(BytesIO(cover_data))
            image.thumbnail((self.size, self.size), Image.Resampling.LANCZOS)
            
            if image.mode == "CMYK":
                image = image.convert("RGB")
                
            image.save(self.output_path, "PNG")
            return True
            
        except Exception as e:
            print(f"Error saving cover: {e}", file=sys.stderr)
            return False

    def extract(self) -> bool:
        """Try all strategies to extract cover image."""
        self.load_epub()
        
        strategies: List[Callable] = [
            self.get_cover_from_manifest,
            self.get_cover_by_guide, 
            self.get_cover_by_filename
        ]
        
        for strategy in strategies:
            try:
                if cover_path := strategy():
                    if self.extract_and_save_cover(cover_path):
                        return True
            except Exception as e:
                print(f"Error with {strategy.__name__}: {e}", file=sys.stderr)
                
        return False

def parse_args() -> tuple:
    """Parse command line arguments."""
    if len(sys.argv) != 4:
        print("Usage: thumbnailer-epub.py <input> <output> <size>", file=sys.stderr)
        sys.exit(1)
        
    input_path = sys.argv[1]
    output_path = Path(sys.argv[2])
    
    try:
        size = int(sys.argv[3])
        if size <= 0:
            raise ValueError("Size must be positive")
    except ValueError as e:
        print(f"Invalid size: {e}", file=sys.stderr)
        sys.exit(1)
        
    return input_path, output_path, size

def main() -> None:
    """Main entry point."""
    input_path, output_path, size = parse_args()
    
    extractor = EpubCoverExtractor(input_path, output_path, size)
    if not extractor.extract():
        print("Failed to extract cover image", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
