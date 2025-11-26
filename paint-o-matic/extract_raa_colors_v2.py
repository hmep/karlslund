#!/usr/bin/env python3
"""
Extract masstone RGB values from RAÄ Kulturkulör images
Now fetches actual image URLs from pages to avoid 404 errors
"""

from PIL import Image
import requests
from io import BytesIO
import re

# RAÄ pigment pages
RAA_PAGES = {
    "J225": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/jarnoxidrott/",
    "J920": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/jarnoxidgult/",
    "J318": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/jarnoxidsvart/",
    "BU100": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/brand-umbra/",
    "OU103": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/obrand-umbra/",
    "GU30": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/gron-umbra/",
    "UB88": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/ultramarinblatt/",
    "KG83": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/kromoxidgront/",
    "LO92": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/ljusockra/",
    "GO94": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/guldockra/",
    "J180M": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/caput-mortuum/",
    "ER48A": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/engelskt-rott/",
    "BT44": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/brand-terra/",
    "OT46": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/obrand-terra/",
    "BRU39": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/brun-umbra/",
    "GRAU36": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/gra-umbra/",
    "ZG65": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/zinkgront/",
    "BS98": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/bensvart/",
    "KB28": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/koboltblatt/",
    "J663": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/jarnoxidbrunt/",
    "J120N": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/jarnoxidrott/",  # Same page as J225
    "J686": "https://www.raa.se/kulturarv/byggnader/byggnadsvard/kulturkulor-ett-fargsystem-for-linoljefarg/jarnoxidbrunt/",  # Same as J663
}

HEADERS = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36'
}

def get_image_url_from_page(page_url):
    """
    Scrape the RAÄ page to find the actual image URL
    """
    try:
        response = requests.get(page_url, headers=HEADERS, timeout=10)
        response.raise_for_status()
        
        # Look for image URL in the format: https://www.raa.se/app/uploads/.../pigment-name-1024x307.jpg
        match = re.search(r'https://www\.raa\.se/app/uploads/[^"]+1024x307\.jpg', response.text)
        
        if match:
            return match.group(0)
        else:
            return None
            
    except Exception as e:
        print(f"  Error fetching page: {e}")
        return None

def get_average_rgb(img, x_start, y_start, width=50, height=50):
    """
    Get average RGB from a region to avoid single-pixel noise
    """
    r_sum, g_sum, b_sum = 0, 0, 0
    count = 0
    
    for x in range(x_start, x_start + width):
        for y in range(y_start, y_start + height):
            if x < img.width and y < img.height:
                pixel = img.getpixel((x, y))
                r_sum += pixel[0]
                g_sum += pixel[1]
                b_sum += pixel[2]
                count += 1
    
    if count == 0:
        return (0, 0, 0)
    
    return (
        round(r_sum / count),
        round(g_sum / count),
        round(b_sum / count)
    )

def extract_masstone_color(pigment_id, page_url):
    """
    Get image URL from page, download image, and extract masstone RGB
    """
    try:
        print(f"Processing {pigment_id}...")
        
        # First, get the actual image URL from the page
        image_url = get_image_url_from_page(page_url)
        
        if not image_url:
            print(f"  Could not find image URL on page")
            return None
        
        print(f"  Found image: {image_url.split('/')[-1]}")
        
        # Download the image
        response = requests.get(image_url, headers=HEADERS, timeout=10)
        response.raise_for_status()
        
        img = Image.open(BytesIO(response.content))
        
        # RAÄ images are ~1024x307
        # Masstone stripe is on the left
        # Sample from center-left area to avoid borders
        
        x_sample = 80  # Left area (masstone)
        y_sample = img.height // 2  # Vertical center
        
        # Get average over 50x50 pixel area
        rgb = get_average_rgb(img, x_sample, y_sample, width=50, height=50)
        
        return rgb
        
    except Exception as e:
        print(f"  Error: {e}")
        return None

def main():
    print("=" * 70)
    print("EXTRACTING MASSTONE COLORS FROM RAÄ IMAGES")
    print("=" * 70)
    print()
    
    results = {}
    
    for pigment_id, page_url in RAA_PAGES.items():
        rgb = extract_masstone_color(pigment_id, page_url)
        
        if rgb:
            results[pigment_id] = rgb
            print(f"  ✓ {pigment_id:8s}: RGB{rgb} = #{rgb[0]:02X}{rgb[1]:02X}{rgb[2]:02X}")
        else:
            print(f"  ✗ {pigment_id:8s}: Failed")
        print()
    
    print("=" * 70)
    print("R CODE FOR APP.R")
    print("=" * 70)
    print()
    print("rgb <- list(")
    print('  # VITA BASER')
    print('  "vitbas" = c(245, 245, 245),')
    print('  "44100"  = c(248, 248, 248),')
    print('  "44400"  = c(252, 252, 250),')
    print()
    print('  # RAÄ PIGMENTS (extracted from website images)')
    
    for pigment_id, rgb in sorted(results.items()):
        print(f'  "{pigment_id}" = c({rgb[0]}, {rgb[1]}, {rgb[2]}),')
    
    print(")")
    print()
    print("=" * 70)
    print(f"Successfully extracted {len(results)}/{len(RAA_PAGES)} pigments")
    print("=" * 70)
    print()
    
    # Comparison with Grok's values
    grok_values = {
        "J225": (142, 52, 52),
        "J920": (195, 165, 85),
        "BU100": (90, 60, 45),
        "UB88": (45, 60, 130),
        "KG83": (74, 117, 82),
    }
    
    print("COMPARISON WITH GROK'S VALUES:")
    print("=" * 70)
    for pigment_id, grok_rgb in grok_values.items():
        if pigment_id in results:
            raa_rgb = results[pigment_id]
            diff = tuple(abs(r - g) for r, g in zip(raa_rgb, grok_rgb))
            avg_diff = sum(diff) / 3
            print(f"{pigment_id}:")
            print(f"  RAÄ:  RGB{raa_rgb}")
            print(f"  Grok: RGB{grok_rgb}")
            print(f"  Diff: {diff} (avg: {avg_diff:.1f})")
            print()

if __name__ == "__main__":
    # Check if required libraries are available
    try:
        import PIL
        import requests
    except ImportError:
        print("Please install required libraries:")
        print("  pip install Pillow requests")
        exit(1)
    
    main()
