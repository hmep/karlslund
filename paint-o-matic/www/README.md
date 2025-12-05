# Paint-o-matic Web Resources

This directory contains static web resources for the Paint-o-matic Shiny application, including CSS, JavaScript, icons, and Progressive Web App (PWA) support files.

## Directory Structure

```
www/
├── css/
│   └── custom.css                  # All application styles
├── js/
│   ├── utils.js                    # Utility functions (color calculations)
│   ├── fullscreen.js               # Fullscreen preview functionality
│   └── favorites.js                # localStorage favorites management
├── icons/
│   ├── icon-192.png               # PWA icon (192×192px)
│   └── icon-512.png               # PWA icon (512×512px)
├── manifest.json                   # PWA manifest file
├── service-worker.js               # Service worker for offline caching
└── service-worker-register.js      # Service worker registration script
```

## File Descriptions

### CSS Files

#### `css/custom.css`
Contains all application styles including:
- Layout styles (`.content-wrapper`, `.step`, `.footer-ref`, `.ready-box`)
- Preview and swatch styles (`.preview`, `.kulturkulor-swatch`, `.kulturkulor-gallery`)
- Box and alert styles (`.normalized-box`, `.info-box`, `.paint-type-box`)
- Button styles (`.btn`, `.btn-primary`, `.back-btn`, `.next-btn`)
- Table styles
- Fullscreen preview styles (`.preview-container`, `.zoom-icon`, `.fullscreen-overlay`, etc.)

**How to modify:** Edit this file directly. Changes will be reflected when the app is reloaded.

### JavaScript Files

#### `js/utils.js`
Contains utility functions:
- `getTextColorForBackground(bgColor)` - Calculates text color (black/white) based on background luminance using WCAG formula

#### `js/fullscreen.js`
Handles fullscreen preview functionality:
- `openFullscreen(previewId)` - Opens color preview in fullscreen mode
- `closeFullscreen()` - Closes fullscreen preview
- ESC key event listener for closing fullscreen

#### `js/favorites.js`
Manages favorites using browser localStorage:
- `MAX_FAVORITES` - Maximum number of favorites (50)
- `STORAGE_KEY` - localStorage key ("paintomatic_favorites")
- `getFavorites()` - Retrieve all favorites
- `saveFavorites(favorites)` - Save favorites array
- `addFavorite(favorite)` - Add new favorite
- `deleteFavorite(id)` - Remove favorite by ID
- `clearAllFavorites()` - Clear all saved favorites
- Shiny integration handlers

**How to modify:** Edit the relevant JS file. Changes will be reflected when the app is reloaded.

### PWA Support Files

#### `manifest.json`
PWA manifest file that defines:
- App name and description
- Display mode (standalone)
- Theme colors (background: #cccccc, theme: #333333)
- Icons for different sizes
- Orientation preference (portrait)

**How to modify:** Edit `manifest.json` to change PWA metadata. Update icon paths if you replace icons.

#### `service-worker.js`
Service worker that enables offline functionality:
- Caches CSS, JS, and icon files on install
- Serves cached resources when offline
- Cleans up old caches on activation
- Cache name: `paint-o-matic-v1`

**How to modify:** 
- Update `CACHE_NAME` when making breaking changes
- Add/remove files from `ASSETS_TO_CACHE` array as needed
- Clear browser cache and unregister service worker during development

#### `service-worker-register.js`
Registers the service worker when the app loads. This file is loaded in `app.R`.

**How to modify:** Generally no need to modify unless changing service worker file path.

### Icons

#### `icons/icon-192.png` and `icons/icon-512.png`
PWA icons shown when:
- Installing the app to home screen
- App switcher/task manager
- PWA install prompt

The current icons feature a paint palette with colorful swatches on a dark background (#333333).

**How to replace:**
1. Create PNG files with exact dimensions (192×192px and 512×512px)
2. Replace existing files in `www/icons/`
3. Use design that matches app theme
4. Ensure icons are recognizable at small sizes

## PWA Functionality

### What is a PWA?
A Progressive Web App allows Paint-o-matic to be installed on devices and work offline.

### Features Enabled
- **Installable**: Users can install the app to their home screen
- **Offline support**: Cached CSS, JS, and icons work without internet
- **App-like experience**: Runs in standalone mode without browser UI
- **Theme integration**: Uses device theme colors (#333333)

### Testing PWA Features

#### Chrome/Edge Desktop:
1. Open app in browser
2. Look for install icon (+) in address bar
3. Click to install as standalone app
4. Check Console for "Service Worker registered" message

#### Chrome/Safari Mobile:
1. Open app in browser
2. Tap Share/Menu → "Add to Home Screen"
3. App will install as standalone app
4. Open from home screen to test

#### Verify Offline:
1. Visit app once (caches resources)
2. Open DevTools → Application → Service Workers
3. Check "Offline" mode
4. Reload page - cached resources should load
5. Note: Dynamic content requires network

### Development Tips

#### Clear Service Worker Cache:
When making changes during development, you may need to clear the service worker cache:

1. Open DevTools → Application → Service Workers
2. Click "Unregister" next to service worker
3. Clear browser cache (Ctrl+Shift+Del)
4. Hard reload page (Ctrl+Shift+R)

Or programmatically:
```javascript
navigator.serviceWorker.getRegistrations().then(function(registrations) {
  for(let registration of registrations) {
    registration.unregister();
  }
});
```

#### Update Cache Version:
When making changes to cached files, update `CACHE_NAME` in `service-worker.js`:
```javascript
const CACHE_NAME = 'paint-o-matic-v2'; // Increment version
```

## Integration with app.R

The `app.R` file loads these resources in the UI:

```r
tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css"),
  tags$link(rel = "manifest", href = "manifest.json"),
  tags$meta(name = "theme-color", content = "#333333"),
  # ... PWA meta tags
)

tags$script(src = "js/utils.js"),
tags$script(src = "js/fullscreen.js"),
tags$script(src = "js/favorites.js"),
tags$script(src = "service-worker-register.js"),
```

Shiny automatically serves files from the `www/` directory.

## Best Practices

1. **Keep CSS organized** - Use comments to separate sections
2. **Minimize JavaScript** - Keep functions focused and well-named
3. **Test PWA features** - Verify install and offline functionality after changes
4. **Version cache** - Update cache name when making breaking changes
5. **Optimize icons** - Use compressed PNG files to reduce app size
6. **Document changes** - Update this README when modifying structure

## Troubleshooting

### Styles not loading:
- Check browser console for 404 errors
- Verify file path matches href in app.R
- Hard reload page (Ctrl+Shift+R)

### JavaScript not working:
- Check browser console for errors
- Verify functions are defined before use
- Check script load order in app.R

### PWA not installing:
- Verify manifest.json is valid (check DevTools → Application → Manifest)
- Ensure icons exist and have correct dimensions
- Check service worker registration (DevTools → Application → Service Workers)
- HTTPS required for PWA (or localhost)

### Offline not working:
- Visit app once to cache resources
- Check service worker is active
- Verify resources are in cache (DevTools → Application → Cache Storage)
- Check `ASSETS_TO_CACHE` includes necessary files

## Version History

- **v0.10.5-pwa** (2025-12-05): Initial refactoring to extract CSS/JS and add PWA support
  - Extracted inline CSS to `css/custom.css`
  - Extracted inline JavaScript to separate files
  - Added PWA manifest and service worker
  - Created app icons
