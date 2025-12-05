# Testing Checklist for v0.10.5-pwa

This checklist should be completed when testing the refactored Paint-o-matic app in a live R/Shiny environment.

## Basic Functionality Tests

### App Loading
- [ ] App loads without errors
- [ ] No console errors in browser DevTools
- [ ] Version number shows "v0.10.5-pwa" in header

### Visual Appearance
- [ ] All styles render correctly (compare with previous version)
- [ ] Layout looks identical to v0.10.4-db
- [ ] Colors and spacing are correct
- [ ] Responsive design works on mobile
- [ ] All UI elements are properly styled

### Core Features
- [ ] Color mixing works correctly
- [ ] Preview circles display proper colors
- [ ] Pigment selection dropdowns work
- [ ] Percentage sliders function
- [ ] Recipe calculations are accurate
- [ ] All steps (1-4) are accessible
- [ ] Navigation buttons work

### Fullscreen Preview
- [ ] Zoom icon appears on preview circles
- [ ] Click zoom icon opens fullscreen overlay
- [ ] Fullscreen shows correct color
- [ ] Color name displays when set
- [ ] Text color is readable on all backgrounds (black on light, white on dark)
- [ ] ESC key closes fullscreen
- [ ] Click overlay closes fullscreen
- [ ] X button closes fullscreen

### Favorites Management
- [ ] "Spara som favoritkulör" button works
- [ ] Favorites save to localStorage
- [ ] Favorites persist after page reload
- [ ] Favorites list displays correctly
- [ ] Can load favorite recipes
- [ ] Can delete individual favorites
- [ ] "Rensa alla favoriter" clears all favorites
- [ ] Limit of 50 favorites enforced
- [ ] Swedish alert messages display correctly

### URL Sharing
- [ ] Share URL button generates correct URLs
- [ ] URLs load with correct parameters
- [ ] Color names preserved in URLs
- [ ] All pigment selections preserved

## External Resource Tests

### CSS Loading
- [ ] `www/css/custom.css` loads successfully (check Network tab)
- [ ] No 404 errors for CSS file
- [ ] Styles are applied correctly

### JavaScript Loading
- [ ] `www/js/utils.js` loads successfully
- [ ] `www/js/fullscreen.js` loads successfully
- [ ] `www/js/favorites.js` loads successfully
- [ ] `www/service-worker-register.js` loads successfully
- [ ] No 404 errors for JS files
- [ ] No JavaScript errors in console

### Icon Loading
- [ ] `www/icons/icon-192.png` is accessible
- [ ] `www/icons/icon-512.png` is accessible
- [ ] Icons display in PWA install prompt

## PWA Functionality Tests

### Manifest
- [ ] `/manifest.json` is accessible
- [ ] Manifest is valid (check DevTools → Application → Manifest)
- [ ] All manifest fields are correct
- [ ] Icons are correctly referenced

### Service Worker
- [ ] Service worker registers successfully
- [ ] Console shows "Service Worker registered" message
- [ ] Service worker visible in DevTools → Application → Service Workers
- [ ] Service worker status is "activated"

### Installation
- [ ] **Desktop (Chrome/Edge)**: Install icon (+) appears in address bar
- [ ] **Desktop**: Click install icon shows app info and "Install" button
- [ ] **Desktop**: Installation completes successfully
- [ ] **Desktop**: App opens in standalone window (no browser UI)
- [ ] **Mobile (Chrome)**: "Add to Home Screen" option available
- [ ] **Mobile (Safari)**: "Add to Home Screen" option available
- [ ] **Mobile**: Installation completes successfully
- [ ] **Mobile**: App icon appears on home screen
- [ ] **Mobile**: Tap icon opens app in standalone mode

### Offline Functionality
- [ ] Visit app while online (cache assets)
- [ ] Verify service worker cache populated (DevTools → Application → Cache Storage)
- [ ] Check "Offline" mode in DevTools
- [ ] Reload page - cached assets (CSS, JS, icons) load
- [ ] App renders basic UI (note: dynamic content requires network)
- [ ] Styles still apply
- [ ] JavaScript functions work
- [ ] Icons display

### PWA Experience
- [ ] Theme color (#333) applies to browser UI
- [ ] App runs without browser address bar (standalone)
- [ ] App behaves like native application
- [ ] Navigation works correctly

## Browser Compatibility

Test in multiple browsers:

### Desktop
- [ ] **Chrome** (Windows/Mac/Linux)
- [ ] **Edge** (Windows/Mac)
- [ ] **Firefox** (Windows/Mac/Linux)
- [ ] **Safari** (Mac)

### Mobile
- [ ] **Chrome** (Android)
- [ ] **Safari** (iOS)
- [ ] **Samsung Internet** (Android)

## Performance Tests

- [ ] Initial page load time acceptable
- [ ] No significant performance degradation vs v0.10.4-db
- [ ] CSS loads quickly
- [ ] JavaScript loads quickly
- [ ] Service worker doesn't impact performance

## Regression Tests

Compare with v0.10.4-db to ensure no functionality lost:

- [ ] All pigments still available
- [ ] All calculations identical
- [ ] All UI behaviors identical
- [ ] No new bugs introduced
- [ ] Edge cases still handled

## Developer Experience Tests

- [ ] Can modify CSS in `www/css/custom.css`
- [ ] CSS changes apply after reload
- [ ] Can modify JavaScript in `www/js/*.js`
- [ ] JS changes apply after reload
- [ ] Can update manifest without editing `app.R`
- [ ] Can replace icons without editing `app.R`

## Documentation Tests

- [ ] `www/README.md` is accurate and helpful
- [ ] `PWA_REFACTORING.md` is accurate
- [ ] `README_STRUCTURE.md` includes www/ section
- [ ] All documentation is up-to-date

## Edge Cases

- [ ] Very long color names in fullscreen
- [ ] Maximum favorites (50) behavior
- [ ] Empty favorites list
- [ ] No color name set
- [ ] Special characters in color names
- [ ] Very small/large screen sizes

## Security Tests

- [ ] No console warnings about mixed content (HTTP/HTTPS)
- [ ] Service worker only runs on HTTPS (or localhost)
- [ ] No XSS vulnerabilities in color names
- [ ] localStorage doesn't contain sensitive data
- [ ] No security warnings in browser

## Cleanup Tests

- [ ] No temporary files committed
- [ ] No unnecessary dependencies added
- [ ] No debug code left in
- [ ] Git repository clean

## Final Verification

- [ ] All tests passed
- [ ] No critical issues found
- [ ] App ready for production
- [ ] Documentation complete
- [ ] Version number correct

---

## Notes

Record any issues, observations, or recommendations here:

**Issues Found**:
- 

**Performance Notes**:
- 

**Browser-Specific Issues**:
- 

**Recommendations**:
- 

---

**Tested By**: _______________  
**Date**: _______________  
**Environment**: _______________  
**Result**: ☐ Pass ☐ Fail ☐ Pass with Issues
