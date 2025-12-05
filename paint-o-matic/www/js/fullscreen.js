// Fullscreen preview functionality
function openFullscreen(previewId) {
  var preview = document.querySelector("#" + previewId + " .preview");
  if (!preview) return;
  
  var color = window.getComputedStyle(preview).backgroundColor;
  var overlay = document.getElementById("fullscreen-overlay");
  var fullPreview = document.getElementById("fullscreen-preview");
  var colorNameDiv = document.getElementById("fullscreen-color-name");
  
  fullPreview.style.background = color;
  overlay.classList.add("active");
  document.body.style.overflow = "hidden"; // Prevent scrolling
  
  // CRITICAL: Always clear the div first to prevent stale content
  if (colorNameDiv) {
    colorNameDiv.textContent = "";
    colorNameDiv.style.display = "none";
  }
  
  // Get color name from input field (try step 3 first, then step 1)
  var colorName = "";
  var colorNameStep3 = document.getElementById("color_name_step3");
  var colorNameStep1 = document.getElementById("color_name");
  
  if (colorNameStep3 && colorNameStep3.value) {
    colorName = colorNameStep3.value;
  } else if (colorNameStep1 && colorNameStep1.value) {
    colorName = colorNameStep1.value;
  }
  
  // Update color name display (only if there is a name)
  if (colorName && colorNameDiv) {
    colorNameDiv.textContent = colorName;
    colorNameDiv.style.display = "block";
    
    // Set text color based on background luminance
    colorNameDiv.style.color = getTextColorForBackground(color);
  }
}

function closeFullscreen() {
  var overlay = document.getElementById("fullscreen-overlay");
  overlay.classList.remove("active");
  document.body.style.overflow = ""; // Restore scrolling
}

// Close on ESC key
document.addEventListener("keydown", function(e) {
  if (e.key === "Escape") {
    closeFullscreen();
  }
});
