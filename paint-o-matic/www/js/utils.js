// Helper function to calculate luminance and choose text color
function getTextColorForBackground(bgColor) {
  // Parse RGB from background color string
  var rgb = bgColor.match(/\d+/g);
  if (!rgb || rgb.length < 3) return "white";
  
  var r = parseInt(rgb[0]);
  var g = parseInt(rgb[1]);
  var b = parseInt(rgb[2]);
  
  // Calculate relative luminance (WCAG formula)
  var luminance = (0.299 * r + 0.587 * g + 0.114 * b) / 255;
  
  // Return black for light backgrounds, white for dark backgrounds
  return luminance > 0.5 ? "black" : "white";
}
