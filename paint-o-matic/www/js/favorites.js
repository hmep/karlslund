// Favorites management with localStorage
const MAX_FAVORITES = 50;
const STORAGE_KEY = "paintomatic_favorites";

// Get all favorites from localStorage
function getFavorites() {
  try {
    const data = localStorage.getItem(STORAGE_KEY);
    if (!data) return [];
    return JSON.parse(data);
  } catch(e) {
    console.error("Error loading favorites:", e);
    localStorage.removeItem(STORAGE_KEY);
    return [];
  }
}

// Save all favorites to localStorage
function saveFavorites(favorites) {
  try {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(favorites));
    return true;
  } catch(e) {
    console.error("Error saving favorites:", e);
    return false;
  }
}

// Add a new favorite
function addFavorite(favorite) {
  let favorites = getFavorites();
  
  // Check limit
  if (favorites.length >= MAX_FAVORITES) {
    alert("Du har nått gränsen på " + MAX_FAVORITES + " sparade favoriter. Ta bort några för att spara fler.");
    return false;
  }
  
  // Add timestamp and ID
  favorite.id = Date.now().toString();
  favorite.timestamp = new Date().toISOString();
  
  // Add to beginning of array (most recent first)
  favorites.unshift(favorite);
  
  return saveFavorites(favorites);
}

// Delete a favorite by ID
function deleteFavorite(id) {
  let favorites = getFavorites();
  favorites = favorites.filter(f => f.id !== id);
  saveFavorites(favorites);
  
  // Update Shiny with new list
  Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
  Shiny.setInputValue("favorites_updated", Math.random(), {priority: "event"});
}

// Clear all favorites
function clearAllFavorites() {
  if (confirm("Är du säker på att du vill ta bort alla sparade favoriter?")) {
    localStorage.removeItem(STORAGE_KEY);
    Shiny.setInputValue("favorites_list", JSON.stringify([]));
    Shiny.setInputValue("favorites_updated", Math.random(), {priority: "event"});
  }
}

// Send favorites to Shiny when connected
$(document).on("shiny:connected", function() {
  Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
});

// Custom message handlers
Shiny.addCustomMessageHandler("save_favorite", function(favorite) {
  if (addFavorite(favorite)) {
    Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
  }
});

Shiny.addCustomMessageHandler("clear_all_favorites", function(msg) {
  clearAllFavorites();
  Shiny.setInputValue("favorites_list", JSON.stringify(getFavorites()));
});
