// Infinite scroll for extended swatches pagination
// Automatically loads more pigments when user scrolls near bottom

(function() {
  'use strict';
  
  let isLoading = false;
  let scrollTimeout = null;
  
  // Threshold: trigger when within 100px of bottom
  const SCROLL_THRESHOLD = 100;
  
  // Debounce delay to avoid excessive triggers
  const DEBOUNCE_DELAY = 200;
  
  // Find the swatch container and monitor for scroll events
  function initInfiniteScroll() {
    // Wait for Shiny to be ready
    $(document).on('shiny:connected', function() {
      // Give the UI time to render
      setTimeout(function() {
        attachScrollListener();
      }, 500);
    });
    
    // Re-attach listener when palette changes
    $(document).on('shiny:value', function(event) {
      if (event.name === 'palette_choice') {
        setTimeout(function() {
          attachScrollListener();
        }, 500);
      }
    });
  }
  
  function attachScrollListener() {
    // Find the swatch container by ID
    const container = $('#swatch-container');
    
    if (container.length === 0) {
      return;
    }
    
    setupScrollHandler(container[0]);
  }
  
  function setupScrollHandler(container) {
    // Remove existing listener if any
    $(container).off('scroll.infiniteScroll');
    
    // Add scroll event listener with debouncing
    $(container).on('scroll.infiniteScroll', function() {
      // Clear previous timeout
      if (scrollTimeout) {
        clearTimeout(scrollTimeout);
      }
      
      // Debounce: wait for user to stop scrolling
      scrollTimeout = setTimeout(function() {
        checkScrollPosition(container);
      }, DEBOUNCE_DELAY);
    });
  }
  
  function checkScrollPosition(container) {
    // Don't trigger if already loading
    if (isLoading) {
      return;
    }
    
    // Check if "Load More" button exists
    const loadMoreBtn = $('#load_more_swatches');
    if (loadMoreBtn.length === 0) {
      // No button means all content is loaded
      return;
    }
    
    // Check if button is disabled (loading in progress)
    if (loadMoreBtn.prop('disabled')) {
      return;
    }
    
    // Calculate scroll position
    const scrollTop = container.scrollTop;
    const scrollHeight = container.scrollHeight;
    const clientHeight = container.clientHeight;
    
    // Distance from bottom
    const distanceFromBottom = scrollHeight - scrollTop - clientHeight;
    
    // Trigger load if within threshold
    if (distanceFromBottom < SCROLL_THRESHOLD) {
      triggerLoadMore();
    }
  }
  
  function triggerLoadMore() {
    const loadMoreBtn = $('#load_more_swatches');
    
    if (loadMoreBtn.length === 0) {
      return;
    }
    
    // Set loading flag
    isLoading = true;
    
    // Click the button programmatically
    loadMoreBtn.click();
    
    // Reset loading flag after a delay
    // This prevents rapid-fire triggering
    setTimeout(function() {
      isLoading = false;
    }, 1000);
  }
  
  // Initialize on page load
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initInfiniteScroll);
  } else {
    initInfiniteScroll();
  }
})();
