// Client-Side Swatch Renderer for Paint-o-matic
// Renders color swatches using pure JavaScript for improved performance

(function() {
  'use strict';
  
  // Global SwatchRenderer object
  window.SwatchRenderer = {
    // Save scroll positions before Shiny updates
    savedScrollPositions: {},
    
    // Render all swatches in the data structure
    renderAll: function(containerId, swatchData, config) {
      const container = document.getElementById(containerId);
      if (!container) {
        console.error('Swatch container not found:', containerId);
        return;
      }
      
      // Get saved scroll position if available
      const scrollContainer = document.getElementById('swatch-container');
      const savedScrollTop = this.savedScrollPositions[containerId] || 0;
      if (savedScrollTop > 0) {
        delete this.savedScrollPositions[containerId];
      }
      
      // Clear container
      container.innerHTML = '';
      
      // Handle empty data
      if (!swatchData || (swatchData.type === 'matrix' && (!swatchData.matrices || swatchData.matrices.length === 0))) {
        container.innerHTML = '<p style="text-align: center; padding: 20px; color: #666;">Inga recept tillgängliga.</p>';
        return;
      }
      
      // Render based on type
      if (swatchData.type === 'matrix') {
        this.renderMatrix(container, swatchData.matrices, config);
        
        // Restore scroll position after render
        if (scrollContainer && savedScrollTop > 0) {
          requestAnimationFrame(() => {
            scrollContainer.scrollTop = savedScrollTop;
          });
        }
      } else if (swatchData.type === 'favorites') {
        this.renderFavorites(container, swatchData.favorites, config);
      }
    },
    
    // Render swatch matrices (for RAÄ and Extended palettes)
    renderMatrix: function(container, matrices, config) {
      const fragment = document.createDocumentFragment();
      const wrapper = document.createElement('div');
      wrapper.className = 'swatch-matrices';
      
      // Render each pigment's matrix
      for (const matrix of matrices) {
        // Add pigment heading
        const heading = document.createElement('div');
        heading.style.cssText = 'margin-top: 0.5em; margin-bottom: 0.5em; font-weight: bold;';
        heading.textContent = matrix.base_name;
        wrapper.appendChild(heading);
        
        // Create matrix container
        const matrixDiv = document.createElement('div');
        matrixDiv.className = 'swatch-matrix';
        
        // Group swatches by shade level (rows)
        const swatchesByShade = {};
        for (const swatch of matrix.swatches) {
          const shadeKey = swatch.shade_pct;
          if (!swatchesByShade[shadeKey]) {
            swatchesByShade[shadeKey] = [];
          }
          swatchesByShade[shadeKey].push(swatch);
        }
        
        // Render rows (sorted by shade level)
        const shadeLevels = Object.keys(swatchesByShade).map(Number).sort((a, b) => a - b);
        for (const shadeLevel of shadeLevels) {
          const rowSwatches = swatchesByShade[shadeLevel];
          
          // Sort swatches by vitbas level within row
          rowSwatches.sort((a, b) => a.vitbas_pct - b.vitbas_pct);
          
          const rowDiv = document.createElement('div');
          rowDiv.className = 'swatch-row';
          rowDiv.style.whiteSpace = 'nowrap';
          
          for (const swatch of rowSwatches) {
            const swatchSpan = this.createSwatchElement(swatch);
            rowDiv.appendChild(swatchSpan);
          }
          
          matrixDiv.appendChild(rowDiv);
        }
        
        wrapper.appendChild(matrixDiv);
      }
      
      fragment.appendChild(wrapper);
      container.appendChild(fragment);
    },
    
    // Render favorite swatches (flat list)
    renderFavorites: function(container, favorites, config) {
      if (!favorites || favorites.length === 0) {
        container.innerHTML = `
          <div style="text-align: center; padding: 40px; color: #666;">
            <i class="fa fa-star" style="font-size: 48px; color: #ddd;"></i><br><br>
            <p>Inga sparade favoritkulörer än.</p>
            <p style="font-size: 12px;">Blanda en egen kulör och klicka på 'Spara som favoritkulör' för att spara den här.</p>
          </div>
        `;
        return;
      }
      
      const fragment = document.createDocumentFragment();
      const wrapper = document.createElement('div');
      wrapper.style.marginBottom = '20px';
      
      for (const fav of favorites) {
        const favElement = this.createFavoriteElement(fav);
        wrapper.appendChild(favElement);
      }
      
      fragment.appendChild(wrapper);
      
      // Add clear all button
      const buttonDiv = document.createElement('div');
      buttonDiv.style.cssText = 'margin-top: 20px; text-align: center;';
      buttonDiv.innerHTML = `
        <button id="clear_all_favorites" class="btn btn-default btn-sm action-button">
          <i class="fa fa-trash-alt" style="margin-right: 6px;"></i>
          Rensa alla favoriter
        </button>
      `;
      fragment.appendChild(buttonDiv);
      
      container.appendChild(fragment);
      
      // Bind clear all button
      const clearBtn = container.querySelector('#clear_all_favorites');
      if (clearBtn) {
        clearBtn.addEventListener('click', function() {
          Shiny.setInputValue('clear_all_favorites', Math.random(), {priority: 'event'});
        });
      }
    },
    
    // Create a single swatch element
    createSwatchElement: function(swatch) {
      const span = document.createElement('span');
      span.className = 'kulturkulor-swatch';
      span.style.backgroundColor = swatch.hex_color;
      span.title = swatch.paint_name;
      
      // Add click handler
      span.addEventListener('click', function() {
        SwatchRenderer.handleClick(swatch.code);
      });
      
      return span;
    },
    
    // Create a favorite swatch element with delete button
    createFavoriteElement: function(fav) {
      const container = document.createElement('span');
      container.style.cssText = 'position: relative; display: inline-block; margin: 5px;';
      
      // Create swatch
      const swatch = document.createElement('span');
      swatch.className = 'kulturkulor-swatch';
      swatch.style.cssText = `background-color:${fav.hex_color}; width: 48px; height: 48px;`;
      swatch.title = fav.name || 'Namnlös';
      
      // Add click handler for loading favorite
      swatch.addEventListener('click', function() {
        Shiny.setInputValue('favorite_click', fav.id, {priority: 'event'});
      });
      
      // Create delete button
      const deleteBtn = document.createElement('span');
      deleteBtn.className = 'favorite-delete-btn';
      deleteBtn.style.cssText = 'position: absolute; top: -4px; right: -4px; width: 20px; height: 20px; background: white; border: 1px solid #ccc; border-radius: 50%; cursor: pointer; display: flex; align-items: center; justify-content: center; font-size: 14px; color: #000; box-shadow: 0 2px 4px rgba(0,0,0,0.3); z-index: 20;';
      deleteBtn.title = 'Ta bort favorit';
      deleteBtn.textContent = '×';
      
      // Add delete click handler
      deleteBtn.addEventListener('click', function(event) {
        event.stopPropagation();
        if (typeof deleteFavorite === 'function') {
          deleteFavorite(fav.id);
        }
        return false;
      });
      
      container.appendChild(swatch);
      container.appendChild(deleteBtn);
      
      return container;
    },
    
    // Handle swatch click
    handleClick: function(code) {
      if (window.Shiny && Shiny.setInputValue) {
        Shiny.setInputValue('swatch_click', code, {priority: 'event'});
      }
    }
  };
  
  // Set up Shiny event listeners to save scroll position before updates
  if (typeof window !== 'undefined') {
    window.SwatchRenderer = SwatchRenderer;
    
    // When document is ready, set up Shiny listeners
    $(document).ready(function() {
      // Listen for Shiny recalculating outputs
      $(document).on('shiny:recalculating', function(event) {
        // Check if it's the premade_swatches output being recalculated
        if (event.target && event.target.id === 'premade_swatches') {
          const scrollContainer = document.getElementById('swatch-container');
          if (scrollContainer) {
            // Save the current scroll position
            SwatchRenderer.savedScrollPositions['premade-swatch-target'] = scrollContainer.scrollTop;
          }
        }
      });
    });
  }
})();
