methods.addTiles = function(urlTemplate, layerId, group, options) {
    this.layerManager.addLayer(L.tileLayer(urlTemplate, options), "tile", layerId, group);
};
