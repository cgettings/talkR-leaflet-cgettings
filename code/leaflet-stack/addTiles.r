addTiles <- function(
    map,
    urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution = NULL,
    layerId = NULL,
    group = NULL,
    options = tileOptions(),
    data = getMapData(map)
) {
    options$attribution <- attribution
    if (missing(urlTemplate) && is.null(options$attribution))
        options$attribution <- paste(
            "&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap</a>, ",
            "<a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL</a>"
        )
    invokeMethod(map, data, "addTiles", urlTemplate, layerId, group,
        options)
}
