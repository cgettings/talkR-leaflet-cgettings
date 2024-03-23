# `addResetMapButton` from leaflet.extras, but allowing specification of position

addResetMapButtonPosition <- 
    function(map, position = "topleft") {
        map %>% 
            addEasyButton(
                easyButton(
                    icon = "fa-compress",
                    title = "Reset View",
                    onClick = 
                        JS(
                            paste0(
                                "function(btn, map){",
                                    "map.setView(map._initialCenter, map._initialZoom);",
                                "}"
                            )
                        ),
                    position = position
                )) %>%
            htmlwidgets::onRender(
                JS(
                    paste(
                        "function(el, x){",
                            "let map = this;",
                            "map._initialCenter = map.getCenter();",
                            "map._initialZoom = map.getZoom();",
                        "}"
                    )
                )
            )
    }
