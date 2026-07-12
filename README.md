# poc-shiny-meteocat-xema-daily
## Description

Proof of concept of a Shiny app using `leaflet` and `sf` libraries.

The goal is to show how to draw administrative borders over a leaflet map, capture the click events on them to trigger other actions and manipulate the map with actions triggered from outside of it.

## Data

### Weather measurments

The data used corresponds to daily measurments of automatic weather stations across all Catalonia, a net known as XEMA. More information can be found on the webstite of [Servei Meteorològic de Catalunya](https://www.meteo.cat/).

It has been manually manually downloaded from this [URL](https://analisi.transparenciacatalunya.cat/Medi-Ambient/Dades-meteorol-giques-di-ries-de-la-XEMA/7bvh-jvq2/about_data).

More information about the datasets and similar others can be found [here](https://meteo.cat/wpweb/serveis/dades-obertes/).

### Borders

The borders polygon data has been downloaded from [this](https://github.com/aariste/GeoJSON-Mapas) GitHub repository. Thanks to [@aariste](https://github.com/aariste).
