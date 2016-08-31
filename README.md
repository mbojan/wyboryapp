# wybory

To set up:
* run set_up.R
  * downloads the 2015 election results and processes them into local SQLite database (880mb)
* run the Shiny app in ./visual

Requires development version of leaflet for map labels:
`devtools::install_github('rstudio/leaflet')`