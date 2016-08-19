# wybory

To set up:
* run set_up.R
  * downloads the 2015 election results and processes them into local SQLite database (880mb)
* download maps of Poland from ./visual/data/maps in the repository
  **optional: instead of downloading you can edit set_up.R and pass 'maps=TRUE' to 'set_up()' to have them processed from scratch
* run the Shiny app in ./visual
