
###########################################################################
##### AniMove 2024. Continuous-time movement models #######################
##### Study design ########################################################
###########################################################################

# Install package and dependencies: ---------------------------------------

# # install.packages("remotes")
# remotes::install_github("ecoisilva/movedesign", force = TRUE)
# 
# # or (if above fails):
# install.packages(c("shiny", "fontawesome"))
# shiny::runGitHub("movedesign", "ecoisilva", ref = "main")

# Run Shiny app: ----------------------------------------------------------

library(movedesign)
movedesign::run_app()
