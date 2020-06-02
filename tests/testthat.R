library(testthat)
library(gfpopgui)

# Run the following before running local tests: --------------------------------
# Start RSelenium server
# system("docker run -d --net=host selenium/standalone-firefox&")
# ------------------------------------------------------------------------------

print("In testthat, starting shiny in background")
system("${R_HOME}/bin/Rscript -e 'library(gfpopgui);options(shiny.port = 11616);run_app()' &")
# Give the system a couple seconds to load app
Sys.sleep(2)

test_check("gfpopgui")
