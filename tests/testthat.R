library(testthat)
library(gfpopgui)

# Run the following before running local tests: --------------------------------
# Start RSelenium server
# system("docker run -d --net=host selenium/standalone-firefox&")
# ------------------------------------------------------------------------------

test_check("gfpopgui")