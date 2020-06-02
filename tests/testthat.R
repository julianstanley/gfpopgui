library(testthat)
library(gfpopgui)

# Start RSelenium server
# system("docker run -d --net=host selenium/standalone-firefox&")

# Start app
#system("Rscript -e 'golem::detach_all_attached();golem::document_and_reload();options(shiny.port = 11616);run_app()' &")

test_check("gfpopgui")
