
# Data prep ---------------------------------------------------------------

# We have a separate R Markdown file for just the data prep, so that the main
# analysis isn't so cluttered with the data source information and cleaning
# code. So we'll want to export this separately first.


# Render the document!
rmarkdown::render(
  "prep-data.Rmd",
  output_file = "docs/prep-data.html"
)


# Analysis ----------------------------------------------------------------

# Render our main analysis document with a given set of parameters

# Should the margin of error columns be hidden from the tables?
hide_moe <- TRUE

# How many iterations should be used for the random assignment of job loss
iterations <- 100

# Create the output filename
moe_yesno <- ifelse(hide_moe, "no", "yes")
out_file <- glue::glue("docs/analysis_moe-{moe_yesno}_rep-{iterations}.html")

# Render the document!
rmarkdown::render(
  "analysis.Rmd",
  output_file = out_file,
  params = list(
    hide_moe = hide_moe, 
    iterations = iterations
  )
)
