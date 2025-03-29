# Wrap curly brackets around capital letters in .bib file. Ignore capital letters in short month names and entry classes.
# This script assumes that reference names do not contain capital letters (e.g. "@article{R2025,"). Use lowercase letters in reference names.

# Set script working directory to script location
script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(script_dir)

wrap_capitals_in_bib <- function(input_file, output_file) {
  # Read the file with UTF-8 encoding
  lines <- readLines(input_file, warn = FALSE, encoding = "UTF-8")
  
  # Define a regex pattern to match month names
  month_pattern <- "\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\b"
  
  # Function to wrap unprotected capital letters and single-bracketed ones correctly, ignoring those after '@' and month names
  wrap_capitals <- function(line) {
    # Skip processing if the line contains a month name
    if (grepl(month_pattern, line, perl = TRUE)) {
      return(line)
    }
    
    gsub(
      "(?<![{@])([A-Z])(?![}])|(?<![{@])([A-Z])(?=[}])|(?<=[{])([A-Z])(?![}])",  # Match capital letters missing full {} protection, but ignore those after '@'
      "{\\1\\2\\3}",                    # Wrap with full {}
      line,
      perl = TRUE
    )
  }
  
  # Apply the function to each line
  lines <- vapply(lines, wrap_capitals, character(1L), USE.NAMES = FALSE)
  
  # Write back to a new file with UTF-8 encoding
  writeLines(lines, output_file, useBytes = TRUE)
}
# Example usage
wrap_capitals_in_bib("input.bib", "output.bib")
