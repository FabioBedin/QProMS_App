# Contenuto di load_packages.R

load_packages <- function(lock_file = "renv.lock") {
  # Read the content of the renv.lock file
  lock_content <- readLines(lock_file)
  
  # Extract the lines containing package names
  package_lines <- grep("^\\s+\"Package\":\\s+\".*\",$", lock_content, value = TRUE)
  
  # Extract the package names
  packages <- gsub("^.*\"Package\":\\s+\"([^\"]+).*\",$", "\\1", package_lines)
  
  # Remove the "box" package from the list of packages
  packages <- packages[packages != "box"]
  
  # Load the packages
  invisible(lapply(packages, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }))
}

# Call the load_packages function
load_packages()
