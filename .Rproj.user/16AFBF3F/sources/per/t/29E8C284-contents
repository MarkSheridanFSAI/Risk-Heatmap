# Ensure tools package is available
if (!requireNamespace("tools")) install.packages("tools")

# Get names of currently loaded packages
loaded_package_names <- names(sessionInfo()$otherPkgs)

# Find dependencies for each loaded package, including all nested dependencies
all_dependencies <- unique(unlist(
  tools::package_dependencies(loaded_package_names, db = installed.packages(), recursive = TRUE)
))

# Combine loaded packages and their dependencies, removing duplicates
all_relevant_packages <- unique(c(loaded_package_names, all_dependencies))

# Display list of all loaded packages and their dependencies
all_relevant_packages
