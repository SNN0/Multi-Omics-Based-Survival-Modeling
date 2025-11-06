
# 1. Install BiocManager if it's not already present
if (!require("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

# 2. Define list of required CRAN packages
# Note: 'tidyverse' includes ggplot2, dplyr, stringr, and readxl.
cran_packages <- c(
    "glmnet", 
    "factoextra", 
    "FactoMineR", 
    "caret", 
    "gplots", 
    "survival", 
    "survminer", 
    "RColorBrewer", 
    "tidyverse", 
    "ggfortify", 
    "boot", 
    "patchwork", 
    "gridExtra", 
    "data.table", 
    "doParallel"
)

# 3. Define list of required Bioconductor packages
bioc_packages <- c(
    "TCGAbiolinks", 
    "survcomp"
)

# 4. Install missing CRAN packages
message("Checking for missing CRAN packages...")
for (pkg in cran_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        message(paste("Installing CRAN package:", pkg))
        install.packages(pkg, dependencies = TRUE)
    }
}

# 5. Install missing Bioconductor packages
message("Checking for missing Bioconductor packages...")
for (pkg in bioc_packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        message(paste("Installing Bioconductor package:", pkg))
        BiocManager::install(pkg)
    }
}

# 6. Load all libraries to confirm installation
message("\n--- Setup Complete ---")
message("Loading all required libraries...")

all_packages <- c(cran_packages, bioc_packages, "grid") # 'grid' is base R, just loading.
sapply(all_packages, library, character.only = TRUE)

message("All packages are installed and loaded successfully.")