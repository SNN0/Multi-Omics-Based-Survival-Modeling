# Survival Analysis of Chemoresistance in TCGA Cohorts

![R Language](https://img.shields.io/badge/Language-R-blue.svg)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

The analysis identifies prognostic gene signatures associated with sensitivity or resistance to specific chemotherapeutic agents (Paclitaxel, 5-Fluorouracil, Gemcitabine) by integrating multi-omic data with patient survival outcomes.

The primary methods used include **Penalized Cox Regression** (Ridge via `glmnet`) from high-dimensional data, multi-gene Cox models for pathway-level analysis, and Kaplan-Meier survival curves for visualization.

## Table of Contents

1.  [Project Overview](#project-overview)
2.  [Analysis Pipeline](#analysis-pipeline)
3.  [Repository Structure](#repository-structure)
4.  [Setup and Installation](#setup-and-installation)
5.  [How to Run the Analysis](#how-to-run-the-analysis)
    * [Option 1: Load Pre-computed Models (Recommended)](#option-1-load-pre-computed-models-recommended)
    * [Option 2: Run the Full Analysis (Computationally Intensive)](#option-2-run-the-full-analysis-computationally-intensive)
6.  [Analysis Notebooks Overview](#analysis-notebooks-overview)

## Project Overview

* **Data:** TCGA multi-omic data (Gene Expression, Mutation [MUT], and Copy Number Alteration [CNA]) is used.
* **Cohorts & Drugs:**
    * **Paclitaxel:** All applicable cohorts (BRCA, HNSC, STAD, LUAD, UCS) & BRCA-only.
    * **5-Fluorouracil (5-FU):** All applicable cohorts (STAD, PAAD, ESCA, READ) & STAD-only.
    * **Gemcitabine:** All applicable cohorts (PAAD, PCPG, LIHC, LUSC, SARC) & PAAD-only.
* **Models:**
    * **Model 1 (CNA-based):** Penalized Cox model using **Expression (expr) + CNA + Condition** (Sensitive/Resistant) + **CNA:Condition Interaction**.
    * **Model 2 (Mutation-based):** Penalized Cox model using **Expression (expr) + Mutation (mut) + Condition** (Sensitive/Resistant) + **Mutation:Condition Interaction**.
    * **Multi-Gene Models:** Pathway-based penalized Cox models derived from the results of Model 1 and Model 2.

## Analysis Pipeline

Each notebook follows a consistent analytical workflow:

1.  **Data Ingestion & Preprocessing:** Loads the respective EMC (`.tsv`) files for sensitive and resistant samples.
2.  **Gene Symbol Standardization:** Applies a conversion table to standardize gene names.
3.  **Clinical Data Integration:** Fetches corresponding clinical data from TCGAbiolinks and creates a `Surv` object for survival analysis.
4.  **Model 1 & 2 (Single-Gene):** Runs `drug_small_sample_models_cond_penalized_with_pvalue_optimized` to fit penalized Cox models and perform feature selection.
5.  **Model 3 (Multi-Gene):** Runs `drug_multigene_models_cond_penalized` to build pathway-based models.
6.  **Analysis & Visualization:** Generates C-Index plots, Hazard Ratio (HR) plots, and Kaplan-Meier (KM) curves for significant gene signatures.

## Repository Structure

The project is structured with notebooks located inside their respective data directories. Pre-computed models are saved to a `Results/` folder within each directory.

**Note on Paths:** For this repository to be portable, you should change this in each notebook to a relative path, e.g.: `source('../../functions.R')`.

## Setup and Installation

To run the analyses in these notebooks, you first need to install the required R packages.

### Automatic Installation

The easiest way to set up your environment is to run the provided dependency script from the root directory of this project.

1.  Open R or RStudio.
2.  Set your working directory to the root of this project.
3.  Run the following command in your R console:

```R
source("install_dependencies.R")
```

This script will automatically:
* Install `BiocManager` (the installer for bioinformatics packages).
* Check for all required packages from both CRAN and Bioconductor.
* Install only the packages that are currently missing from your system.
* Load all libraries so you are ready to run the analysis notebooks.

### Manual Package List

If you prefer to install packages manually, you will need the following:

#### CRAN Packages:
```R
install.packages(c(
    "glmnet", "factoextra", "FactoMineR", "caret", "gplots", 
    "survival", "survminer", "RColorBrewer", "tidyverse", 
    "ggfortify", "boot", "patchwork", "gridExtra", "data.table", 
    "doParallel"
))
```

#### Bioconductor Packages:
```R
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c(
    "TCGAbiolinks", 
    "survcomp"
))
```

## How to Run the Analysis

This project contains computationally intensive steps (Penalized Cox Regressions) that can take a long time to complete. You have two options for running the notebooks.

### Option 1: Load Pre-computed Results (Recommended)

For quick analysis and visualization, you can use the pre-computed models (`.rds` files) included in the `Results/` directory of each analysis folder.

In each notebook (e.g., `BRCA_Samples_Results.ipynb`):

1. **SKIP** the cells under the "Penalized Cox Regression Analysis" and "Multi-Gene Cox Models" sections (these are the calculation steps).

2. **RUN** the code cell that loads the `.rds` files. This cell looks like this:
```R
# --- LOAD PRE-COMPUTED MODELS ---
# To skip the lengthy model computation steps, simply run this code block.

DNAi_EMC_..._Pen_CoxModel = readRDS('Results/DNAi_EMC_..._Pen_CoxModel.rds')
multiGeneCox_... = readRDS('Results/DNAi_EMC_..._MultiGeneCox.rds')
```

3. You can then proceed to run all subsequent analysis and visualization cells (e.g., C-Index plots, HR plots, and Kaplan-Meier curves).

### Option 2: Run the Full Analysis (Computationally Intensive)

If you wish to reproduce the results from scratch:

1. In the notebook, **RUN** all cells sequentially from the top.
2. This includes the cells under "Penalized Cox Regression Analysis" and "Multi-Gene Cox Models".
   * **WARNING**: This may take some time to complete.
3. **SKIP** or comment out the cell block dedicated to loading the pre-computed `.rds` files.

## Analysis Notebooks Overview

This repository includes 6 main analysis notebooks, grouped by drug and cohort:

### Paclitaxel (CYTOi)
* `Pac_All_Samples_Results.ipynb`: Analysis of Paclitaxel response across all 5 matched TCGA cohorts (BRCA, HNSC, STAD, LUAD, UCS).
* `BRCA_Samples_Results.ipynb`: A focused, deep-dive analysis on the BRCA cohort only.

### 5-Fluorouracil (DNA_REPi)
* `Fu_All_Samples_Results.ipynb`: Analysis of 5-FU response across all 4 matched TCGA cohorts (STAD, PAAD, ESCA, READ).
* `STAD_Samples_Results.ipynb`: A focused, deep-dive analysis on the STAD cohort only.

### Gemcitabine (DNA_REPi)
* `Gem_All_Samples_Results.ipynb`: Analysis of Gemcitabine response across all 5 matched TCGA cohorts (PAAD, PCPG, LIHC, LUSC, SARC).
* `PAAD_Samples_Results.ipynb`: A focused, deep-dive analysis on the PAAD cohort only.
