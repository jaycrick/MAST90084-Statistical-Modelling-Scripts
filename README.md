# MAST90084-Statistical-Modelling-Scripts
Rewriting the R scripts for MAST90084 Statistical Modelling masters level subject to Quarto scripts

## Prerequisites

Install the following software:

- **[R](https://cran.r-project.org/)** (v4.0.0+) - Run statistical analysis scripts
- **[RStudio](https://posit.co/download/rstudio-desktop/)** (optional) - IDE for R and Quarto
- **[Quarto](https://quarto.org/docs/get-started/)** - Render .qmd files to HTML
- **[Git](https://git-scm.com/downloads)** - Version control
- **[Git LFS](https://git-lfs.github.com/)** - Manage large data files
  ```bash
  git lfs install
  ```

## Setup

**1. Clone the repository:**
```bash
git clone https://github.com/yourusername/MAST90084-Statistical-Modelling-Scripts.git
cd MAST90084-Statistical-Modelling-Scripts
```

**2. Install R packages:**
```r
install.packages(c("tidyverse", "lme4", "nlme",
                   "survival", "gee", "geepack", "MASS"))
```

**3. Render documents:**
```bash
# Single document
quarto render scripts/01-ANODEV.qmd

# Entire site
quarto render

# Preview with live reload
quarto preview
```

## Repository Structure

- `scripts/` - Quarto files (.qmd)
- `raw_scripts/` - Original R scripts
- `formatted_scripts/` - Formatted R scripts
- `data/` - Analysis data files
- `Lectures/` - Course materials
- `_output/` - Rendered HTML

## Resources

- [Quarto Documentation](https://quarto.org/docs/guide/)
- [R Documentation](https://www.r-project.org/)
- [Git LFS Documentation](https://git-lfs.github.com/)
