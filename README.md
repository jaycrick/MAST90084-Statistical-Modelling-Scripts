# MAST90084 Statistical Modelling Scripts

R scripts for MAST90084 Statistical Modelling, rewritten as interactive Quarto documents.

## Quick Start

**1. Install prerequisites:**
- [R](https://cran.r-project.org/) (v4.0.0+)
- [Quarto](https://quarto.org/docs/get-started/)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

**2. Clone and open:**
```bash
git clone https://github.com/yourusername/MAST90084-Statistical-Modelling-Scripts.git
```
Open the project in RStudio by double-clicking `MAST90084-Statistical-Modelling-Scripts.Rproj`

**3. Install packages:**

Option A - Use `renv` for reproducibility (recommended):
```r
install.packages("renv")
renv::restore()  # Installs exact versions from renv.lock
```

Option B - Manually install packages

**4. View documents:**
```bash
quarto preview          # Live preview of entire site
quarto render          # Generate all HTML files
```

Or in RStudio: Open any `.qmd` file in `scripts/` and click "Render"

## Working with renv

After installing new packages:
```r
renv::snapshot()  # Lock current versions
```

Check environment status:
```r
renv::status()    # See if packages need updating
```

## Structure

- `scripts/*.qmd` - Interactive analysis documents
- `data/` - Analysis datasets
- `_output/` - Rendered HTML outputs

## Resources

[Quarto Guide](https://quarto.org/docs/guide/) • [R Docs](https://www.r-project.org/)
