# MAST90084 Statistical Modelling Scripts

R scripts for MAST90084 Statistical Modelling, rewritten as interactive Quarto documents.

## Quick Start

**1. Install prerequisites:**
- [R](https://cran.r-project.org/) (v4.0.0+)
- [Quarto](https://quarto.org/docs/get-started/)
- [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

**2. Clone and setup (once):**
```bash
git clone https://github.com/yourusername/MAST90084-Statistical-Modelling-Scripts.git
cd MAST90084-Statistical-Modelling-Scripts
```

Open in RStudio and run in the console:
```r
renv::restore()  # Installs all package dependencies (one time only)
```

**3. Render documents:**

In RStudio: Open any `.qmd` file in `quarto_scripts/` folder and click **"Render"**

Or from terminal:
```bash
quarto preview      # Live preview of all documents
```

## Structure

- `quarto_scripts/*.qmd` - Interactive analysis documents
- `data/` - Analysis datasets
- `_output/` - Rendered HTML outputs

## Resources

[Quarto Guide](https://quarto.org/docs/guide/) • [R Docs](https://www.r-project.org/)

## Contributing

To keep things organized and safe:

1. **Create a new branch** for each correction (e.g., `fix-typo-linear-model`)
2. **Make your edits** and save
3. **Commit** with a short summary (e.g., "Fix ANOVA section")
4. **Push origin** and create a pull request
5. **One branch per correction** — keep changes small and focused

Never edit the main branch directly. All changes should go through a pull request.
