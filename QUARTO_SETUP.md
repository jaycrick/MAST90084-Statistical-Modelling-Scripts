# Quarto Project Structure for MAST90084

## Project Setup Complete ✓

This Quarto website project has been configured to provide detailed educational guides for each R script in the course.

## Directory Structure

```
/scripts/
├── 01-ANODEV.qmd                  # ANOVA and Deviance Analysis (Binomial GLM)
├── 02-CellDiff.qmd                # Cell Differentiation Study (Poisson GLM)
├── 03-Example3-1and3-11Caes.qmd   # Caesar Dataset Logistic Regression
├── 04-Example3-2BTR.qmd           # BTR Binary Logistic Regression
├── 05-gee_ctsib.qmd               # GEE with Multiple Correlation Structures
├── 06-GLM_mixed_Effect.qmd        # Binomial GLMM with Different Estimators
├── 07-Happiness.qmd               # Ordinal Regression with Cumulative Links
├── 08-LM_mixed_effect.qmd         # Linear Mixed Effects Random Intercepts
├── 09-ohio.qmd                    # Ohio Respiratory Data (GEE Focus)
├── 10-survival.qmd                # Survival Analysis - KM & Weibull
└── 11-tonsil.qmd                  # Ordinal Models Comparison
```

## Files Modified/Created

### Configuration Files
- **_quarto.yml** - Updated with website structure, navigation menu, and HTML formatting
- **default.qmd** - Rewritten as comprehensive landing page

### Educational Content (11 documents)
Each Quarto document contains:
- ✓ Clear topic overview
- ✓ Data loading and exploration
- ✓ Step-by-step code with explanations
- ✓ Statistical interpretation
- ✓ Model diagnostics and visualization
- ✓ Key learning takeaways
- ✓ References to textbooks

## Learning-Focused Features

### Format & Style
- **Theme**: Cosmo (clean, professional)
- **Table of Contents**: 3-level depth for easy navigation
- **Code Display**: Fully visible (not folded) with copy button
- **Syntax Highlighting**: Atom One (readable)

### Content Structure per Document
1. **Overview** - What will be learned
2. **Data Loading** - Understanding the dataset
3. **Exploration** - Data visualization and summary statistics
4. **Analysis** - Model fitting with explanation
5. **Interpretation** - What the results mean
6. **Diagnostics** - Model assessment
7. **Takeaways** - Key learning points
8. **References** - Sources for deeper learning

## How to Build and View

### Option 1: Build the entire website
```bash
cd /Users/jccum/git_repos/MAST90084-Statistical-Modelling-Scripts
quarto render
```
Output will be in `_output/` directory

### Option 2: Preview specific document
```bash
quarto preview scripts/01-ANODEV.qmd
```

### Option 3: Build specific document
```bash
quarto render scripts/01-ANODEV.qmd
```

## Topics Covered

### 1. Generalized Linear Models (GLM)
- Binomial/logistic regression
- Poisson regression
- Interaction effects
- Goodness-of-fit testing

### 2. Mixed Effects Models
- Random intercepts
- REML vs MLE estimation
- GLMM with different numerical methods
- Variance component interpretation

### 3. Longitudinal/Correlated Data
- GEE with multiple correlation structures
- Independence, exchangeable, unstructured
- Marginal vs conditional inference
- Robust standard errors

### 4. Ordinal Regression
- Cumulative link models
- Partial proportional odds
- Sequential models
- Category-specific effects

### 5. Survival Analysis
- Kaplan-Meier estimator
- Handling censored data
- Weibull distribution assessment

## Customization Options

To modify the appearance or structure:

### Change HTML theme
Edit `_quarto.yml`:
```yaml
format:
  html:
    theme: [lux|simplex|journal|flatly|darkly|slate|etc]
```

### Add table of contents to side
```yaml
format:
  html:
    toc-location: left
```

### Change code folding
```yaml
format:
  html:
    code-fold: true  # Hide code by default
    code-summary: "Show code"
```

## Next Steps

1. **Test the build** with `quarto render` to generate HTML files
2. **Review output** in `_output/` directory
3. **Adjust** formatting/styling as needed
4. **Add data files** (e.g., survival.csv) to project if needed
5. **Deploy** to web server or GitHub Pages for sharing

## Notes

- All code blocks are executable (reproducible analyses)
- External data files referenced (e.g., survival.csv) may need to be added
- Each document is self-contained with full explanations
- Designed for students learning statistical modeling concepts
