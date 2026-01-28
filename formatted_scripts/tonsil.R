library(conflicted)
library(dplyr)
library(Fahrmeir)
library(VGAM)
?tonsil

data(tonsil)
head(tonsil)

# tonsil_df <- dplyr::tbl_df(tonsil) # create a "tibble" dataframe
#
# tonsil_grouped <- dplyr::group_by(tonsil_df, Streptococcus.p) # Group the data by carrier status of Streptococcus pyogenes
#
# tonsil_summary <- dplyr::summarize(
#   tonsil_grouped,
#   present = n[which(Size == 1)],
#   enlarged = n[which(Size == 2)],
#   great_enlarged = n[which(Size == 3)]
# )
# tonsil <- as.data.frame(tonsil_summary)
#
# tonsil

tonsil <- reshape(
  tonsil,
  idvar = "Streptococcus.p",
  timevar = "Size",
  direction = "wide"
) # change the dataset into "wide format" for the vglm function

tonsil
# just to change the colnames
colnames(tonsil) <- c(
  "Streptococcus.p",
  "present",
  "enlarged",
  "great_enlarged"
)


options(contrasts = c("contr.sum", "contr.poly")) # change to effect coding (for Streptococcus.p)
contrasts(tonsil$Streptococcus.p)

fit_cumulative <- vglm(
  cbind(present, enlarged, great_enlarged) ~ Streptococcus.p,
  family = cumulative(parallel = TRUE ~ Streptococcus.p - 1),
  data = tonsil
)

summary(fit_cumulative)


fit_cumulative_extended <- vglm(
  cbind(present, enlarged, great_enlarged) ~ Streptococcus.p,
  family = cumulative(),
  data = tonsil
)

summary(fit_cumulative_extended)

fit_sequential <- vglm(
  cbind(present, enlarged, great_enlarged) ~ Streptococcus.p,
  family = sratio(parallel = TRUE ~ Streptococcus.p - 1),
  data = tonsil
)

summary(fit_sequential)
## I think the p-values reported in F and T are incorrect

# try to use polr
response <- c(
  "present",
  "enlarged",
  "great_enlarged",
  "present",
  "enlarged",
  "great_enlarged"
)
response <- ordered(
  response,
  levels = c("present", "enlarged", "great_enlarged")
)
x <- c(
  "carriers ",
  "carriers ",
  "carriers ",
  "noncarriers",
  "noncarriers",
  "noncarriers"
)

polr_obj <- MASS::polr(
  formula = response ~ x,
  weights = c(as.numeric(tonsil[1, 2:4]), as.numeric(tonsil[2, 2:4]))
)

summary(polr_obj)

# the std errors are same as vglm
# These numbers check out with Table 3.6 in Example 3.7 of F & T
