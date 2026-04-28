# =============================================================================
# Survival Analysis: Simulated Data + Mixed Logistic Regression (log-log link)
# Uses: lme4 for mixed-effects model, complementary log-log (cloglog) link
# =============================================================================

library(lme4)
library(ggplot2)     # optional, for visualization
library(dplyr)       # optional, for data wrangling

set.seed(42)

# =============================================================================
# 1. SIMULATE DATASET
# =============================================================================
# Design:
#   N = 100 individuals nested within 10 groups (random effect)
#   Response: survived (binary 0/1)
#   Predictors:
#     - age         : continuous (years, ~Normal)
#     - weight      : continuous (kg, ~Normal)
#     - treatment   : categorical (3 levels: "control", "low_dose", "high_dose")
#     - sex         : categorical (2 levels: "M", "F")
#     - group       : random effect grouping factor (10 groups)

n_individuals <- 100
n_groups      <- 10

# --- Grouping structure ---
group <- factor(rep(paste0("G", 1:n_groups), each = n_individuals / n_groups))

# --- Random intercepts for each group ---
group_ranef <- rnorm(n_groups, mean = 0, sd = 0.8)   # SD of random intercept
names(group_ranef) <- paste0("G", 1:n_groups)

# --- Continuous predictors ---
age    <- round(rnorm(n_individuals, mean = 45, sd = 12), 1)
weight <- round(rnorm(n_individuals, mean = 70, sd = 15), 1)

# --- Categorical predictors ---
treatment <- factor(
  sample(c("control", "low_dose", "high_dose"),
         size = n_individuals, replace = TRUE),
  levels = c("control", "low_dose", "high_dose")   # "control" is reference
)

sex <- factor(
  sample(c("M", "F"), size = n_individuals, replace = TRUE),
  levels = c("M", "F")                              # "M" is reference
)

# --- Linear predictor (log-log scale = cloglog link) ---
# True fixed-effect coefficients (on the cloglog scale):
#   intercept       =  0.5
#   age             = -0.03   (older → lower survival)
#   weight          =  0.02   (heavier → slightly higher survival)
#   treatment=low   =  0.6    (low dose helps)
#   treatment=high  =  1.2    (high dose helps more)
#   sex=F           =  0.4    (females survive better)

beta_intercept       <-  0.5
beta_age             <- -0.03
beta_weight          <-  0.02
beta_trt_low         <-  0.6
beta_trt_high        <-  1.2
beta_sex_F           <-  0.4

trt_low  <- as.integer(treatment == "low_dose")
trt_high <- as.integer(treatment == "high_dose")
sex_F    <- as.integer(sex == "F")

eta <- beta_intercept +
  beta_age    * age +
  beta_weight * weight +
  beta_trt_low  * trt_low +
  beta_trt_high * trt_high +
  beta_sex_F    * sex_F +
  group_ranef[as.character(group)]   # add group random intercept

# --- Convert linear predictor to probability via COMPLEMENTARY LOG-LOG link ---
# cloglog link:  eta = log(-log(1 - p))
# Inverse:       p   = 1 - exp(-exp(eta))
prob_survive <- 1 - exp(-exp(eta))

# --- Simulate binary outcome ---
survived <- rbinom(n_individuals, size = 1, prob = prob_survive)

# --- Assemble data frame ---
df <- data.frame(
  id        = 1:n_individuals,
  group     = group,
  survived  = survived,
  age       = age,
  weight    = weight,
  treatment = treatment,
  sex       = sex
)

cat("=== Dataset Summary ===\n")
print(summary(df))
cat("\nSurvival rate:", round(mean(df$survived), 3), "\n\n")

# =============================================================================
# 2. FIT MIXED-EFFECTS LOGISTIC REGRESSION WITH LOG-LOG LINK
# =============================================================================
# The complementary log-log (cloglog) link IS the log-log link used in
# survival/discrete-time hazard models:
#   log(-log(1 - p)) = eta
#
# In lme4, this is specified via family = binomial(link = "cloglog")
# Random intercept for 'group' captures clustering of individuals.

model <- glmer(
  survived ~ age + weight + treatment + sex + (1 | group),
  data   = df,
  family = binomial(link = "cloglog"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl   = list(maxfun = 2e5))
)
# Note on running, several warning messages here:
# Warning messages:
# 1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model failed to converge with max|grad| = 0.0580147 (tol = 0.002, component 1)
# 2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#   Model is nearly unidentifiable: large eigenvalue ratio
#  - Rescale variables?

cat("=== Model Summary ===\n")
print(summary(model))

# =============================================================================
# 3. EXTRACT AND DISPLAY KEY RESULTS
# =============================================================================

# --- Fixed effects ---
fe <- fixef(model)
cat("\n=== Fixed Effects (cloglog scale) ===\n")
print(round(fe, 4))

# --- Odds-ratio-like interpretation: exp(coef) gives hazard ratios
#     on the complementary log-log scale (interpretable as log hazard ratios)
cat("\n=== exp(Fixed Effects) — Hazard Ratios ===\n")
print(round(exp(fe), 4))

# --- Confidence intervals (profile or Wald) ---
cat("\n=== 95% Confidence Intervals (Wald) ===\n")
ci <- confint(model, method = "Wald", parm = "beta_")
print(round(ci, 4))

# --- Random effects variance ---
cat("\n=== Random Effects ===\n")
print(VarCorr(model))

re <- ranef(model)$group
cat("\nEstimated group-level random intercepts:\n")
print(round(re, 4))

# --- Model fit ---
cat("\n=== Model Fit ===\n")
cat("AIC:", AIC(model), "\n")
cat("BIC:", BIC(model), "\n")
cat("Log-likelihood:", logLik(model)[1], "\n")

# =============================================================================
# 4. COMPARE TO NULL MODEL (LRT for overall model significance)
# =============================================================================
model_null <- glmer(
  survived ~ 1 + (1 | group),
  data   = df,
  family = binomial(link = "cloglog"),
  control = glmerControl(optimizer = "bobyqa")
)

cat("\n=== Likelihood Ratio Test: Full vs. Null ===\n")
print(anova(model_null, model))

# =============================================================================
# 5. OPTIONAL: VISUALIZE FITTED PROBABILITIES
# =============================================================================
df$fitted_prob <- fitted(model)

# Plot fitted survival probability by treatment, coloured by sex
p <- ggplot(df, aes(x = age, y = fitted_prob, colour = treatment, shape = sex)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  facet_wrap(~group, ncol = 5) +
  labs(
    title    = "Fitted Survival Probabilities by Group",
    subtitle = "Mixed logistic regression — complementary log-log link",
    x        = "Age",
    y        = "P(Survive)",
    colour   = "Treatment",
    shape    = "Sex"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("survival_fitted_probs.png", plot = p, width = 10, height = 7, dpi = 150)
cat("\nPlot saved to survival_fitted_probs.png\n")

# =============================================================================
# END
# =============================================================================
