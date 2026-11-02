# Candidacy Paper: AX Discrimination Reaction-Time Analysis
# ---------------------------------------------------------
# Cleaned from the original R console analysis.
#
# The original analysis:
#   - loaded the discrimination data
#   - removed RTs more than 2 SD above the overall mean
#   - examined RT using aspiration contrast, voicing contrast, and context
#   - examined place of articulation and speaker gender
#   - compared prevocalic and intervocalic RTs
#   - produced an RT-by-context boxplot
#
# Failed/debugging commands from the console log have been omitted.


# Load data ----------------------------------------------------------------

# Original console analysis used this filename.
DiscData <- read.table("disc20withRT.txt", header = TRUE)


# RT cleaning: exclude responses above mean + 2 SD --------------------------

rt_mean <- mean(DiscData$RT)
rt_sd <- sd(DiscData$RT)

rt_upper_cutoff <- rt_mean + (2 * rt_sd)

rt_mean
rt_sd
rt_upper_cutoff

Disccleaned <- subset(
  DiscData,
  RT < rt_upper_cutoff
)


# Main reaction-time ANOVA --------------------------------------------------

rt_model <- aov(
  RT ~ AspirationDiff. * VoicingDiff. * PreV.,
  data = Disccleaned
)

summary(rt_model)


# Additional check: aspiration difference ---------------------------------

aspiration_model <- aov(
  RT ~ AspirationDiff.,
  data = Disccleaned
)

summary(aspiration_model)


# Place of articulation and speaker gender --------------------------------

place_gender_model <- aov(
  RT ~ Place * SpeakerGender,
  data = Disccleaned
)

summary(place_gender_model)


# Compare prevocalic and intervocalic contexts -----------------------------

Prevocalic <- subset(
  Disccleaned,
  PreV. == 1
)

Intervocalic <- subset(
  Disccleaned,
  PreV. == 0
)

# Compare variances
context_variance_test <- var.test(
  Prevocalic$RT,
  Intervocalic$RT
)

context_variance_test


# Welch two-sample t-test ---------------------------------------------------

context_rt_test <- t.test(
  Prevocalic$RT,
  Intervocalic$RT
)

context_rt_test


# Descriptive means ---------------------------------------------------------

mean(Prevocalic$RT)
mean(Intervocalic$RT)


# RT visualization ----------------------------------------------------------

boxplot(
  RT ~ PreV.,
  data = Disccleaned,
  names = c(
    "Intervocalic Combinations",
    "Prevocalic Combinations"
  ),
  xlab = "Stimuli Combinations in Different Contexts",
  ylab = "Reaction Time",
  main = "RT for Stimuli Combinations in Contexts"
)
