# Candidacy Paper: AX Discrimination Logistic Regression
# -----------------------------------------------------
# Analysis extracted from the original R console log.
# Input file expected: disc20withRT.txt
#
# Outcome:
#   Different.resp
#
# Predictor:
#   PairDifference
#
# Four analysis subsets:
#   1. Voiced stimuli with aspiration contrast
#   2. Unaspirated stimuli with voicing contrast
#   3. Aspirated stimuli with voicing contrast
#   4. Voiceless stimuli with aspiration contrast


# Load data ---------------------------------------------------------------

DiscData <- read.table("disc20withRT.txt", header = TRUE)


# Create the four discrimination datasets ---------------------------------

# Voiced stimuli with aspiration contrast
Prev_Vd_Aspiration <- subset(
  DiscData,
  PairDifference == "Prev_Vd_Aspiration"
)

Interv_Vd_Aspiration <- subset(
  DiscData,
  PairDifference == "Interv_Vd_Aspiration"
)

Vd_Aspiration_Diff <- rbind(
  Prev_Vd_Aspiration,
  Interv_Vd_Aspiration
)


# Unaspirated stimuli with voicing contrast
Prev_Unasp_Voicing <- subset(
  DiscData,
  PairDifference == "Prev_Unasp_Voicing"
)

Interv_Unasp_Voicing <- subset(
  DiscData,
  PairDifference == "Interv_Unasp_Voicing"
)

Unasp_Voicing_Diff <- rbind(
  Prev_Unasp_Voicing,
  Interv_Unasp_Voicing
)


# Aspirated stimuli with voicing contrast
Prev_Asp_Voicing <- subset(
  DiscData,
  PairDifference == "Prev_Asp_Voicing"
)

Interv_Asp_Voicing <- subset(
  DiscData,
  PairDifference == "Interv_Asp_Voicing"
)

Asp_Voicing_Diff <- rbind(
  Prev_Asp_Voicing,
  Interv_Asp_Voicing
)


# Voiceless stimuli with aspiration contrast
Prev_Vl_Aspiration <- subset(
  DiscData,
  PairDifference == "Prev_Vl_Aspiration"
)

Interv_Vl_Aspiration <- subset(
  DiscData,
  PairDifference == "Interv_Vl_Aspiration"
)

Vl_Aspiration_Diff <- rbind(
  Prev_Vl_Aspiration,
  Interv_Vl_Aspiration
)


# Main logistic-regression models -----------------------------------------

Model1 <- glm(
  Different.resp ~ PairDifference,
  family = binomial,
  data = Vd_Aspiration_Diff
)

Model2 <- glm(
  Different.resp ~ PairDifference,
  family = binomial,
  data = Unasp_Voicing_Diff
)

Model3 <- glm(
  Different.resp ~ PairDifference,
  family = binomial,
  data = Asp_Voicing_Diff
)

Model4 <- glm(
  Different.resp ~ PairDifference,
  family = binomial,
  data = Vl_Aspiration_Diff
)


# Model summaries ----------------------------------------------------------

summary(Model1)
summary(Model2)
summary(Model3)
summary(Model4)


# Secondary analyses: place of articulation -------------------------------

Model5 <- glm(
  Different.resp ~ Place,
  family = binomial,
  data = Vd_Aspiration_Diff
)

Model6 <- glm(
  Different.resp ~ Place,
  family = binomial,
  data = Unasp_Voicing_Diff
)

Model7 <- glm(
  Different.resp ~ Place,
  family = binomial,
  data = Asp_Voicing_Diff
)

Model8 <- glm(
  Different.resp ~ Place,
  family = binomial,
  data = Vl_Aspiration_Diff
)

summary(Model5)
summary(Model6)
summary(Model7)
summary(Model8)


# Secondary analyses: stimulus speaker gender -----------------------------

Model9 <- glm(
  Different.resp ~ SpeakerGender,
  family = binomial,
  data = Vd_Aspiration_Diff
)

Model10 <- glm(
  Different.resp ~ SpeakerGender,
  family = binomial,
  data = Unasp_Voicing_Diff
)

Model11 <- glm(
  Different.resp ~ SpeakerGender,
  family = binomial,
  data = Asp_Voicing_Diff
)

Model12 <- glm(
  Different.resp ~ SpeakerGender,
  family = binomial,
  data = Vl_Aspiration_Diff
)

summary(Model9)
summary(Model10)
summary(Model11)
summary(Model12)
