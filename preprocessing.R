# library(mlr)
# library(party)
# library(pROC)
library(dplyr)
library(rio)
library(knitr)
library(stringr)

# partial dependence plots
# TODO: Skip?
#library(DALEX)
library(breakDown)

library(rmarkdown)
library(DT) # interactive tables

dat <- import("raw/Daten_Stable_ML_klein.sav")

# fix a weird column name
colnames(dat)[5] <- "Untergebrachter_21_2"


# Primary outcome: RF_Sex (Rückfall Sexualstraftat)
target <- "RF_Sex_FIX_5"
table(dat[, target], useNA = "ifany")

# only keep cases which have no missing value in the primary outcome
dat <- dat[!is.na(dat[, target]), ]
dat[, target] <- factor(dat[, target])


## ======================================================================
## Preprocessing
## ======================================================================

# Define variable types, transformations, missing values, etc.

# check: which variables have missing values?
for (v in names(dat)) {
	NAs <- sum(is.na(dat[, v]))
	if (NAs > 0) print(paste0(v, ": ", NAs))
}


dat$Subgroup <- factor(dat$Subgroup, levels=c(1, 2, 3), labels=c("Rapists", "Child Molesters", "Other sex delict"))

# ---------------------------------------------------------------------
# Recode some NAs

# two cases have no delict number --> recode missing as "other"
dat$Subgroup[is.na(dat$Subgroup)] <- "Other sex delict"

# This variable was manually retrieved
dat$Jahr_Entlassung[is.na(dat$Jahr_Entlassung)] <- 2002

# All (few) Stable items with NA are coded as 0 (which also matches the provided sum score)
dat$Stadyp2_3[is.na(dat$Stadyp2_3)] <- 0
dat$Stadyp14_8[is.na(dat$Stadyp14_8)] <- 0
dat$Stadyp15_9[is.na(dat$Stadyp15_9)] <- 0
dat$Stadyp7_11[is.na(dat$Stadyp7_11)] <- 0


# ---------------------------------------------------------------------
# Fix a typo:

dat$STATIC5m[dat$STATIC5m == 2] <- 1



## ======================================================================
## Fix the STATIC
## ======================================================================

# ---------------------------------------------------------------------
# There are single NAs in STATIC; in the sum score computation, they have been
# treated as 0s. --> Set them to 0

# mark person who have NA in all STATIC items
dat$all_STATIC_NA <- apply(dat %>% select(matches("STATIC\\d*m")), 1, function(x) all(is.na(x)))
table(dat$all_STATIC_NA)

# find STATIC NAs which are *not* part of an "all-NA" person,
# replace with 0
dat[!dat$all_STATIC_NA, 31:40][is.na(dat[!dat$all_STATIC_NA, 31:40])] <- 0


# Recompute STATIC sum

dat$STATIC_Sum_recalc <- dat %>% select(matches("STATIC\\d*m")) %>% rowSums()

# compare differences between originally provided STATIC sum and recalc STATIC sum
table(dat$STATIC_Sum_recalc, dat$STATIC_Sum)

# mark cases which have a mismatch in original and recalc STATIC sum
dat$STATIC_Sum_mismatch <- dat$STATIC_Sum_recalc != dat$STATIC_Sum

# ---------------------------------------------------------------------
# Some cases have all NAs in STATIC items, but still a STATIC_Sum score.
# Set the sum score and the STATIC_Kat to NA for these cases.

# Noch eine kurze Anmerkung zu den Fällen, bei denen zwar ein Static-99-Gesamtwert zunächst vorlag, 
# aber dann alle Werte als fehlend kodiert waren: Hierbei handelt es sich um Fälle, bei denen unter 
# Verwendung einer Vorgängerversion des Static-99 (noch vor Erscheinen des offiziellen Manuals in der 
# deutschsprachigen Version im Jahr 2006) zunächst der Static-99 angewendet wurde, es sich dann aber später – 
# im Zuge einer nochmaligen Überprüfung – herausgestellt hat, dass die Eingangskriterien des Static-99 nicht 
# erfüllt waren (eindeutige sexuelle Motivation des Delikts mit mind. einer sexuell motivierten Kontaktstraftat 
# von Personen, die bei Entlassung über 18 Jahre alt waren). Diese Fällen wurden deshalb von den 
# weiteren Analysen ausgeschlossen.

dat$STATIC_Sum[dat$all_STATIC_NA == TRUE] <- NA
dat$STATIC_Kat[dat$all_STATIC_NA == TRUE] <- NA
dat$StaSta_Kat[dat$all_STATIC_NA == TRUE] <- NA
dat$STATIC_Sum_recalc[dat$all_STATIC_NA == TRUE] <- NA


## ======================================================================
## Check the Stable
## ======================================================================

# is the provided sum score identical to the recalc sum score?
dat$Stable_Sum_recalc <- dat[, 14:25] %>% rowSums()

# compare differences between originally provided STATIC sum and recalc STATIC sum
table(dat$Stable_Sum_recalc, dat$Stable_Sum)
cor(dat$Stable_Sum_recalc, dat$Stable_Sum, use="p")

# --> looks good

# ---------------------------------------------------------------------
# Fix the STATIC-Kat and StaSta-Kat category based on the new STATIC sum scores

dat$STATIC_Kat_recalc <- NA
dat$STATIC_Kat_recalc[dat$STATIC_Sum_recalc %in% c(0, 1)] <- 1
dat$STATIC_Kat_recalc[dat$STATIC_Sum_recalc %in% c(2, 3)] <- 2
dat$STATIC_Kat_recalc[dat$STATIC_Sum_recalc %in% c(4, 5)] <- 3
dat$STATIC_Kat_recalc[dat$STATIC_Sum_recalc >= 6] <- 4

table(dat$STATIC_Kat, dat$STATIC_Kat_recalc, useNA="always")

# --> 5 cases changed their STATIC_Kat score

# StaSta category

dat$StaSta_Kat_recalc <- NA
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 1 & dat$Stable_Kat <= 2] <- 1
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 1 & dat$Stable_Kat == 3] <- 2

dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 2 & dat$Stable_Kat == 1] <- 1
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 2 & dat$Stable_Kat == 2] <- 2
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 2 & dat$Stable_Kat == 3] <- 3

dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 3 & dat$Stable_Kat == 1] <- 2
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 3 & dat$Stable_Kat == 2] <- 3
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 3 & dat$Stable_Kat == 3] <- 4

dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 4 & dat$Stable_Kat <= 2] <- 4
dat$StaSta_Kat_recalc[dat$STATIC_Kat_recalc == 4 & dat$Stable_Kat == 3] <- 5

table(dat$StaSta_Kat, dat$StaSta_Kat_recalc, useNA="always")

# --> 7 cases changed their StaSta_Kat score

## ======================================================================
# Create final data set
## ======================================================================

# rename Static99 items to ensure the same labels as used in the manuscript
# ATTENTION: Static99 item labels 1 and 5 are switched to adhere to current labeling standards
dat <- rename(dat, Static99_Item_1 = STATIC5m, Static99_Item_2 = STATIC2m, 
  Static99_Item_3 = STATIC3m, Static99_Item_4 = STATIC4m, Static99_Item_5 = STATIC1m,
  Static99_Item_6 = STATIC6m, Static99_Item_7 = STATIC7m, Static99_Item_8 = STATIC8m,
  Static99_Item_9 = STATIC9m, Static99_Item_10 = STATIC10m)

# rename Stable items to ensure the same labels as used in the manuscript
dat <- rename(dat, Stable2007_Item_2 = Stadyp1_2, Stable2007_Item_3 = Stadyp2_3, 
  Stable2007_Item_4 = Stadyp3_4, Stable2007_Item_5 = Stadyp4_5, Stable2007_Item_6 = Stadyp5_6,
  Stable2007_Item_7 = Stadyp13_7, Stable2007_Item_8 = Stadyp14_8, Stable2007_Item_9 = Stadyp15_9,
  Stable2007_Item_10 = Stadyp6_10, Stable2007_Item_11 = Stadyp7_11, 
  Stable2007_Item_12 = Stadyp8_12, Stable2007_Item_13 = Stadyp12_13)

# ---------------------------------------------------------------------
# Create sets of predictor variables

varSet0 <- names(dat)[c(1:40, 85, 88, 89)]	# exclude PCL, include the recalculated STATIC sum and categories

# remove variables with too many missings, remove irrelevant predictors
varSet.all <- varSet0[!varSet0 %in% c("Stadyp9", "Stadyp10", "Stadyp11", "Untergebrachter_21_2", "Jahr_Entlassung", "BE", "STATIC_Sum")]

Stable.items <- names(dat)[c(14:25)]
STATIC.items <- names(dat)[c(31:40)]
SS.items <- c(Stable.items, STATIC.items)

# All SS items + extra items, but without the sum scores and StaSta categories
# remove STATIC items which have continuous equivalents

# "bedingteJaNein" könnte tautologisch sein, da es auch das Ergebnis der Einschätzung widerspiegelt
# --> erst mal drinlassen

# ATTENTION: This variable set is not used in the final analysis!
varSet.all.items <- varSet.all[!varSet.all %in% 
    c("STATIC_Sum_recalc", "STATIC_Kat_recalc", "StaSta_Kat_recalc", "Stable_Kat", "Stable_Sum",
      "Static99_Item_5", "Static99_Item_2", "Static99_Item_9", "Static99_Item_1")]



# ---------------------------------------------------------------------
#  look at correlation table: Which variables seem to be doubled?

C1 <- cor(dat[, varSet.all][, -3], use="p") %>% round(2)
C1[abs(C1) < .5] <- NA
C1

# double variables:
# Static 5 <-> vordelsex
# Static 2 <-> vordelges
# Static 9 <-> Alter_Entlassung
# Static 1 <-> vordelgew ??? (cor is only .52)
	# more potential information in continuous variable, but maybe Static 1 more reliable?
	# Remove Static 1 for the final model
table("STATIC 1"=dat$Static99_Item_1, vordelgew=dat$vordelgew)
table("STATIC 1"=dat$Static99_Item_1, vordelgew=dat$vordelgew > 0)

# BE and bedingteJaNein are the same (reverse coded)
table(dat$BE, dat$bedingteJaNein)



# ---------------------------------------------------------------------
# Create final data set "dat2" by selecting the relevant variables
dat2 <- dat[, c(target, varSet.all)]
save(dat2, file="raw/dat2.RData")
