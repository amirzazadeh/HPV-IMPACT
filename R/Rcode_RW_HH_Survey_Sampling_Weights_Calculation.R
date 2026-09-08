# Last updated: 1 April 2026

# Rwanda Household survey
# Calculate sampling weights and adjust them using post-stratification.
# Caregiver and Adolescent household survey

# Reads caregiver and adolescent household survey data and selected EA/district target files.
# Calculates refusal rates among eligible caregivers and adolescent girls.
# Computes multi-stage sampling probabilities (EA, household, eligible HH, girl participation) and derives survey weights.
# Applies post-stratification adjustments so weights match the district population of girls aged 13-15.
# Exports datasets, probabilities, refusal summaries, and final adjusted weights to an Excel file.

# For questions, contact: [ali.mirzazadeh@ucsf.edu](mailto:ali.mirzazadeh@ucsf.edu)

# data needed
# Rwanda_Household_Sampling_Steps_Data.xlsx
# this excel file has the following sheets: 
## CG_survey: short dataset for Caregiver (CG) survey data by age.
## Girls_survey: short dataset containing Adolescent survey data by age.
## Girls_10_14_by_District: Total girls aged 10–14, organized by district.
## L1 - EA:Level 1: Enumeration Area allocated to each district.
## EAs_187: Dataset listing 187 Enumeration Areas included in the study.
## L2 - HH: Level 2: Household listing data.
## L3_HHenrolled: Level 3: Households that are visited and attempted to enroll in the study.
## L3_Girlsenrolled_sero: Level 3: Girls that visited and attempted and enrol in survey / serology.



rm(list = ls())

library(readxl)
library(janitor)
library(stringr)
library(dplyr)
library(haven)
library(writexl)

# -----------------------------
# 0) Set working directory
# -----------------------------
#setwd("/Users/alimirzazadeh1/Documents/GitHub/RwandaSamplingWeights")
#setwd("/Users/alimirzazadeh1/Desktop/RwandaSamplingWeights")

# ---------------------------------------------------
# 1) Caregiver and Girls data from the HH databases
# ---------------------------------------------------

# Caregiver data
library(readxl)
c <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx", sheet = "CG_survey")
c$HHenrolled<-1
c$inHHdata<-"Y"
table(c$prvdis, useNA = "ifany")
colSums(c[, c("consent_caregiver_sum","eligible_girls_sum","enrolled_girls_sum")], na.rm = TRUE)
table(duplicated(c$caregiver_id))
table(c$eligible_girls_sum)
table(c$enrolled_girls_sum)

# Girls data 
library(readxl)
g <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx", sheet = "Girls_survey")
g$HHenrolled<-1
g$inHHdata<-"Y"
table(g$prvdis, useNA = "ifany")
colSums(g[, c("assent_girls_sum", "enrolled_girls_sum", "serology_girls_sum")], na.rm = TRUE)
table(duplicated(g$ag_caregiver_id)) # 98 HH enrolled more than 1 girl in the survey


# -----------------------------
# 2) Selected EAs (187)
# -----------------------------

ea <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx",sheet = "EAs_187")
colnames(ea)
table(ea$prv, useNA = "ifany")
sum(table(ea$prv, useNA = "ifany"))
table(ea$prvdis, useNA = "ifany")

# -----------------------------
# 3) District target EA data set
# -----------------------------
DisTargets <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx",
                      sheet = "L1 - EA", range = "A1:H31")
colnames(DisTargets)
colnames(DisTargets)<-c("province","dis","TargetedDistricts","TotalHH","TotalEAs","NumberEATargeted","NumberEAsSurveyed","NumberHHTargeted")
colnames(DisTargets)

library(stringr)
DisTargets$prv <- ""
DisTargets$prv[str_detect(DisTargets$province, "Kigali")]    <- "Kigali"
DisTargets$prv[str_detect(DisTargets$province, "Eastern")]   <- "East"
DisTargets$prv[str_detect(DisTargets$province, "Western")]   <- "West"
DisTargets$prv[str_detect(DisTargets$province, "Northern")]  <- "North"  
DisTargets$prv[str_detect(DisTargets$province, "Southern")]  <- "South"

DisTargets$prvdis <- paste(DisTargets$prv, DisTargets$dis, sep = "_")
table(DisTargets$prv, useNA = "ifany")
table(DisTargets$prvdis, useNA = "ifany")

DisTargets <- DisTargets[, c("province","prv","dis","prvdis","TargetedDistricts","TotalHH","TotalEAs",
                       "NumberEATargeted","NumberEAsSurveyed","NumberHHTargeted")]
sum(DisTargets$NumberHHTargeted)

# -------------------------------------------------------
# 4) Enumeration - HH Listing data in the selected EA
# -------------------------------------------------------

# Eligible HH count and enrollment data in each EA 
library(readxl)
L2HH <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx", 
                       sheet = "L2 - HH")
colnames(L2HH)<-c("prv", "dis", "sector", "newvillageid", "ea_name", "fullylisted",
              "totalHH_listed", "totalHH_elig", "HH_samplesize", "totalHH_enrolled")
L2HH$totalHH_elig
L2HH$inHHenumr<-"Y"
any(duplicated(L2HH$newvillageid))

# The household was visited, and a caregiver was identified and invited to participate in the survey.
L2HH$VisitSucces<- L2HH$totalHH_enrolled/L2HH$HH_samplesize
summary(L2HH$VisitSucces)
by(L2HH$VisitSucces, L2HH$dis, summary)

# ------------------------------------------------------------
# 5) HH selection, visit, and refusal data in the selected EA
# ------------------------------------------------------------

# Adolescents' HH survey and serology study

L3Girls <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx", 
                  sheet = "L3_Girlsenrolled_sero")
colnames(L3Girls) <- c("prv","dis","newvillageid","newvillage","caregiver_id","girls_elig","girls_enrl","girls_assent","sero_selct","sero_assent","girls_blood")
L3Girls$inSurvey<-"Y"
table(L3Girls$inSurvey)
any(duplicated(L3Girls$caregiver_id))

# % of eligible girls who refused to participate in the survey 
L3Girls$Refusal <- 1-L3Girls$girls_assent/L3Girls$girls_elig
100*summary(L3Girls$Refusal)

# By district, calculate the percentage of adolescent girls who were eligible but did not provide assent to participate in the survey
library(dplyr)
Refusal_ByDis <- L3Girls %>%
  group_by(dis) %>%
  summarise(mean_refusal = round(100*mean(Refusal, na.rm = TRUE),1))
Refusal_ByDis
Refusal_ByDis<-as.data.frame(Refusal_ByDis)
colnames(Refusal_ByDis)<-c("Dis","Refusal_Percent")

# By province, calculate the percentage of adolescent girls who were eligible but did not provide assent to participate in the survey
library(dplyr)
Refusal_ByPrv <- L3Girls %>%
  group_by(prv) %>%
  summarise(mean_refusal = round(100*mean(Refusal, na.rm = TRUE),1))
Refusal_ByPrv
Refusal_ByPrv<-as.data.frame(Refusal_ByPrv)
colnames(Refusal_ByPrv)<-c("Prv","Refusal_Percent")

MeanRefuzal<-mean(round(100*mean(L3Girls$Refusal, na.rm = TRUE),1))

Refusal_ByPrv <- rbind(
  Refusal_ByPrv,
  data.frame(Prv = "Overall", Refusal_Percent = MeanRefuzal)
)
Refusal_ByPrv # % of eligible girls who refused to participate in the survey overall and by province 


## Caregiver HH Survey 

L3Caregiver <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx", 
                      sheet = "L3_HHenrolled")
colnames(L3Caregiver) <- c("prv","dis","sector","newvillage","newvillageid","caregiver_id","CG_consent","girls_elig","girls_data")
L3Caregiver$inSurvey<-"Y"
table(L3Caregiver$inSurvey)
any(duplicated(L3Caregiver$caregiver_id))

# % visit was that was not successful and the consent data is missing 
table(is.na(L3Caregiver$CG_consent))
prop.table(table(is.na(L3Caregiver$CG_consent)))*100  # about 0.5% unsuccessful visit after 3 attempt

# % of caregivers who had an eligible girls but did not provide consent 
table(L3Caregiver$CG_consent)
prop.table(table(L3Caregiver$CG_consent))*100 # about 5.1% of caregivers did not provide consent

# % unsuccessful visit or did not provide consent
L3Caregiver$VisitConsentSuccess <- ifelse(
  is.na(L3Caregiver$CG_consent) | L3Caregiver$CG_consent == 0,
  0,
  1
)
table(L3Caregiver$VisitConsentSuccess)
prop.table(table(L3Caregiver$VisitConsentSuccess))*100 # 5.6% unsuccessful visit or did not provide consent


# Report the Proportion unsuccessful visit or did not provide consent 
# by province and districts

library(table1)
Prv_VisitConsentSuccess <- t(as.matrix(as.data.frame(
  table1(~ VisitConsentSuccess | prv , data = L3Caregiver)
)))
Prv_VisitConsentSuccess <- Prv_VisitConsentSuccess[-1,1:3]

L3Caregiver$PrvDis<-paste0(L3Caregiver$prv,"_",L3Caregiver$dis)
Dis_VisitConsentSuccess <- t(as.matrix(as.data.frame(
  table1(~ VisitConsentSuccess | PrvDis , data = L3Caregiver)
)))
# remove first row and column V4
Dis_VisitConsentSuccess <- Dis_VisitConsentSuccess[-1,1:3]

PrvDis_VisitConsentSuccess <- rbind(Prv_VisitConsentSuccess, Dis_VisitConsentSuccess)
PrvDis_VisitConsentSuccess<-as.data.frame(as.matrix(PrvDis_VisitConsentSuccess))
PrvDis_VisitConsentSuccess$PrvDis<-rownames(PrvDis_VisitConsentSuccess)
PrvDis_VisitConsentSuccess<-PrvDis_VisitConsentSuccess[,c("PrvDis","V1","V3")]
colnames(PrvDis_VisitConsentSuccess)<-c("PrvDis","N","Prop (SD)")
library(dplyr)
library(stringr)
PrvDis_VisitConsentSuccess <- PrvDis_VisitConsentSuccess %>%
  mutate(
    Prop = as.numeric(str_extract(`Prop (SD)`, "^[0-9.]+")),
    SD_value   = as.numeric(str_extract(`Prop (SD)`, "(?<=\\()[0-9.]+(?=\\))"))
  )
PrvDis_VisitConsentSuccess<-PrvDis_VisitConsentSuccess[,c("PrvDis","N","Prop")]

library(dplyr)
L3Caregiver <- L3Caregiver %>%
  group_by(newvillageid) %>%
  mutate(
    EA_total_records = n(),
    EA_VisitConsentSuccess_total = sum(VisitConsentSuccess, na.rm = TRUE),
    EA_VisitConsentSuccess_prop = mean(VisitConsentSuccess, na.rm = TRUE)
    ) %>%
  ungroup()

summary(L3Caregiver$EA_VisitConsentSuccess_prop)

# % of eligible girls whose their caregivers who provided consent did not provided data for them
L3Caregiver$DataRefusal <- 1-L3Caregiver$girls_data/L3Caregiver$girls_elig
100*summary(L3Caregiver$DataRefusal)


## Merge enumeration data (L2) with the no consent no successful visit and proportion for each area
L2HH <- merge(
  L2HH,
  L3Caregiver[!duplicated(L3Caregiver$newvillageid),
              c("newvillageid", "EA_total_records","EA_VisitConsentSuccess_total", "EA_VisitConsentSuccess_prop")],
  by = "newvillageid",
  all.x = TRUE
)

# ------------------------------------------------------------
# 6) Total girls in every districts for each age of 13, 14, 15
# ------------------------------------------------------------
library(readxl)
DisG10t14 <- read_excel("D/Rwanda_Household_Sampling_Steps_Data.xlsx", 
                     sheet = "Girls_10_14_by_District")
table(DisG10t14$District)
library(dplyr)
DisG10t14 <- DisG10t14 %>%
  mutate(Province = case_when(
    District %in% c("Bugesera","Gatsibo","Kayonza","Kirehe","Ngoma","Nyagatare","Rwamagana") ~ "East",
    District %in% c("Gasabo","Kicukiro","Nyarugenge") ~ "Kigali",
    District %in% c("Burera","Gakenke","Gicumbi","Musanze","Rulindo") ~ "North",
    District %in% c("Gisagara","Huye","Kamonyi","Muhanga","Nyamagabe","Nyanza","Nyaruguru","Ruhango") ~ "South",
    District %in% c("Karongi","Ngororero","Nyabihu","Nyamasheke","Rubavu","Rusizi","Rutsiro") ~ "West"
  ),
  prvdis = paste(Province, District, sep = "_"))
table(DisG10t14$prvdis)
head(DisG10t14)
DisG10t14 <- DisG10t14 %>%
  mutate(
    Girls_10 = round(Girls_10_14 / 5),
    Girls_11 = round(Girls_10_14 / 5),
    Girls_12 = round(Girls_10_14 / 5),
    Girls_13 = round(Girls_10_14 / 5),
    Girls_14 = round(Girls_10_14 / 5),
    Girls_15 = round(Girls_10_14 / 5)
  )

DisG13t15<-DisG10t14[,c("Province","District","prvdis","Girls_13","Girls_14","Girls_15")]
colnames(DisG13t15)<-c("prv","dis","prvdis","Girls_13","Girls_14","Girls_15")
  
library(tidyr)
library(dplyr)
DisG13t15_long <- DisG13t15 %>%
  pivot_longer(
    cols = c(Girls_13, Girls_14, Girls_15),
    names_to = "age",
    values_to = "girls"
  )
DisG13t15_long <- DisG13t15_long %>%
  mutate(age = as.numeric(gsub("Girls_", "", age)))


# ----------------------------------------------------------------
# 5) Compute sampling probabilities (P1, P2, P3, P4) 
# ----------------------------------------------------------------

P1 <- merge(ea, DisTargets[,c("prvdis","TotalHH","NumberEATargeted")], by = "prvdis", all.x = TRUE)

# P1 - EA selection probability
# The probability of selecting each EA (P1) is calculated using probability 
# proportional to size (PPS), where the size is the number of households in 
# the EA relative to the total households in the district, multiplied by the number of EAs targeted.
# The probability of selecting each EA within a district
P1$P1 <-  P1$NumberEATargeted * (P1$TotalHHinPrimaryAndBackupEA / P1$TotalHH)
P1$W1 <- 1 / P1$P1

# P2 - Household selection probability
# The probability of selection of a household within a selected EA (or EA and its Backup, if backup was used)
P1$P2 <-  15 / P1$TotalHHinPrimaryAndBackupEA 
P1$W2 <- 1 / P1$P2

# P1 x P2
P1$P1P2 <- P1$P1*P1$P2
P1$W1W2 <- 1/P1$P1P2
summary(P1[, c("P1", "W1", "P2","W2")])

# P3 - Eligible household selection probability
P3<-L2HH
P3$P3<-L2HH$totalHH_enrolled/L2HH$totalHH_elig
summary(P3$P3)

# Eligible household selection probability X the probability of unsuccessful visit or consent 
P3$P3<-P3$P3*P3$EA_VisitConsentSuccess_prop
summary(P3$P3)

# P4 - Girls selection probability for the Adolescents' survey and serology study

# Adolescents enrollment probability
P4g<-L3Girls
P4g$P4g<-P4g$girls_assent/P4g$girls_elig
P4g$P4g_sero<-P4g$girls_blood/P4g$girls_elig

summary(P4g[, c("P4g", "P4g_sero")])
hist(P4g$P4g)


## Caregiver enrollment probability
P4cg<-L3Caregiver
P4cg$P4cg<-P4cg$girls_data/P4cg$girls_elig
summary(P4cg[, c("P4cg")])



# --------------------------------------------------------------------------------------------
# 5) Merge Caregiver data (=c) with the sampling probabilities (P1, P2, P3, P4) and calculate P 
# --------------------------------------------------------------------------------------------
cP4<-merge(c, P4cg[,c("prv", "dis", "sector", "newvillage",
                        "caregiver_id", "CG_consent", "girls_elig",
                        "girls_data", "inSurvey", "DataRefusal","P4cg")], by = "caregiver_id", all.x= TRUE)
table(is.na(cP4$P4cg), useNA = "always")


cP4P3<-merge(cP4,P3[,c("newvillageid","P3")], all.x = TRUE)
table(is.na(cP4P3$P3), useNA = "always")

cP4P3P2P1<-merge(cP4P3,P1[,c("newvillageid","P1","P2")], all.x = TRUE)
table(is.na(cP4P3P2P1$P1), useNA = "always")

# in the caregiver HH, data was provided for at least one girl, 
# so we are using the number enrolled girls in each HH
# not the number of girls who provided assent for P4_ed calculation
cP4P3P2P1$P4_ed<-cP4P3P2P1$enrolled_girls_sum/cP4P3P2P1$girls_elig
summary(cP4P3P2P1[,c("P4cg","P4_ed")])

cP4P3P2P1$P<-cP4P3P2P1$P1*cP4P3P2P1$P2*cP4P3P2P1$P3*cP4P3P2P1$P4_ed
summary(cP4P3P2P1$P)
cP4P3P2P1$W<-1/cP4P3P2P1$P
hist(cP4P3P2P1$W)
summary(cP4P3P2P1$W)

# Distribution of W in each district 
library(ggplot2)
ggplot(cP4P3P2P1, aes(x = W)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ prvdis, scales = "free_y") +
  labs(
    x = "Weight (W)",
    y = "Count",
    title = "Distribution of W by prvdis"
  ) +
  theme_minimal()


# --------------------------------------------------------------------------------------------
# 6) Merge Adolescents data (=g) with the sampling probabilities (P1, P2, P3, P4) and calculate P 
# --------------------------------------------------------------------------------------------
gP4<-merge(g, P4g[,c("prv", "dis", "newvillage", "caregiver_id",
                     "girls_elig", "girls_enrl", "girls_assent", "sero_selct",
                     "sero_assent", "girls_blood", "inSurvey", "Refusal",
                     "P4g", "P4g_sero")], by.x = "ag_caregiver_id" , by.y = "caregiver_id", all.x= TRUE)
table(is.na(gP4$P4g), useNA = "always")

gP4P3<-merge(gP4,P3[,c("newvillageid","P3")], all.x = TRUE)
table(is.na(gP4P3$P3), useNA = "always")

gP4P3P2P1<-merge(gP4P3,P1[,c("newvillageid","P1","P2")], all.x = TRUE)
table(is.na(gP4P3P2P1$P1), useNA = "always")

# one record with assent = 0, the p has changed from 0 to 1 
table(gP4P3P2P1$P4g==0)
gP4P3P2P1$P4g[gP4P3P2P1$P4g==0]<-1

gP4P3P2P1$P<-gP4P3P2P1$P1*gP4P3P2P1$P2*gP4P3P2P1$P3*gP4P3P2P1$P4g
summary(gP4P3P2P1$P)
gP4P3P2P1$W<-1/gP4P3P2P1$P
hist(gP4P3P2P1$W)
summary(gP4P3P2P1$W)

# Distribution of W by district 
library(ggplot2)
ggplot(gP4P3P2P1, aes(x = W)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ prvdis, scales = "free_y") +
  labs(
    x = "Weight (W)",
    y = "Count",
    title = "Distribution of W by prvdis"
  ) +
  theme_minimal()

# W for serology survey
gP4P3P2P1$Psero<-gP4P3P2P1$P1*gP4P3P2P1$P2*gP4P3P2P1$P3*gP4P3P2P1$P4g_sero
gP4P3P2P1$Wsero<-1/gP4P3P2P1$Psero
gP4P3P2P1$Wsero[is.infinite(gP4P3P2P1$Wsero)] <- NA # To replace Inf (or +Inf) in Wsero with NA.
hist(gP4P3P2P1$Wsero)


##### check discrepancies #####


# ------------------------------------------------------------------
# 7) Post Stratification Adjustment for Caregiver in HH
# ------------------------------------------------------------------

# Aggregate (sum) sampling weights (W) by district (prvdis) and age
library(dplyr)
CGsumW_ByDisAge<-cP4P3P2P1 %>%
  group_by(prvdis, age) %>%
  summarise(sum_W = sum(W, na.rm = TRUE), .groups = "drop")

# Merge summed weights with external data containing total number of girls for each age in each district
# (DisG13t15_long has counts of girls by district and age)
CGDisSumWSumGirls <- merge(CGsumW_ByDisAge,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age")) 

# Get unique age groups and assign colors for plotting
age_levels <- sort(unique(CGDisSumWSumGirls$age))
cols <- c("blue", "darkgreen", "orange")

# Scatter plot of sum of sampling weights vs total number of girls for each disrict, by age
plot(CGDisSumWSumGirls$sum_W,
     CGDisSumWSumGirls$girls,
     col = cols[as.factor(CGDisSumWSumGirls$age)],
     pch = 19,
     xlab = "Sum of Sampling Weights",
     ylab = "Sum of Girls",
     main = "Girls vs Sum Sampling W. by District and Age")
for(i in seq_along(age_levels)) {
  d <- subset(CGDisSumWSumGirls, age == age_levels[i])
  abline(lm(girls ~ sum_W, data = d), col = cols[i], lwd = 2)
}
legend("topleft",
       legend = age_levels,
       col = cols,
       pch = 19,
       lwd = 2,
       title = "Age")

# Calculate adjustment factor (AdjF)
# This is the ratio of actual girls to summed sampling weights
CGDisSumWSumGirls$AdjF<-CGDisSumWSumGirls$girls / CGDisSumWSumGirls$sum_W

# Merge adjustment factor back into original dataset
# Keeps all original rows (all.x = TRUE)
cP4P3P2P1AdjF<-merge(cP4P3P2P1,CGDisSumWSumGirls[,c("prvdis","age","AdjF")], by = c("prvdis","age"), all.x = "TRUE")

# Apply adjustment factor to original weights
# Creates adjusted weights (Wadj)
cP4P3P2P1AdjF$Wadj <- cP4P3P2P1AdjF$W*cP4P3P2P1AdjF$AdjF
summary(cP4P3P2P1AdjF[,c("W","Wadj")]) # Compare original and adjusted weights

ggplot(data = cP4P3P2P1AdjF, aes( x = W, y = Wadj)) +
  geom_point(colour = cP4P3P2P1AdjF$age)
PrvDis_W<- aggregate(cbind(W, Wadj) ~ prvdis + age,
                     data = cP4P3P2P1AdjF,
                     sum,
                     na.rm = TRUE)
PrvDis_W<-merge(PrvDis_W,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age"), all = TRUE)

# plot final adj. weights vs. total target girls at district
ggplot(data = PrvDis_W, aes( x = girls, y = Wadj)) +
  geom_point(colour = PrvDis_W$age)

# ------------------------------------------------------------------
# 8) Post Stratification Adjustment for Adolescents in HH
# ------------------------------------------------------------------

# Calculate total sampling weights (W) by district (prvdis) and age
table(is.na(gP4P3P2P1$W))
library(dplyr)
ADsumW_ByDisAge<-gP4P3P2P1 %>%
  group_by(prvdis, age) %>%
  summarise(sum_W = sum(W, na.rm = TRUE), .groups = "drop")

# Calculate total serology sampling weights (Wsero)
# Only include individuals with blood samples (girls_blood == 1)
library(dplyr)
ADsumWsero_ByDisAge<-gP4P3P2P1[gP4P3P2P1$girls_blood==1,] %>%
  group_by(prvdis, age) %>%
  summarise(sum_Wsero = sum(Wsero, na.rm = TRUE), .groups = "drop")
ADsumWsero_ByDisAge<-ADsumWsero_ByDisAge[!is.na(ADsumWsero_ByDisAge$prvdis),] # one record has missing prvdis and age, drop it 

# Merge total weights and serology weights by district and age
ADsumWW_ByDisAge<-merge(ADsumW_ByDisAge,ADsumWsero_ByDisAge[,c("prvdis","age","sum_Wsero")],by = c("prvdis","age"), all.x = TRUE) 

# Merge with external population data (total number of girls by district and age)
ADDisSumWWSumGirls <- merge(ADsumWW_ByDisAge,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age"), all.x = TRUE) 

# Visualization setup (color by age group)
age_levels <- sort(unique(ADDisSumWWSumGirls$age))
cols <- c("blue", "darkgreen", "orange")

# Scatter plot of total weights vs total girls for each district by age
plot(ADDisSumWWSumGirls$sum_W,
     ADDisSumWWSumGirls$girls,
     col = cols[as.factor(ADDisSumWWSumGirls$age)],
     pch = 19,
     xlab = "Sum of Sampling Weights",
     ylab = "Sum of Girls",
     main = "Girls vs Sum Sampling W. by District and Age")
for(i in seq_along(age_levels)) {
  d <- subset(ADDisSumWWSumGirls, age == age_levels[i])
  abline(lm(girls ~ sum_W, data = d), col = cols[i], lwd = 2)
}
legend("topleft",
       legend = age_levels,
       col = cols,
       pch = 19,
       lwd = 2,
       title = "Age")

# Compute adjustment factors
# AdjF: adjusts overall weights to match known population totals
# AdjFsero: adjusts serology weights (subset with blood samples)
ADDisSumWWSumGirls$AdjF<-ADDisSumWWSumGirls$girls / ADDisSumWWSumGirls$sum_W
ADDisSumWWSumGirls$AdjFsero<-ADDisSumWWSumGirls$girls / ADDisSumWWSumGirls$sum_Wsero

# Merge adjustment factors back into individual-level dataset
gP4P3P2P1AdjF<-merge(gP4P3P2P1,ADDisSumWWSumGirls[,c("prvdis","age","AdjF","AdjFsero")], by = c("prvdis","age"), all.x = "TRUE")
gP4P3P2P1AdjF$Wadj <- gP4P3P2P1AdjF$W*gP4P3P2P1AdjF$AdjF
gP4P3P2P1AdjF$Wadjsero <- gP4P3P2P1AdjF$Wsero*gP4P3P2P1AdjF$AdjFsero
colnames(gP4P3P2P1AdjF)

# Handle infinite values in adjusted serology weights
# (Occurs if sum_Wsero = 0 --> division by zero)
gP4P3P2P1AdjF$Wadjsero[is.infinite(gP4P3P2P1AdjF$Wadjsero)] <- NA

# Compare original vs adjusted weights
summary(gP4P3P2P1AdjF[,c("W","Wadj","Wsero","Wadjsero")]) 

ggplot(data = gP4P3P2P1AdjF, aes( x = W, y = Wadj)) +
  geom_point(colour = gP4P3P2P1AdjF$age)

# Check that adjusted weights sum to total number of girls by age and district
PrvDisGirls_W<-gP4P3P2P1AdjF[,c("prvdis","age","W", "Wadj")]
PrvDisGirls_Wsum <- PrvDisGirls_W %>%
  group_by(prvdis, age) %>%
  summarise(
    sum_W = sum(W, na.rm = TRUE),
    sum_Wadj = sum(Wadj, na.rm = TRUE),
    .groups = "drop"
  )

PrvDisGirls_Wsum<-merge(PrvDisGirls_Wsum,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age"), all = TRUE)

# plot final adj. weights vs. total target girls at district
ggplot(PrvDisGirls_Wsum, aes(x = girls, y = sum_Wadj, color = factor(age))) +
  geom_point() 

# Check that adjusted weights for serology data sum to total number of girls by age and district
PrvDisGirls_Wsero<-gP4P3P2P1AdjF[which(gP4P3P2P1AdjF$girls_blood==1),c("prvdis","age","Wsero","Wadjsero", "girls_blood")]
PrvDisGirls_Wserosum <- PrvDisGirls_Wsero %>%
  group_by(prvdis, age) %>%
  summarise(
    sum_Wsero = sum(Wsero, na.rm = TRUE),
    sum_Wadjsero = sum(Wadjsero, na.rm = TRUE),
    .groups = "drop"
  )

PrvDisGirls_Wserosum<-merge(PrvDisGirls_Wserosum,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age"), all = TRUE)

# plot final adj. weights for serology vs. total target girls at district
ggplot(PrvDisGirls_Wserosum, aes(x = girls, y = sum_Wadjsero, color = factor(age))) +
  geom_point()


# -----------------------------
# 10) Normalized Adj. weights
# -----------------------------

library(dplyr)
cP4P3P2P1AdjF <- cP4P3P2P1AdjF %>%
  mutate(NrmlzWadj = Wadj / mean(Wadj, na.rm = TRUE))

library(dplyr)
gP4P3P2P1AdjF <- gP4P3P2P1AdjF %>%
  mutate(NrmlzWadj = Wadj / mean(Wadj, na.rm = TRUE))

library(dplyr)
gP4P3P2P1AdjF <- gP4P3P2P1AdjF %>%
  mutate(NrmlzWadjsero = Wadjsero / mean(Wadjsero, na.rm = TRUE))


# -----------------------------
# 11) Export Excel
# -----------------------------

options(scipen = 999)

library(writexl)
write_xlsx(
  list(
    Caregivers_Weights =  cP4P3P2P1AdjF[,c("prvdis","age","caregiver_id","Wadj","NrmlzWadj")],
    Adolescents_Weights = gP4P3P2P1AdjF[,c("prvdis","age","ag_caregiver_id","Wadj","Wadjsero","NrmlzWadj","NrmlzWadjsero")],
    VisitConsentSuccess = PrvDis_VisitConsentSuccess,
    AdolescentsRefusalByDis = Refusal_ByDis,
    AdolescentsRefusal_ByPrv = Refusal_ByPrv,
    CaregiversHH = c,
    AdolocentsHH = g,
    DistrictGirls13t15 = DisG13t15,
    P1P2 = P1,
    P3 = P3,
    P4_girls = P4g ,
    P4_caregiver = P4cg ,
    Caregivers_P4P3P2P1 = cP4P3P2P1,
    Caregivers_P4P3P2P1AdjF = cP4P3P2P1AdjF,
    Adolescents_P4P3P2P1 = gP4P3P2P1,
    Adolescents_P4P3P2P1AdjF = gP4P3P2P1AdjF),
    "D/RW_HHSurvey_SamplingWeights_1April2026.xlsx"
)


