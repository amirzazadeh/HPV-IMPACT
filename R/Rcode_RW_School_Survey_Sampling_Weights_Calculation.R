# Last updated: 27 March 2026

# Rwanda School Survey 
# Calculate the sampling probabilities and analysis weight

# Rwanda School Survey — Sampling Probabilities & Analysis Weights
# Purpose: Compute school- and student-level selection probabilities, create survey weights,
# and apply post-stratification adjustment by district × age (13-15).

# For questions, contact: [ali.mirzazadeh@ucsf.edu](mailto:ali.mirzazadeh@ucsf.edu)

# data needed
# Rwanda_School_Sampling_Steps_Data.xlsx
# this excel file has the following sheets: 
## Students_survey: short dataset of studnets' survey by age.
## Schools_data: school dataset containing school-level information (e.g., eligible girls when school was visited).
## East_Schools_list: List of schools in the East region (full list ).
## East_Schools_selected: List of schools in the East region (full list + selected ones).
## South_Schools_list: List of schools in the South region (full list ).
## South_Schools_selected: List of schools in the South region (full list + selected ones).
## Girls_10_14_by_District: Total girls aged 10–14, organized by district.


rm(list = ls())

# -----------------------------
# 0) Set working directory
# -----------------------------

setwd("/Users/alimirzazadeh1/Documents/GitHub/RwandaSamplingWeights")
#setwd("/Users/alimirzazadeh1/Desktop/RwandaSamplingWeights")

# -----------------------------
# 1) import data
# -----------------------------

# Total girls in every districts for each age of 13, 14, 15
library(readxl)
DisG10t14 <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", 
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

# Sampling Frame data
library(readxl)
SL <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", 
                 sheet = "South_Schools_list")
table(SL$Dis)
SLs <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", 
                 sheet = "South_Schools_selected")
table(SLs$Dis, SLs$selected)

EL <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", 
                 sheet = "East_Schools_list")
table(EL$Dis)
ELs <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", 
                  sheet = "East_Schools_selected")
table(ELs$Dis,ELs$selected)


### import Stu data
library(readxl)
Stu <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", 
                  sheet = "Students_survey")
Stu$Enrolled<-1

# Fix this: two schools had same ID and names.
# School ID: 210603, School name: GS GATAGARA, date of collection: 09 JUNE 2025
# School ID: 210804, School name: GS KATARARA, date of collection: 11 JUNE 2025

#View(Stu[Stu$ss_school==210804,]) 
#View(Stu[Stu$ss_school==210804 & Stu$today=="9-Jun-25",]) 
Stu$note<-NA
for (i in 1:nrow(Stu)) {
  if (Stu$ss_school[i] == 210804 & Stu$today[i] == "9-Jun-25") {
    Stu$note[i] <- "changed to School ID 210603 (from 210804), School name: GS GATAGARA (from GS KATARARA)"
    Stu$ss_school[i] <- 210603
    Stu$ss_school_name[i] <- "GS GATAGARA"
  }
}

# Count students that were enrolled from each school
library(dplyr)
Stu_PerSchool <- Stu %>%
  group_by(ss_school) %>%
  summarise(Enrolled = sum(Enrolled, na.rm = TRUE), .groups = "drop")

#######################
# adding all schools from South and East into one data
SELs<-rbind(SLs,ELs)
SELs$prvdis<-paste0(SELs$province,"_",SELs$district)
table(SELs$prvdis)
table(SELs$prvdis,SELs$selected)

library(dplyr)
SELs <- SELs %>%
  group_by(prvdis) %>%
  mutate(district_GirlStu_total = sum(students_female, na.rm = TRUE)) %>%
  ungroup()

SELs$P1<-SELs$students_female/SELs$district_GirlStu_total*15
summary(SELs$P1)
#View(SELs[,c("prvdis","school_code","students_female","district_GirlStu_total","P1")])

############################################################################## 
## import Schools_data and clean the data, and calculate the sampling weights

library(readxl)
SD <- read_excel("D/Rwanda_School_Sampling_Steps_Data.xlsx", sheet = "Schools_data")
# View(SD)
head(SD)
colnames(SD)

SD2 <- SD[, c(
  "sn","school_name","school_code","school_type",
  "province","district","location","urban_rural",
  "n_girls","n_boys","n_total",
  "n_GirlsInRoom13","n_EligGirls13","n_EligGirlsRand13","n_EligGirlsAssnt13","n_EligGirlsIntrvw13",
  "n_GirlsInRoom14","n_EligGirls14","n_EligGirlsRand14","n_EligGirlsAssnt14","n_EligGirlsIntrvw14",
  "n_GirlsInRoom15","n_EligGirls15","n_EligGirlsRand15","n_EligGirlsAssnt15","n_EligGirlsIntrvw15")]

table(SD2$province, useNA = "ifany")

library(dplyr)
SD2 <- SD2 %>%
  mutate(
    province = case_when(
      toupper(province) == "EAST" ~ "East",
      toupper(province) %in% c("SOUTH", "SOUTHERN") ~ "South",
      TRUE ~ province ## anything else -> keep the original value
    )
  )

table(SD2$province, useNA = "ifany")
# View(SD2[is.na(SD2$province),])
# View(SD2[SD2$district=="BUGESERA",])
SD2$province[SD2$district=="BUGESERA" & is.na(SD2$province) ] <- "East"
table(SD2$province, useNA = "ifany") # final check

# use a loop to check several variables
vars <- c("province", "school_type", "urban_rural")
# Loop through and print frequency tables
for (v in vars) {
  cat("\n###", v, "###\n")
  print(table(SD2[[v]], useNA = "ifany"))
}


SD2$school_type2 <- tolower(SD2$school_type)
SD2$school_type2 <- ifelse(
  grepl("g\\.a|government aided", SD2$school_type2),
  "government_aided",
  ifelse(
    grepl("public", SD2$school_type2),
    "public",
    ifelse(
      grepl("private", SD2$school_type2),
      "private",
      "other"
    )
  )
)
table(SD2$school_type2)

SD2$urban_rural2 <- tolower(trimws(SD2$urban_rural))
SD2$urban_rural2 <- ifelse(SD2$urban_rural2 == "rural", "rural",
                               ifelse(SD2$urban_rural2 == "urban", "urban",
                                      "mixed"))
table(SD2$urban_rural2)

## Calculate sampling weights:
head(SD2[,c("school_code","n_GirlsInRoom13","n_EligGirls13","n_EligGirlsRand13","n_EligGirlsAssnt13","n_EligGirlsIntrvw13")])

## calculate the sampling probability for each age group (i.e., 13, 14, 15)
SD2$y13_P2<- SD2$n_EligGirlsIntrvw13/SD2$n_EligGirls13
SD2$y14_P2<- SD2$n_EligGirlsIntrvw14/SD2$n_EligGirls14
SD2$y15_P2<- SD2$n_EligGirlsIntrvw15/SD2$n_EligGirls15

# View(SD2[,c("school_code","n_GirlsInRoom13","n_EligGirls13","n_EligGirlsRand13","n_EligGirlsAssnt13","n_EligGirlsIntrvw13","y13_P2")])

## make a short data frame that include sampling probability for age groups
SampProb <- SD2[,c("school_code","province","district","school_type2","urban_rural2","n_girls","n_EligGirls13","n_EligGirlsIntrvw13","n_EligGirls14","n_EligGirlsIntrvw14","n_EligGirls15","n_EligGirlsIntrvw15","y13_P2","y14_P2","y15_P2")]
head(SampProb)
summary(SampProb[, c("y13_P2","y14_P2","y15_P2")])

# reshape the data into long format
library(dplyr)
library(tidyr)
SampProb_long <- SampProb %>%
  pivot_longer(
    cols = starts_with("y"),
    names_to = "year_prob",
    values_to = "P2"
  ) %>%
  separate(year_prob, into = c("year", "var"), sep = "_") %>%
  mutate(year = readr::parse_number(year))

head(SampProb_long)
summary(SampProb_long$P2)

library(dplyr)
SampProb_long <- SampProb_long %>%
  mutate(
    n_EligGirls = case_when(
      year == 13 ~ n_EligGirls13,
      year == 14 ~ n_EligGirls14,
      year == 15 ~ n_EligGirls15,
      TRUE ~ NA_real_
    ),
    n_GirlsInterviewed = case_when(
      year == 13 ~ n_EligGirlsIntrvw13,
      year == 14 ~ n_EligGirlsIntrvw14,
      year == 15 ~ n_EligGirlsIntrvw15,
      TRUE ~ NA_real_
    )
  )


D <- merge(SD2,Stu_PerSchool, by.x = "school_code",by.y = "ss_school", all = TRUE)
D$Enrolled2 <- rowSums(D[, c("n_EligGirlsIntrvw13",
                            "n_EligGirlsIntrvw14",
                            "n_EligGirlsIntrvw15")],na.rm = TRUE)
D$Enrolled_Diff<-D$Enrolled2-D$Enrolled
#View(D)


### merging school selection P1 with SampProb_long
SampProb_longP1P2<-merge(SampProb_long,SELs[SELs$selected==1,c("prvdis","school_code","students_female","district_GirlStu_total","P1")], by = "school_code", all = TRUE)
SampProb_longP1P2$P<-SampProb_longP1P2$P2*SampProb_longP1P2$P1
SampProb_longP1P2$W<-1/SampProb_longP1P2$P
summary(SampProb_longP1P2[,c("P2","P1","P","W")])
hist(SampProb_longP1P2$W)
#View(SampProb_longP1P2)

V2<-c("school_code","province","district","prvdis","school_type2","urban_rural2","n_girls","year",
      "n_EligGirls","n_GirlsInterviewed","students_female","district_GirlStu_total","P1","P2","P","W")
SamplingWeights_long<-SampProb_longP1P2[,V2]
colnames(SamplingWeights_long)<-c("school_code","province","district","prvdis","school_type","urban_rural","n_girls","age",
                                  "n_EligGirls","n_GirlsInterviewed","students_female","district_GirlStu_total","P1","P2","P","W")

# Merge students data with the sampling weights 
StuW <- merge(Stu, SamplingWeights_long, by.x = c("ss_school","Age"), by.y = c("school_code","age"), all = TRUE)
colnames(StuW)
colnames(StuW)[colnames(StuW) == "Age"] <- "age"

# ------------------------------------------------------------------
# 3) Post Stratification Adjustment for Students in School Survey
# ------------------------------------------------------------------

library(dplyr)
STUsumW_ByDisAge<-StuW %>%
  group_by(prvdis, age) %>%
  summarise(sum_W = sum(W, na.rm = TRUE), .groups = "drop")

STUDisSumWSumGirls <- merge(STUsumW_ByDisAge,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age")) 

age_levels <- sort(unique(STUDisSumWSumGirls$age))
cols <- c("blue", "darkgreen", "orange")
plot(STUDisSumWSumGirls$sum_W,
     STUDisSumWSumGirls$girls,
     col = cols[as.factor(STUDisSumWSumGirls$age)],
     pch = 19,
     xlab = "Sum of Sampling Weights",
     ylab = "Sum of Girls",
     main = "Girls vs Sum Sampling W. by District and Age")

for(i in seq_along(age_levels)) {
  d <- subset(STUDisSumWSumGirls, age == age_levels[i])
  abline(lm(girls ~ sum_W, data = d), col = cols[i], lwd = 2)
}
legend("topleft",
       legend = age_levels,
       col = cols,
       pch = 19,
       lwd = 2,
       title = "Age")

STUDisSumWSumGirls$AdjF<-STUDisSumWSumGirls$girls / STUDisSumWSumGirls$sum_W

StuWAdjF<-merge(StuW,STUDisSumWSumGirls[,c("prvdis","age","girls","AdjF")], by = c("prvdis","age"), all.x = "TRUE")

StuWAdjF$Wadj <- StuWAdjF$W*StuWAdjF$AdjF

library(ggplot2)
ggplot(data = StuWAdjF, aes( x = W, y = Wadj)) +
  geom_point(colour = StuWAdjF$age)

PrvDisStu_W<- aggregate(cbind(W, Wadj) ~ prvdis + age,
                          data = StuWAdjF,
                          sum,
                          na.rm = TRUE)

PrvDisStu_W<-merge(PrvDisStu_W,DisG13t15_long[,c("prvdis","age","girls")], by = c("prvdis","age"), all = TRUE)

# plot final adj. weights vs. total target girls at district
ggplot(data = PrvDisStu_W, aes( x = girls, y = Wadj)) +
  geom_point(colour = PrvDisStu_W$age)



# -----------------------------
# 4) Export Excel
# -----------------------------

options(scipen = 999)
V<-c("sn","school_name","school_code","province","district","location",
     "school_type2","urban_rural2", "n_girls","n_boys","n_total",
     "n_GirlsInRoom13","n_EligGirls13","n_EligGirlsRand13","n_EligGirlsAssnt13","n_EligGirlsIntrvw13",
     "n_GirlsInRoom14","n_EligGirls14","n_EligGirlsRand14","n_EligGirlsAssnt14","n_EligGirlsIntrvw14",
     "n_GirlsInRoom15","n_EligGirls15","n_EligGirlsRand15","n_EligGirlsAssnt15","n_EligGirlsIntrvw15")

library(ggplot2)
ggplot(StuWAdjF, aes(x = Wadj)) +
  geom_histogram(bins = 30, color = "black", fill = "skyblue") +
  facet_wrap(~ prvdis) +
  theme_minimal() +
  labs(title = "Sampling Weights After Post Stratification Adjustnmet by District - Rwanda School Survey",
       x = "Adj. Sampling Weights",
       y = "Count")

library(writexl)
write_xlsx(
  list(
    Student_weights = StuWAdjF[,c("prvdis","ss_school","ss_school_name","adolescent_id","Wadj")],
    Student_data = Stu,
    School_Sampling_Frame = SELs,
    School_data = SD2[,V],
    InSchool_Wide = SampProb,
    InSchool_Long = SampProb_long,
    DistrictGirls13t15 = DisG13t15,
    SamplingWeights_long = SamplingWeights_long,
    Student_data_weights = StuWAdjF),
    "RW_SchoolSurvey_SamplingWeights_27March2026.xlsx"
)




