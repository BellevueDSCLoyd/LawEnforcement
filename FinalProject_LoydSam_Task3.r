
# Assignment: FinalProject_LoydSam

# Name: Loyd, Sam

# Date: 18 December 2018

# Set needed library functions

# Turn off scientific notation to a degree
options(scipen = 999)

library(ggplot2)
library(Rmisc)
library(mice)
library(pastecs)
library(moments)
library(corrplot)
library(ggpubr)
library(psych)
library(dplyr)
library(plyr)
library(grid)
library(knitr)
# library(kableExtra)
library(lubridate)
library(psych)
library(car)
library(naniar)
library(QuantPsyc)
library(ggcorrplot)
library(reghelper)
library(ggstance)
library(jtools)
library(ROCR)
library(caret)


# TList <- list.files(path="C:/Users/Loyd/Downloads/STCRIME/", pattern = "nibrs_arrestee.csv", recursive=TRUE, full.names = TRUE)
# # Look at the list
# TList
# # Initialize the frames
# TotFile <- NULL
# TmpFile <- NULL
# for (TValue in TList) {
#  # Data set is unruly large.  Picking out needed
#  # columns only.  It was bringing down my laptop to run
#  TmpFileA <- read.csv(TValue)
#  TmpFile <- TmpFileA[,c("arrest_date","offense_type_id","age_num","sex_code","ethnicity_id","race_id","resident_code")]
#  TmpFile$state <- substring(TValue,34,35)
#  TmpFile$year <- substring(TValue,37,40)
#  TotFile <- rbind(TotFile,TmpFile)
# }
# write.csv(TotFile, file = "Totalnibrs_arrests.csv", row.names = FALSE)

# str(TmpFileA)
# TotFile <- NULL
# TotFile <- read.csv(file = "Totalnibrs_arrests.csv")

# CleanNIBR <- TotFile
UnsortedCleanNIBR <- read.csv(file = "Totalnibrs_arrests.csv",stringsAsFactors = FALSE, na.strings = c("","NA"))

UnsortedCleanNIBR$arrest_date <- ymd_hms(UnsortedCleanNIBR$arrest_date)
UnsortedCleanNIBR$resident_code <- as.character(UnsortedCleanNIBR$resident_code)
UnsortedCleanNIBR$sex_code <- as.character(UnsortedCleanNIBR$sex_code)
# RACE_CAUC_CNTR = 0
# RACE_BLACK_CNTR = 0
# RACE_ASIAN_CNTR = 0
# RACE_NAT_CNTR = 0
# ETHN_HIS_CNTR = 0
# ETHN_NON_CNTR = 0
# RESD_NON_CNTR = 0
# RESD_RES_CNTR = 0
# BOTH_U_CNTR = 0
# GEND_M_CNTR = 0
# GEND_F_CNTR = 0
# ARREST_CNTR = 0
# NIBRTOTAL <- NULL
# NEWROW <- NULL
SortedCleanNIBR <- UnsortedCleanNIBR[order(UnsortedCleanNIBR$arrest_date),]
# for (VRow in 1:nrow(SortedCleanNIBR)) 
# {
#   VDate <- SortedCleanNIBR[VRow,"arrest_date"]
#   if (VRow == 1) {
#     VoldDate <- SortedCleanNIBR[VRow,"arrest_date"]
#   }
#   if ( VDate != VoldDate) {
#     NEWROW <- data.frame( arrest_date = VoldDate , CAUC_TOTAL = RACE_CAUC_CNTR, 
#                           BLACK_TOTAL = RACE_BLACK_CNTR, 
#                           NATIVE_TOTAL = RACE_NAT_CNTR,
#                           ASIAN_TOTAL = RACE_ASIAN_CNTR, 
#                           HISP_TOTAL = ETHN_HIS_CNTR, 
#                           NON_HISP_TOTAL = ETHN_NON_CNTR,
#                           NON_RESD_TOTAL = RESD_NON_CNTR, 
#                           RESD_TOTAL = RESD_RES_CNTR,
#                           UNDOC_TOTAL = BOTH_U_CNTR, 
#                           MALE_TOTAL = GEND_M_CNTR, 
#                           FEMALE_TOAL = GEND_F_CNTR,
#                           ARREST_TOTAL = ARREST_CNTR)
#     NIBRTOTAL <- rbind(NIBRTOTAL,NEWROW)
#     RACE_CAUC_CNTR = 0
#     RACE_BLACK_CNTR = 0
#     RACE_ASIAN_CNTR = 0
#     RACE_NAT_CNTR = 0
#     ETHN_HIS_CNTR = 0
#     ETHN_NON_CNTR = 0
#     RESD_NON_CNTR = 0
#     RESD_RES_CNTR = 0
#     BOTH_U_CNTR = 0
#     GEND_M_CNTR = 0
#     GEND_F_CNTR = 0
#     ARREST_CNTR = 0
#     print(VDate)
#     
#   }
#   if (!is.na(SortedCleanNIBR[VRow,"race_id"]) & SortedCleanNIBR[VRow,"race_id"] == 1) {RACE_CAUC_CNTR = RACE_CAUC_CNTR + 1}
#   if (!is.na(SortedCleanNIBR[VRow,"race_id"]) & SortedCleanNIBR[VRow,"race_id"] == 2) {RACE_BLACK_CNTR = RACE_BLACK_CNTR + 1}
#   if (!is.na(SortedCleanNIBR[VRow,"race_id"]) & SortedCleanNIBR[VRow,"race_id"] == 3) {RACE_NAT_CNTR = RACE_NAT_CNTR + 1}
#   if (!is.na(SortedCleanNIBR[VRow,"race_id"]) & SortedCleanNIBR[VRow,"race_id"] == 4) {RACE_ASIAN_CNTR = RACE_ASIAN_CNTR + 1}
#   
#   if (!is.na(SortedCleanNIBR[VRow,"ethnicity_id"]) & SortedCleanNIBR[VRow,"ethnicity_id"] == 1) {ETHN_HIS_CNTR = ETHN_HIS_CNTR + 1}
#   if (!is.na(SortedCleanNIBR[VRow,"ethnicity_id"]) & SortedCleanNIBR[VRow,"ethnicity_id"] == 2) {ETHN_NON_CNTR = ETHN_NON_CNTR + 1}
#   
#   if (!is.na(SortedCleanNIBR[VRow,"resident_code"]) & SortedCleanNIBR[VRow,"resident_code"] == "R") {RESD_RES_CNTR = RESD_RES_CNTR + 1}
#   if (!is.na(SortedCleanNIBR[VRow,"resident_code"]) & SortedCleanNIBR[VRow,"resident_code"] == "N") {RESD_NON_CNTR = RESD_NON_CNTR + 1}
#   
#   if (!is.na(SortedCleanNIBR[VRow,"sex_code"]) &SortedCleanNIBR[VRow,"sex_code"] == "F" ) {GEND_F_CNTR = GEND_F_CNTR + 1}
#   if (!is.na(SortedCleanNIBR[VRow,"sex_code"]) &SortedCleanNIBR[VRow,"sex_code"] == "M" ) {GEND_M_CNTR = GEND_M_CNTR + 1}
#   
#   if (!is.na(SortedCleanNIBR[VRow,"resident_code"]) & SortedCleanNIBR[VRow,"resident_code"] == "N" & !is.na(SortedCleanNIBR[VRow,"ethnicity_id"]) & SortedCleanNIBR[VRow,"ethnicity_id"] == 1)  {BOTH_U_CNTR = BOTH_U_CNTR + 1}
#   ARREST_CNTR = ARREST_CNTR + 1
#   
#   VoldDate <- VDate
# }
# 
# write.csv(NIBRTOTAL, file = "summary_by_date_nibrs_arrests.csv", row.names = FALSE)

NIBRTOTAL <- read.csv(file = "summary_by_date_nibrs_arrests.csv",stringsAsFactors = FALSE, na.strings = c("","NA"))

colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="arrest_date"] <- "Arrest_Date"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="CAUC_TOTAL"] <- "Caucasion"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="BLACK_TOTAL"] <- "Black"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="NATIVE_TOTAL"] <- "Native"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="ASIAN_TOTAL"] <- "Asian"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="HISP_TOTAL"] <- "Hispanic"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="NON_HISP_TOTAL"] <- "Non_Hispanic"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="NON_RESD_TOTAL"] <- "Non_Resident"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="RESD_TOTAL"] <- "Resident"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="UNDOC_TOTAL"] <- "Undocumented"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="MALE_TOTAL"] <- "Male"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="FEMALE_TOAL"] <- "Female"
colnames(NIBRTOTAL)[colnames(NIBRTOTAL)=="ARREST_TOTAL"] <- "Arrest_Total"




str(NIBRTOTAL)

NIBRTOTAL$Arrest_Date <- ymd(NIBRTOTAL$Arrest_Date)

#Remove partial 2017 data 
NIBRTOTAL <- NIBRTOTAL[NIBRTOTAL$Arrest_Date < "2017-01-01",]


CleanNIBR <- SortedCleanNIBR
CleanNIBR$ethnicity_id[is.na(CleanNIBR$ethnicity_id)] <- 3
CleanNIBR$resident_code[is.na(CleanNIBR$resident_code)] <- "U"
CleanNIBR$race_id[is.na(CleanNIBR$race_id)] <- 0


CleanNIBR$resident_code <- factor(CleanNIBR$resident_code, labels = c("Resident","Non","Unknown"))
CleanNIBR$race_id <- factor(CleanNIBR$race_id, labels = c("Unknown","White","Black","Native","Asian","Other"))
CleanNIBR$ethnicity_id <- factor(CleanNIBR$ethnicity_id, labels = c("Hispanic","NonHispanic","Unknown"))
CleanNIBR$state <- factor(CleanNIBR$state)

CleanNIBR$offense_type_id <- factor(CleanNIBR$offense_type_id)
CleanNIBR$year <- factor(CleanNIBR$year)
CleanNIBR$state <- factor(CleanNIBR$state)
CleanNIBR$year <- factor(CleanNIBR$year)
CleanNIBR$sex_code <- factor(CleanNIBR$sex_code)

#This was a pain
CleanNIBR$offense_type_id <- factor(CleanNIBR$offense_type_id, labels =
                                      c("Swindling", "Statuatory Rape", "Sexual Assault w/Object", "Vandalism", "NonViolent Family Offenses", "Motor Parts Theft", "Obscene Materials" ,"Sports Tampering",  
                                        "DUI", "Counterfitting", "Welfare Fraud", "PickPocketing", "Theft from Vehicle", "Promoting Prostitution", "Drug Violations", "Wire Fraud", "Purse Snatching", "Runaway",
                                        "Arson", "Vehicle Theft", "Drunkenness", "Shoplifting", "Promoting Gambling", "Bad Checks", "Extortion/Blackmail", "Aggravated Assault", "Stolen Property", 
                                        "Kidnapping/Abduction", "Prostitution", "Betting Wagering", "Murder/Nonnegligent Manslaughter", "Peeping Tom", "Trespassing", "Drug Equipment Violations", "Rape", "Embezzlement", "Negligent Manslaughter",
                                        "Weapon Law Violations", "Robbery", "Credit Card/ATM Fraud", "Curfew/Loitering/Vagrancy", "Sodomy", "Intimidation", "All Other Larceny", "Impersonation", "Theft From Building", "Other Offenses",
                                        "Burglary", "Coin-Operated Machine Theft", "Simple Assault", "Liquor Violations", "Disorderly Conduct", "Gambling Equipment Violation", "Incest", "Fondling", "Bribery" , "Trafficking/Sex Acts", 
                                        "Trafficking, Involuntary Servitude", "Purchasing Prostitution", "Animal Cruelty", "Identity Theft", "Hacking" ))

NatArr <- read.csv("arrests_national_adults.csv");
#str(NatArr)
# Gather only 2015 & 2016 data
NatArr1416 <- subset(NatArr, year >= 2014 & year <= 2016)

NatArr1416$year <- factor(NatArr1416$year)

NatArr1416 <- NatArr1416[,c("year","offense_code","offense_name","population","total_male","total_female","race_population","white","black","asian_pacific_islander","american_indian")]

# The state_abbr column has no values so I decided to just remove it.  
# It was pointless.  Determined by miss_var_summary function()

ROUT_DOT_WHITE <- ggplot(NatArr1416, aes(x=NatArr1416$offense_name, y=NatArr1416$white, color=factor(NatArr1416$year))) + 
  geom_point(size = 3, alpha=0.5 ) +   # Draw points
  theme(legend.title=element_blank()) +
  geom_segment(aes(x=NatArr1416$offense_name, 
                   xend=NatArr1416$offense_name, 
                   y=min(NatArr1416$white), 
                   yend=2000000), 
               linetype="solid", 
               size=0.1) +   # Draw dashed lines
  labs(title="White Crime (2014-2016)", x = "Offense", y = "Frequency",
       subtitle="Offense to Frequency", 
       caption="source: FBI") +  
  scale_y_continuous(limits = c(0, 2000000)) +
  coord_flip()

ROUT_DOT_BLACK <- ggplot(NatArr1416, aes(x=NatArr1416$offense_name, y=NatArr1416$black, color=factor(NatArr1416$year))) + 
  geom_point(size = 3, alpha=0.5 ) +   # Draw points
  theme(legend.title=element_blank()) +
  geom_segment(aes(x=NatArr1416$offense_name, 
                   xend=NatArr1416$offense_name, 
                   y=min(NatArr1416$black), 
                   yend=2000000), 
               linetype="solid", 
               size=0.1) +   # Draw dashed lines
  labs(title="Black/African American Crime (2014-2016)", x = "Offense", y = "Frequency",
       subtitle="Offense to Frequency", 
       caption="source: FBI") +  
  scale_y_continuous(limits = c(0, 2000000)) +
  coord_flip()


FatPShoot <- read.csv("fatal-police-shootings-data.csv", na.strings=c(""));
# Change date to date format
FatPShoot$date <- as.Date(FatPShoot$date, format= "%Y-%m-%d" )
# str(FatPShoot)
#Gather only 2014 & 2016 data
FatPShoot1416 <- subset(FatPShoot, date > "2013-12-31" & date < "2017-01-01")

SortedFatPShoot1416 <- FatPShoot1416[order(FatPShoot1416$date),]

RACE_CAUC_CNTR = 0
RACE_BLACK_CNTR = 0
RACE_ASIAN_CNTR = 0
ETHN_HIS_CNTR = 0
FATALITY_CNTR = 0
FPSHOOTTOTAL <- NULL
NEWROW <- NULL

for (VRow in 1:nrow(SortedFatPShoot1416)) 
{
   VDate <- SortedFatPShoot1416[VRow,"date"]
   if (VRow == 1) {
     VoldDate <- SortedFatPShoot1416[VRow,"date"]
   }
   if ( VDate != VoldDate) {
     NEWROW <- data.frame( SHOOTING_DATE = VoldDate , CAUC_TOTAL = RACE_CAUC_CNTR, 
                           BLACK_TOTAL = RACE_BLACK_CNTR, 
                           ASIAN_TOTAL = RACE_ASIAN_CNTR, 
                           HISP_TOTAL = ETHN_HIS_CNTR, 
                           FATALITY_TOTAL = FATALITY_CNTR)
     FPSHOOTTOTAL <- rbind(FPSHOOTTOTAL,NEWROW)
     RACE_CAUC_CNTR = 0
     RACE_BLACK_CNTR = 0
     RACE_ASIAN_CNTR = 0
     RACE_NAT_CNTR = 0
     ETHN_HIS_CNTR = 0
     FATALITY_CNTR = 0
     print(VDate)
     
   }
   if (!is.na(SortedFatPShoot1416[VRow,"race"]) & SortedFatPShoot1416[VRow,"race"] == "W") {RACE_CAUC_CNTR = RACE_CAUC_CNTR + 1}
   if (!is.na(SortedFatPShoot1416[VRow,"race"]) & SortedFatPShoot1416[VRow,"race"] == "B") {RACE_BLACK_CNTR = RACE_BLACK_CNTR + 1}
   if (!is.na(SortedFatPShoot1416[VRow,"race"]) & SortedFatPShoot1416[VRow,"race"] == "A") {RACE_ASIAN_CNTR = RACE_ASIAN_CNTR + 1}
   if (!is.na(SortedFatPShoot1416[VRow,"race"]) & SortedFatPShoot1416[VRow,"race"] == "H") {ETHN_HIS_CNTR = ETHN_HIS_CNTR + 1}
   FATALITY_CNTR = FATALITY_CNTR + 1
   
   VoldDate <- VDate
}

colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="FATALITY_TOTAL"] <- "Fatality_Total"
colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="SHOOTING_DATE"] <- "Shooting_Date"
colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="CAUC_TOTAL"] <- "Caucasion"
colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="BLACK_TOTAL"] <- "Black"
colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="NATIVE_TOTAL"] <- "Native"
colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="ASIAN_TOTAL"] <- "Asian"
colnames(FPSHOOTTOTAL)[colnames(FPSHOOTTOTAL)=="HISP_TOTAL"] <- "Hispanic"



Wbin=1
FP_CAUC_HIST <- ggplot(FPSHOOTTOTAL, aes(x = FPSHOOTTOTAL$Caucasion)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Caucasion Deaths",
       x ="Deaths Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(FPSHOOTTOTAL$Caucasion),
                            sd = sd(FPSHOOTTOTAL$Caucasion)))

FP_BLACK_HIST <- ggplot(FPSHOOTTOTAL, aes(x = FPSHOOTTOTAL$Black)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Black/African American Deaths",
       x ="Deaths Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(FPSHOOTTOTAL$Black),
                            sd = sd(FPSHOOTTOTAL$Black)))

FP_HIS_HIST <- ggplot(FPSHOOTTOTAL, aes(x = FPSHOOTTOTAL$Hispanic)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Hispanic Deaths",
       x ="Deaths Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(FPSHOOTTOTAL$Hispanic),
                            sd = sd(FPSHOOTTOTAL$Hispanic)))

FP_TOT_HIST <- ggplot(FPSHOOTTOTAL, aes(x = FPSHOOTTOTAL$Fatality_Total)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Total Deaths",
       x ="Deaths Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(FPSHOOTTOTAL$Fatality_Total),
                            sd = sd(FPSHOOTTOTAL$Fatality_Total)))

FP_TOT_QQ <- ggplot(FPSHOOTTOTAL, aes(sample=FPSHOOTTOTAL$Fatality_Total)) +
  stat_qq() +
  labs(title="Total Deaths",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

FP_BLACK_QQ <- ggplot(FPSHOOTTOTAL, aes(sample=FPSHOOTTOTAL$Black)) +
  stat_qq() +
  labs(title="Black Deaths",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

FP_CAUC_QQ <- ggplot(FPSHOOTTOTAL, aes(sample=FPSHOOTTOTAL$Caucasion)) +
  stat_qq() +
  labs(title="Caucasion Deaths",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

FP_HISP_QQ <- ggplot(FPSHOOTTOTAL, aes(sample=FPSHOOTTOTAL$Hispanic)) +
  stat_qq() +
  labs(title="Hispanic Deaths",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")


cor_matrix_fp <- cor(FPSHOOTTOTAL[,2:6], method = c("kendall"))

ROUT_CIR_FP <- corrplot(cor_matrix_fp, method = "circle")


Leoka <- read.csv("LEOKA_ASSAULT_TIME_WEAPON_INJURY.csv", na.strings=c(""));


# Gather only 2015 & 2016 data
Leoka1416 <- subset(Leoka, DATA_YEAR >= 2014 & DATA_YEAR <= 2016)

Leoka1416$DATA_YEAR <- factor(Leoka1416$DATA_YEAR)
Leoka1416$POPULATION_GROUP_DESC <- as.character(Leoka1416$POPULATION_GROUP_DESC)
Leoka1416$POPULATION_GROUP_DESC[Leoka1416$POPULATION_GROUP_DESC == "Possessions (Puerto Rico, Guam, Canal Zone, Virgin Islands, and American Samoa)"] <- "Possessions"
Leoka1416$POPULATION_GROUP_DESC <- as.factor(Leoka1416$POPULATION_GROUP_DESC)
# not useing pub_agency_unit so removing
Leoka1416 <- within(Leoka1416, rm(PUB_AGENCY_UNIT))

range(NIBRTOTAL$Undocumented)

Wbin=20
ROUT_UND_HIST <- ggplot(NIBRTOTAL, aes(x = NIBRTOTAL$Undocumented)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Undocumented Arrests",
       x ="Arrests Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(NIBRTOTAL$Undocumented),
                            sd = sd(NIBRTOTAL$Undocumented)))

LK_0_2_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_0001_0200_CNT)) +
  geom_histogram(binwidth = 20, aes(y = ..density..), alpha = 0.5) +
  labs(title="00:01 to 2:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_0001_0200_CNT),
                            sd = sd(Leoka1416$TIME_0001_0200_CNT)))

LK_2_4_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_0201_0400_CNT)) +
  geom_histogram(binwidth = 10, aes(y = ..density..), alpha = 0.5) +
  labs(title="2:01 to 4:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_0201_0400_CNT),
                            sd = sd(Leoka1416$TIME_0201_0400_CNT)))

LK_4_6_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_0401_0600_CNT)) +
  geom_histogram(binwidth = 5, aes(y = ..density..), alpha = 0.5) +
  labs(title="4:01 to 6:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_0401_0600_CNT),
                            sd = sd(Leoka1416$TIME_0401_0600_CNT)))

LK_6_8_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_0201_0400_CNT)) +
  geom_histogram(binwidth = 5, aes(y = ..density..), alpha = 0.5) +
  labs(title="6:01 to 8:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_0601_0800_CNT),
                            sd = sd(Leoka1416$TIME_0601_0800_CNT)))

LK_8_10_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_0801_1000_CNT)) +
  geom_histogram(binwidth = 5, aes(y = ..density..), alpha = 0.5) +
  labs(title="8:01 to 10:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_0801_1000_CNT),
                            sd = sd(Leoka1416$TIME_0801_1000_CNT)))

LK_10_12_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_1001_1200_CNT)) +
  geom_histogram(binwidth = 2, aes(y = ..density..), alpha = 0.5) +
  labs(title="10:01 to 12:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_1001_1200_CNT),
                            sd = sd(Leoka1416$TIME_0601_0800_CNT)))

LK_12_14_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_1201_1400_CNT)) +
  geom_histogram(binwidth = 2, aes(y = ..density..), alpha = 0.5) +
  labs(title="12:01 to 14:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_1201_1400_CNT),
                            sd = sd(Leoka1416$TIME_1201_1400_CNT)))

LK_14_16_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_1401_1600_CNT)) +
  geom_histogram(binwidth = 2, aes(y = ..density..), alpha = 0.5) +
  labs(title="14:01 to 16:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_1401_1600_CNT),
                            sd = sd(Leoka1416$TIME_1401_1600_CNT)))

LK_16_18_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_1601_1800_CNT)) +
  geom_histogram(binwidth = 3, aes(y = ..density..), alpha = 0.5) +
  labs(title="16:01 to 18:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_1601_1800_CNT),
                            sd = sd(Leoka1416$TIME_1601_1800_CNT)))

LK_18_20_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_1801_2000_CNT)) +
  geom_histogram(binwidth = 4, aes(y = ..density..), alpha = 0.5) +
  labs(title="18:01 to 20:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_1801_2000_CNT),
                            sd = sd(Leoka1416$TIME_1801_2000_CNT)))

LK_20_22_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_2001_2200_CNT)) +
  geom_histogram(binwidth = 3, aes(y = ..density..), alpha = 0.5) +
  labs(title="20:01 to 22:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_2001_2200_CNT),
                            sd = sd(Leoka1416$TIME_2001_2200_CNT)))

LK_22_00_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$TIME_2201_0000_CNT)) +
  geom_histogram(binwidth = 3, aes(y = ..density..), alpha = 0.5) +
  labs(title="22:01 to 00:00",
       x ="Officer Injuries Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$TIME_2201_0000_CNT),
                            sd = sd(Leoka1416$TIME_2201_0000_CNT)))

LK_TOT_HIST <- ggplot(Leoka1416, aes(x = Leoka1416$LEOKA_FELONY_KILLED)) +
  geom_histogram(binwidth = .1, aes(y = ..density..), alpha = 0.5) +
  labs(title="Felony Deaths",
       x ="Officer Felony Deaths Per Year", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(Leoka1416$LEOKA_FELONY_KILLED),
                            sd = sd(Leoka1416$LEOKA_FELONY_KILLED)))

LK_00_02_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_0001_0200_CNT)) +
  stat_qq() +
  labs(title="00:01 to 02:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_02_04_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_0201_0400_CNT)) +
  stat_qq() +
  labs(title="02:01 to 04:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_04_06_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_0401_0600_CNT)) +
  stat_qq() +
  labs(title="04:01 to 06:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_06_08_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_0601_0800_CNT)) +
  stat_qq() +
  labs(title="06:01 to 08:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_08_10_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_0801_1000_CNT)) +
  stat_qq() +
  labs(title="08:01 to 10:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_10_12_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_1001_1200_CNT)) +
  stat_qq() +
  labs(title="10:01 to 12:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_12_14_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_1201_1400_CNT)) +
  stat_qq() +
  labs(title="12:01 to 14:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_14_16_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_1401_1600_CNT)) +
  stat_qq() +
  labs(title="14:01 to 16:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_16_18_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_1601_1800_CNT)) +
  stat_qq() +
  labs(title="16:01 to 18:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_18_20_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_1801_2000_CNT)) +
  stat_qq() +
  labs(title="18:01 to 20:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_20_22_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_2001_2200_CNT)) +
  stat_qq() +
  labs(title="20:01 to 22:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_22_00_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$TIME_2201_0000_CNT)) +
  stat_qq() +
  labs(title="22:01 to 00:00 Injuries",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

LK_TOT_QQ <- ggplot(Leoka1416, aes(sample=Leoka1416$LEOKA_FELONY_KILLED)) +
  stat_qq() +
  labs(title="Officer Felony Deaths",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

set.seed(12345)

# randomize

RanRowB <- runif(nrow(Leoka1416))

Leoka1416Ran <- Leoka1416[order(RanRowB),]

Leoka1416Ran <- head(Leoka1416Ran,n=5000)


# Takes over an hour to run.  Just saved png file.
# ROUT_LK_COR_M <- cor_matrix <- cor(Leoka1416[c(9,10,11,12,13,14,15,16,17,18,19,20,29)],method = "kendall")


# ROUT_LK_R2_M <- (cor(Leoka1416[c(9,10,11,12,13,14,15,16,17,18,19,20,29)],method = "kendall"))^2

# ROUT_LK_CIR <- corrplot(cor_matrix, method = "circle")

range(NIBRTOTAL$Arrest_Total)

Wbin=200
ROUT_TOT_HIST <- ggplot(NIBRTOTAL, aes(x = NIBRTOTAL$Arrest_Total)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Total Arrests",
       x ="Arrests Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(NIBRTOTAL$Arrest_Total),
                            sd = sd(NIBRTOTAL$Arrest_Total)))


range(NIBRTOTAL$Caucasion)

Wbin=200
ROUT_CAUC_HIST <- ggplot(NIBRTOTAL, aes(x = NIBRTOTAL$Caucasion)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Caucasion Arrests",
       x ="Arrests Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(NIBRTOTAL$Caucasion),
                            sd = sd(NIBRTOTAL$Caucasion)))

range(NIBRTOTAL$Black)

Wbin=100
ROUT_BLACK_HIST <- ggplot(NIBRTOTAL, aes(x = NIBRTOTAL$Black)) +
  geom_histogram(binwidth = Wbin, aes(y = ..density..), alpha = 0.5) +
  labs(title="Black/African American Arrests",
       x ="Arrests Per Day", y = "Density") +
  stat_function(fun = dnorm, colour = "red",
                args = list(mean = mean(NIBRTOTAL$Black),
                            sd = sd(NIBRTOTAL$Black)))


round(stat.desc(NIBRTOTAL,basic = FALSE, norm = TRUE), digits = 3)

# The data is not normal as there is significant skew to the right and left for most 
# variables and the p values are well below .05.

ROUT_TOT_PP <- ggplot(NIBRTOTAL, aes(sample=NIBRTOTAL$Arrest_Total)) +
  stat_qq() +
  labs(title="Total Arrests",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

ROUT_UNDOC_PP <- ggplot(NIBRTOTAL, aes(sample=NIBRTOTAL$Undocumented)) +
  stat_qq() +
  labs(title="Undocumented Hispanic Arrests",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")


ROUT_CAUC_PP <- ggplot(NIBRTOTAL, aes(sample=NIBRTOTAL$Caucasion)) +
  stat_qq() +
  labs(title="Caucasion Arrests",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")


ROUT_BLACK_PP <- ggplot(NIBRTOTAL, aes(sample=NIBRTOTAL$Black)) +
  stat_qq() +
  labs(title="Black/African American Arrests",
       x ="Theoretical", y = "Sample") +
  stat_qq_line(col = "red")

str(NIBRTOTAL)

ROUT_COR_M <- cor_matrix <- cor(NIBRTOTAL[2:13],method = "spearman")


ROUT_R2_M <- (cor(NIBRTOTAL[2:13],method = "spearman"))^2

ROUT_CIR <- corrplot(cor_matrix, method = "circle")


# res <- glmulti::glmulti(gender ~ threat_level + armed + signs_of_mental_illness + flee + age + race, data=FatPShoot1416, family=binomial,  fitfunction="glm", crit="aicc", confsetsize=128)
# print(res)

modelFP <- glm(gender ~ 1 + threat_level + age + threat_level:age + signs_of_mental_illness:age + flee:age + race:age, family = binomial, data = FatPShoot1416)
modelFP

summary(modelFP)

PREDICTIONSPROB <- predict.glm(modelFP,FatPShoot1416,type = "response")
PREDICTIONS <- rep(FALSE,NROW(FatPShoot1416))
PREDICTIONS[PREDICTIONSPROB > 0.5] <- "M"

ACTUAL <- FatPShoot1416$gender

cm <- as.matrix(table(PREDICTIONS,ACTUAL))
Accuracy <- sum(diag(cm))/sum(cm)*100

set.seed(88)
NR <- nrow(FatPShoot1416)


SHUFFLED <- FatPShoot1416[sample(NR),]
TRAIN_IND <- 1:round(.8 * NR)
TEST_IND <- (round(.8 * NR) + 1):NR

TRAIN <- SHUFFLED[TRAIN_IND,]
TEST <- SHUFFLED[TEST_IND,]

TRAIN_MODEL <- glm(gender ~ 1 + threat_level + age + threat_level:age + signs_of_mental_illness:age + flee:age + race:age, family = binomial, data = FatPShoot1416)

TEST_PREDICTIONS_PROB <- predict.glm(TRAIN_MODEL,TEST, type = "response")
TEST_PREDICTIONS <- rep("F",NROW(TEST))
# .5 is the usual default 
TEST_PREDICTIONS[TEST_PREDICTIONS_PROB > 0.5] <- "M"
TEST_PREDICTIONS <- as.factor(TEST_PREDICTIONS)

TESTACTUAL <- TEST$gender

cm2 <- as.matrix(table(TEST_PREDICTIONS,TESTACTUAL))
TEST_Accuracy <- sum(diag(cm2))/sum(cm2)*100

result <- confusionMatrix(TEST_PREDICTIONS, TESTACTUAL, mode="prec_recall")
result

pred <- prediction(TEST_PREDICTIONS_PROB, TESTACTUAL)
eval <- performance(pred,"acc")
# plot(eval)
roc <- performance(pred, "tpr", "fpr")
plot(roc, 
     colorize=T,
     main = "ROC Curve",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0, b=1)


auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc,4)
legend(.6, .3, auc, title = "AUC")
