library(nhanesA)
library(knitr)
library(tidyverse)
library(plyr)
library(survey)
library(ggplot2)
library(readxl)
library(car)
library(Rmisc)
library(ggpubr)
library(caret)
library(tableone)
library(odds.n.ends)
library(pROC)
library(Deducer)
library(broom)

#DEMORAPHIC, EXAMINATION, LAB, QUESTIONNAIRE DATA

# Research aim- We aimed to quantify the risk of diabetes and determine whether a relationship is present between physical activity (PA)levels and diabetes.

#Population- USA Non-institutionalized citizens >= 20 years old, Outcome-Diabetes,  exposure- PA, Time- 2017 - 2018

#Covariates- gender, age, race, educational level, BMI, marital status


#DOWNLOAD NHANES DATA DIRECTLY TO R

#Explore the NHANES DATA 2018
kable(head(nhanesTables('DEMO', 2018)))
kable(head(nhanesTables('EXAM', 2018)))
kable(head(nhanesTables('Q', 2018)))
#further explore NHANES 2018
kable(head(nhanesTableVars(data_group = 'DEMO', nh_table = 'DEMO_J', namesonly = FALSE)))
kable(head(nhanesTableVars(data_group = 'EXAM', nh_table = 'BMX_J', namesonly = FALSE)))
kable(head(nhanesTableVars(data_group = 'LAB', nh_table = 'GHB_J', namesonly = FALSE)))
kable(head(nhanesTableVars(data_group = 'Q', nh_table = 'PAQ_J', namesonly = FALSE)))
#Download data  2017 - 2018 demographic data (id, age, gender, educational level, race)
demo_2018<-nhanes('DEMO_J')
names(demo_2018)
demo1<- demo_2018[c("SEQN", "RIAGENDR", "RIDAGEYR", "DMDMARTL", "DMDEDUC2",  "RIDRETH3")]

demo_vars<- names(demo1)
demo_2<-nhanesTranslate('DEMO_J', demo_vars, data = demo1)

kable(head(demo_2, n=5))
#Download data diabetes data (id, hba1c)
#labs-hbaic
db_lab_2018<- nhanes('GHB_J')
names(db_lab_2018)
hbaic_lab<- db_lab_2018[c("SEQN","LBXGH")]

hbaic_lab_vars<- names(hbaic_lab)
hbaic_lab_2<-nhanesTranslate('GHB_J', hbaic_lab_vars, data = hbaic_lab)

kable(head(hbaic_lab_2, n=5))
#Download data PA
db_PA_2018<-nhanes('PAQ_J')
names(db_PA_2018)
db_PA_qs<- db_PA_2018[c("SEQN","PAD615","PAD630", "PAD645", "PAD660", "PAD675","PAQ610", "PAQ625", "PAQ640", "PAQ655", "PAQ670")]

db_PA_qs_vars<- names(db_PA_qs)
have_PA_qs_2<-nhanesTranslate('PAQ_J', db_PA_qs_vars, data = db_PA_qs)

kable(head(have_PA_qs_2))
# multiply the METs by the CDC recommended factors
have_PA_qs_2$PAQ610_score <- have_PA_qs_2$PAQ610 *8
have_PA_qs_2$PAQ625_score <- have_PA_qs_2$PAQ625 *4
have_PA_qs_2$PAQ640_score <- have_PA_qs_2$PAQ640 *4
have_PA_qs_2$PAQ655_score <- have_PA_qs_2$PAQ655 *8
have_PA_qs_2$PAQ670_score <- have_PA_qs_2$PAQ670 *8


#PA duration in minute per day
have_PA_qs_2$minutes<-rowSums(cbind(have_PA_qs_2$PAQ610_score , have_PA_qs_2$PAQ625_score ,have_PA_qs_2$PAQ640_score , have_PA_qs_2$PAQ655_score , have_PA_qs_2$PAQ670_score), na.rm=TRUE)
#PA duration in days per week
have_PA_qs_2$days<-rowSums(cbind(have_PA_qs_2$PAD615 , have_PA_qs_2$PAD630 ,have_PA_qs_2$PAD645 , have_PA_qs_2$PAD660 , have_PA_qs_2$PAD675), na.rm=TRUE)

have_PA_qs_2<-have_PA_qs_2 %>%
  mutate(met_1 = PAQ610_score * PAD615,
         met_2 = PAQ625_score * PAD630,
         met_3 = PAQ640_score * PAD645,
         met_4 = PAQ655_score * PAD660,
         met_5 = PAQ670_score * PAD675)
#cbind- merge PA variables importanat for our analysis
have_PA_qs_2$combined_MET_Scores<- rowSums(cbind(have_PA_qs_2$met_1 , have_PA_qs_2$met_2 ,have_PA_qs_2$met_3 , have_PA_qs_2$met_4 , have_PA_qs_2$met_5), na.rm=TRUE)
have_PA_qs_2$combined_MET_Scores_hrs <- have_PA_qs_2$combined_MET_Scores /60

head(have_PA_qs_2) 
PA_Level_MET_Score <- have_PA_qs_2 %>%
  dplyr::select("SEQN", "combined_MET_Scores_hrs")
kable(head(PA_Level_MET_Score))
#Download data BMI
db_BMI_2018<-nhanes('BMX_J')
db_BMI_vars<-names(db_BMI_2018)
db_BMI_2018_qs<- db_BMI_2018[c("SEQN","BMXHT","BMXWT")]
db_BMI_2018_2<-nhanesTranslate('BMX_J', db_BMI_vars, data = db_BMI_2018_qs)

kable(head(db_BMI_2018_2))
#merge all datasets

stat_project_analytic_data<- join_all(list(demo_2, hbaic_lab_2, PA_Level_MET_Score, db_BMI_2018_2), by = "SEQN", type = "full")

kable(head(stat_project_analytic_data))
dim(stat_project_analytic_data)

#Data cleaning

#filter age 20-80
stat_project_analytic_data_cleaned <-stat_project_analytic_data %>%
  dplyr::filter(RIDAGEYR >= 20 & RIDAGEYR <= 80)




#mutate to rename variables and create BMI variable

stat_project_analytic_data_cleaned <-stat_project_analytic_data %>%
  dplyr::filter(RIDAGEYR >= 20 & RIDAGEYR<= 80)%>%
  mutate(DMDMARTL = recode_factor(DMDMARTL, "Married" = "Married",
                                  "Widowed" = "Other",
                                  "Refused" = "NA",
                                  "Divorced" = "Other",
                                  "Separated"= "Other",
                                  "Living with partner" = "Other",
                                  "Never married" = "Unmarried"),
         DMDEDUC2 = recode_factor(DMDEDUC2, "Don't Know" = "NA",
                                  "Refused" = "NA",
                                  "9-11th grade (Includes 12th grad" = "9-11th grade ",
                                  "High school graduate/GED or equi" = "College graduate or higher",
                                  "Some college or AA degree"= "College graduate or higher",
                                  "College graduate or above" = "College graduate or higher"),
         
         BMXHT = BMXHT/100,
         BMI = BMXWT/BMXHT^2,
         age.cat = cut(x = RIDAGEYR,
                       breaks = c(20, 44, 64, 79, Inf),
                       labels = c("20-44", "45-64", "65-79", ">=80")),
         BMI.cat = cut(x = BMI,
                       breaks = c(0, 18.5, 25, 30, +Inf),
                       labels = c("<18.5", "18.5-24.9", "25-29.9", ">=30"))
         
  )

#Create hbaic category a
stat_project_analytic_data_cleaned$LBXGH<- ifelse(stat_project_analytic_data_cleaned$LBXGH < 6.5, 0, 1)
stat_project_analytic_data_cleaned$LBXGH<- as.factor(stat_project_analytic_data_cleaned$LBXGH)

#create diabetes status category
stat_project_analytic_data_cleaned$db_status[stat_project_analytic_data_cleaned$LBXGH == "0"]<- "Non_diabetic"
stat_project_analytic_data_cleaned$db_status[stat_project_analytic_data_cleaned$LBXGH == "1"]<- "Diabetic"
stat_project_analytic_data_cleaned$db_status<-as.factor(stat_project_analytic_data_cleaned$db_status)

# EDA
table(stat_project_analytic_data_cleaned$RIAGENDR)

table(stat_project_analytic_data_cleaned$age.cat)
table(stat_project_analytic_data_cleaned$DMDMARTL)
table(stat_project_analytic_data_cleaned$DMDEDUC2)
table(stat_project_analytic_data_cleaned$LBXGH)
table(stat_project_analytic_data_cleaned$BMI.cat)
table(stat_project_analytic_data_cleaned$RIDRETH3)
summary(stat_project_analytic_data_cleaned)
#EDA
par(mfrow = c(2, 3))
hist(stat_project_analytic_data_cleaned$RIDAGEYR)#AGE
hist(stat_project_analytic_data_cleaned$BMI)#BMI
plot(stat_project_analytic_data_cleaned$RIDRETH3)#RACE
plot(stat_project_analytic_data_cleaned$RIAGENDR)#Gender
plot(stat_project_analytic_data_cleaned$LBXGH)#DM status
#select diabetes status, PA levels, and covariates that have been shown to be associated with increased prevalence of diabetes (age, gender, race, bmi).We had pre-determined covariates, so we did not perform correlation analysis.
NHANES_DB_PA_DATA_ANALYSIS<- stat_project_analytic_data_cleaned%>%
  dplyr::select(SEQN, RIAGENDR, age.cat, DMDMARTL, DMDEDUC2, RIDRETH3, combined_MET_Scores_hrs, BMI.cat, db_status)

kable(head(NHANES_DB_PA_DATA_ANALYSIS))
#RENAME VARIABLES
NHANES_DB_PA_DATA_ANALYSIS$Age<- NHANES_DB_PA_DATA_ANALYSIS$age.cat
NHANES_DB_PA_DATA_ANALYSIS$marital_status<- NHANES_DB_PA_DATA_ANALYSIS$DMDMARTL
NHANES_DB_PA_DATA_ANALYSIS$education<- NHANES_DB_PA_DATA_ANALYSIS$DMDEDUC2
NHANES_DB_PA_DATA_ANALYSIS$Gender<-NHANES_DB_PA_DATA_ANALYSIS$RIAGENDR
NHANES_DB_PA_DATA_ANALYSIS$race <- NHANES_DB_PA_DATA_ANALYSIS$RIDRETH3


NHANES_DB_PA_DATA_ANALYSIS_2<- NHANES_DB_PA_DATA_ANALYSIS%>%
  dplyr::select(SEQN,combined_MET_Scores_hrs, BMI.cat, db_status, Age, Gender, marital_status, education, race)
kable(head(NHANES_DB_PA_DATA_ANALYSIS_2))

#Remove NA's
complete_NHANES_DB_PA_DATA_ANALYSIS_3<-na.omit(NHANES_DB_PA_DATA_ANALYSIS_2)
#subset
#remove rows where column matital status & education is equal to "NA"
complete_NHANES_DB_PA_DATA_ANALYSIS_3 <- subset(complete_NHANES_DB_PA_DATA_ANALYSIS_3, complete_NHANES_DB_PA_DATA_ANALYSIS_3$education != "NA")
complete_NHANES_DB_PA_DATA_ANALYSIS_3 <- subset(complete_NHANES_DB_PA_DATA_ANALYSIS_3, complete_NHANES_DB_PA_DATA_ANALYSIS_3$marital_status != "NA")

summary(complete_NHANES_DB_PA_DATA_ANALYSIS_3)

dim(complete_NHANES_DB_PA_DATA_ANALYSIS_3)
kable(head(complete_NHANES_DB_PA_DATA_ANALYSIS_3))
#write data as a table
write.table(complete_NHANES_DB_PA_DATA_ANALYSIS_3, file = 'complete_NHANES_DB_PA_DATA_ANALYSIS_33.txt', quote = FALSE,sep='\t', col.names = NA, row.names = TRUE)
#Load cleaned data
NHANES_COMPLETE<- read_excel("complete_NHANES_DB_PA_DATA_ANALYSIS_33.xlsx")
typeof(NHANES_COMPLETE)

NHANES_COMPLETE<- as.data.frame(NHANES_COMPLETE)
#EDA
summary(NHANES_COMPLETE)
str(NHANES_COMPLETE)
#convert to  variables to factor
NHANES_COMPLETE$BMI.cat <- as.factor(NHANES_COMPLETE$BMI.cat)
NHANES_COMPLETE$db_status <- as.factor(NHANES_COMPLETE$db_status)
NHANES_COMPLETE$Age <- as.factor(NHANES_COMPLETE$Age)
NHANES_COMPLETE$Gender <- as.factor(NHANES_COMPLETE$Gender)
NHANES_COMPLETE$marital_status <- as.factor(NHANES_COMPLETE$marital_status)
NHANES_COMPLETE$race <- as.factor(NHANES_COMPLETE$race)
NHANES_COMPLETE$education <- as.factor(NHANES_COMPLETE$education)

str(NHANES_COMPLETE)

#rename variables again
NHANES_COMPLETE <-NHANES_COMPLETE%>%
  dplyr::select(combined_MET_Scores_hrs, BMI.cat, db_status, Age, Gender, marital_status, education, race)%>%
  mutate(race = recode_factor(race, "Mexican American" = "Hispanic",
                              "Other Hispanic" = "Hispanic",
                              "Other Race - Including Multi-Rac" = "Other/Multiracial",
                              "Non-Hispanic Asian" = "Asian",
                              "Non-Hispanic Black" = "Black",
                              "Non-Hispanic White" = "White"))

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

#create Mets score factor
NHANES_COMPLETE<-NHANES_COMPLETE%>%
  mutate(MET.cat = cut(x = combined_MET_Scores_hrs,
                       breaks = c(-Inf,0, 1000,  +Inf),
                       labels = c("No_physical_activity", "mod_PA_level", "High_PA_level"))
  )

summary(NHANES_COMPLETE)

# More EDA
ggplot(NHANES_COMPLETE, aes(x = reorder_size(BMI.cat)))+
  geom_bar(color = "blue", fill="blue")+
  facet_grid(~ db_status)+
  theme_minimal()+
  xlab("BMI Category")+
  scale_x_discrete( guide = guide_axis(angle = 70))
ggplot(NHANES_COMPLETE, aes(x = reorder_size(Age)))+
  geom_bar(color = "blue", fill="blue")+
  facet_grid(~ db_status)+
  theme_minimal()+
  xlab("Age Category")+
  scale_x_discrete( guide = guide_axis(angle = 70))
ggplot(NHANES_COMPLETE, aes(x = reorder_size(db_status)))+
  geom_bar(color = "blue", fill="blue")+
  theme_minimal()+
  xlab("DM Status")+
  scale_x_discrete( guide = guide_axis(angle = 70))
ggplot(NHANES_COMPLETE, aes(x = reorder_size(MET.cat)))+
  geom_bar(color = "blue", fill="blue")+
  facet_grid(~ db_status)+
  theme_minimal()+
  xlab("METscore Category")+
  scale_x_discrete( guide = guide_axis(angle = 70))
ggplot(NHANES_COMPLETE, aes(marital_status))+
  geom_bar(color = "blue", fill="blue")+
  facet_grid(~ db_status)+
  theme_minimal()+
  xlab("Marital status")+
  scale_x_discrete( guide = guide_axis(angle = 70))
ggplot(NHANES_COMPLETE, aes(x = reorder_size(race)))+
  geom_bar(color = "blue", fill="blue")+
  facet_grid(~ db_status)+
  theme_minimal()+
  xlab("Race")+
  scale_x_discrete( guide = guide_axis(angle = 70))
ggplot(NHANES_COMPLETE, aes(x = reorder_size(Gender)))+
  geom_bar(color = "blue", fill="blue")+
  facet_grid(~ db_status)+
  theme_minimal()+
  xlab("Gender")+
  scale_x_discrete( guide = guide_axis(angle = 70))


#normality assumption
##shapiro-wilks and qqplots

#shapiro
shapiro.test(NHANES_COMPLETE$combined_MET_Scores_hrs)
#qqplots
ggqqplot(NHANES_COMPLETE$combined_MET_Scores_hrs)

#other variables are categorical so normality testing was not performed. the METs scores were not normally distributed as seen by the shapiro-wilks test (p , 0.05) and qq-plots results

#Ho= there is no significant association between PA level and diabetes status
#Ha= there is a significant association between PA level and diabetes status

# chi-square for categorical variables
chisq.test(NHANES_COMPLETE$BMI.cat, NHANES_COMPLETE$db_status)
chisq.test(NHANES_COMPLETE$Age, NHANES_COMPLETE$db_status)
chisq.test(NHANES_COMPLETE$Gender, NHANES_COMPLETE$db_status)
chisq.test(NHANES_COMPLETE$marital_status, NHANES_COMPLETE$db_status)
chisq.test(NHANES_COMPLETE$education, NHANES_COMPLETE$db_status)
chisq.test(NHANES_COMPLETE$race, NHANES_COMPLETE$db_status)
chisq.test(NHANES_COMPLETE$MET.cat, NHANES_COMPLETE$db_status)
#T-test
t.test(  NHANES_COMPLETE$combined_MET_Scores_hrs ~  NHANES_COMPLETE$db_status)
#create table one. Categorical Chisquare, continous t-test
CreateTableOne(data= NHANES_COMPLETE, strata = "db_status", vars = c("Age", "Gender", "BMI.cat","marital_status", "education", "race", "MET.cat", "combined_MET_Scores_hrs" ))
#Model1- Model I only comparing diabetes to physical activity
logit1<- glm(formula= db_status~MET.cat, data= NHANES_COMPLETE, family='binomial')
summary(logit1)
odds.n.ends(logit1)
rocplot(logit1)
#Model2 comparing diabetes to physical activity, adjusted for baseline gender, age, and race
logit2<- glm(formula= db_status~MET.cat+Gender+Age+race, data = NHANES_COMPLETE, family='binomial'(link=logit))
summary(logit2)
odds.n.ends(logit2)
rocplot(logit2)
# Model3 comparing diabetes to physical activity,adjusted for baseline gender, age, race, education level, marital status, BMI
logit3<- glm(formula = db_status~ MET.cat+Gender+Age+race+marital_status+education+BMI.cat,  data= NHANES_COMPLETE, family='binomial')
summary(logit3)
odds.n.ends(logit3)
rocplot(logit3)

#females subgroup
female_NHANES_COMPLETE<- NHANES_COMPLETE%>%
  filter(Gender == "Female")

female_NHANES_COMPLETE <-female_NHANES_COMPLETE%>%
  dplyr::select( combined_MET_Scores_hrs, BMI.cat, db_status, Age, Gender, marital_status, education, race)%>%
  mutate(race = recode_factor(race, "Mexican American" = "Hispanic",
                              "Other Hispanic" = "Hispanic",
                              "Other Race - Including Multi-Rac" = "Other/Multiracial",
                              "Non-Hispanic Asian" = "Asian",
                              "Non-Hispanic Black" = "Black",
                              "Non-Hispanic White" = "White"))

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

#create Mets score factor
female_NHANES_COMPLETE<-female_NHANES_COMPLETE%>%
  mutate(MET.cat = cut(x = combined_MET_Scores_hrs,
                       breaks = c(-Inf,0, 1000,  +Inf),
                       labels = c("No_physical_activity", "mod_PA_level", "High_PA_level"))
  )


summary(female_NHANES_COMPLETE)
#Female log regression
#model1
Flogit1<- glm(formula= db_status~MET.cat, data= female_NHANES_COMPLETE, family='binomial')
summary(Flogit1)
odds.n.ends(Flogit1)
rocplot(Flogit1)
#Model2 comparing diabetes to physical activity, adjusted for baseline  age, and race
GFlogit2<- glm(formula= db_status~MET.cat+Age+race, data = female_NHANES_COMPLETE, family='binomial'(link=logit))
summary(GFlogit2)
odds.n.ends(GFlogit2)
rocplot(GFlogit2)
# Model3 comparing diabetes to physical activity,adjusted for baseline  age, race, education level, marital status, BMI
Flogit3<- glm(formula = db_status~ MET.cat+Age+race+marital_status+education+BMI.cat,  data= female_NHANES_COMPLETE, family='binomial')
summary(Flogit3)
odds.n.ends(Flogit3)
rocplot(Flogit3)
#Males subgroup

male_NHANES_COMPLETE<- NHANES_COMPLETE%>%
  filter(Gender == "Male")
male_NHANES_COMPLETE <-male_NHANES_COMPLETE%>%
  dplyr::select(combined_MET_Scores_hrs, BMI.cat, db_status, Age, Gender, marital_status, education, race)%>%
  mutate(race = recode_factor(race, "Mexican American" = "Hispanic",
                              "Other Hispanic" = "Hispanic",
                              "Other Race - Including Multi-Rac" = "Other/Multiracial",
                              "Non-Hispanic Asian" = "Asian",
                              "Non-Hispanic Black" = "Black",
                              "Non-Hispanic White" = "White"))

reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

#create Mets score factor
male_NHANES_COMPLETE<-male_NHANES_COMPLETE%>%
  mutate(MET.cat = cut(x = combined_MET_Scores_hrs,
                       breaks = c(-Inf,0, 1000,  +Inf),
                       labels = c("No_physical_activity", "mod_PA_level", "High_PA_level"))
  )


summary(male_NHANES_COMPLETE)
#Males log regression
#Model1
Mlogit1<- glm(formula= db_status~MET.cat, data= male_NHANES_COMPLETE, family='binomial')
summary(Mlogit1)
odds.n.ends(Mlogit1)
rocplot(Mlogit1)
#Model2 comparing diabetes to physical activity, adjusted for baseline gender, age, and race
GFlogit2<- glm(formula= db_status~MET.cat+Age+race, data = male_NHANES_COMPLETE, family='binomial'(link=logit))
summary(GFlogit2)

odds.n.ends(GFlogit2)
rocplot(GFlogit2)
# Model3 comparing diabetes to physical activity,adjusted for baseline gender, age, race, education level, marital status, BMI
Mlogit3<- glm(formula = db_status~ MET.cat+Age+race+marital_status+education+BMI.cat,  data= male_NHANES_COMPLETE, family='binomial')
summary(Mlogit3)
odds.n.ends(Mlogit3)
rocplot(Mlogit3)















