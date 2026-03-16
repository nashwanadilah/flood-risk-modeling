# Install packages yang belum ter-install
install.packages("brant")
install.packages("sure")

# Impor packages
library(MASS)
library(ordinal)
library(brant)
library(car)
library(brglm2)
library(sure)
library(ggplot2)

# Impor data
studlife <- student_lifestyle_dataset

# Cek data
View(studlife)
dim(studlife)
str(studlife)
table(studlife$Stress_Level)
sum(is.na(studlife))

# Jadikan variabel respons sebagai ordinal
studlife$Stress_Level2 <-0
studlife$Stress_Level2[studlife$Stress_Level=="Moderate"] <- 1
studlife$Stress_Level2[studlife$Stress_Level=="High"] <- 2
studlife$Stress_Level2 <-factor(studlife$Stress_Level2, ordered = TRUE)

# Buat model lengkap
library(VGAM)

model <- vglm(Stress_Level2 ~ Study_Hours_Per_Day + Extracurricular_Hours_Per_Day +Sleep_Hours_Per_Day 
              + Social_Hours_Per_Day + Physical_Activity_Hours_Per_Day + GPA,
              family = cumulative(parallel = TRUE, link = "logit"),data = studlife)

# Uji asumsi multikolinearitas
mk1 <- lm(as.numeric(Stress_Level2) ~ Study_Hours_Per_Day + Extracurricular_Hours_Per_Day +
                Sleep_Hours_Per_Day + Social_Hours_Per_Day + Physical_Activity_Hours_Per_Day + GPA,data = studlife)
car::vif(mk1)
alias(mk1)

mk2 <-lm(as.numeric(Stress_Level2) ~ Study_Hours_Per_Day + Extracurricular_Hours_Per_Day +
           Sleep_Hours_Per_Day + Social_Hours_Per_Day + GPA,data = studlife)
car::vif(mk2)

# Uji Proportional Odds Model
po <- clm(Stress_Level2 ~  Extracurricular_Hours_Per_Day + 
            Sleep_Hours_Per_Day + Social_Hours_Per_Day + Physical_Activity_Hours_Per_Day + GPA, 
          family = cumulative(parallel = TRUE), data = studlife)
nominal_test(po)

m_study <- clm(Stress_Level2 ~ Study_Hours_Per_Day, data = studlife)
nominal_test(m_study)

#pake uji brant
library(brant)
m1 <- polr(Stress_Level2 ~ Physical_Activity_Hours_Per_Day, data = studlife, method = "logistic")
brant(m1)
m2 <- polr(Stress_Level2 ~ Study_Hours_Per_Day, data = studlife, method = "logistic")
brant(m2)
m3 <- polr(Stress_Level2 ~ Extracurricular_Hours_Per_Day, data = studlife, method = "logistic")
brant(m3)

model_polr <- polr(Stress_Level2 ~ Study_Hours_Per_Day + Extracurricular_Hours_Per_Day +
                     Sleep_Hours_Per_Day + Social_Hours_Per_Day + GPA,
                   data = studlife, method = "logistic")
brant(model_polr)

# Cek outlier
var <- c("Study_Hours_Per_Day","Extracurricular_Hours_Per_Day",
          "Sleep_Hours_Per_Day","Social_Hours_Per_Day",
          "Physical_Activity_Hours_Per_Day","GPA")

par(mfrow=c(2,3))
for (v in var) {
  boxplot(studlife[[v]], main = v)}

