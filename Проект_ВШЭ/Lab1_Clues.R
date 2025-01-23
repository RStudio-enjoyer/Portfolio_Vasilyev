### Pashkov Lab1 Assignment ----------------------------------------------------

# unpack the archive files from R
unzip("ESS1e06_6.sav.zip")
unzip("ESS9e03_1.sav.zip")

### Part 1. Task 0: Loading the ESS1 -------------------------------------------

## Importing data for Part 1: ESS Wave 1
library(haven)
ESS1e06_6 <- read_sav("ESS1e06_6.sav")
View(ESS1e06_6)

# Working with Cntry1 and Cntry2 and call the dataset as essw1
essw1 <- subset(ESS1e06_6, cntry == "DK" | cntry == "BE")
# Remove original dataset
rm(ESS1e06_6)
gc() # collect any garbage and put to trash can

### Part 1. Task 1

#download necessary libraries
library(car)
library(descr)
library(labelled)

## Variables to find:
# Gender of the respondent: gndr
# Age of the respondent: agea
# Respondent’s education: edulvla
# Employment status: empl
# Category of financial situation: hincfel
# Place of residence: domicil

##Gender:---------------------------------------------------------------------------
summary(essw1$gndr)
table(essw1$gndr)
val_labels(essw1$gndr)
essw1$gndr_f <- factor(essw1$gndr, levels = c(1,2), labels = c("Male", "Female"))

summary(essw1$gndr_f)
prop.table(table(essw1$gndr_f))*100

#Gender barplot
barplot(prop.table(table(essw1$gndr_f))*100, main = "Respondent's gender", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2))

#crosstab two countries
crosstab(essw1$gndr_f, essw1$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE)

#Plotting
par(mfrow = c(2,1))
barplot(prop.table(table(essw1$gndr_f[essw1$cntry == "DK"]))*100, main = "Respondent's gender",
        ylab = "Percentage (%)", sub = "DK", col = terrain.colors(2))
barplot(prop.table(table(essw1$gndr_f[essw1$cntry == "BE"]))*100, main = "Respondent's gender",
        ylab = "Percentage (%)", sub = "BE", col = terrain.colors(2))

#reset plots
dev.off()

##Age:---------------------------------------------------------------------------

summary(essw1$agea)
essw1$agea_w <- essw1$agea

#sd
summary(essw1$agea_w)
sd(essw1$agea_w, na.rm = TRUE)

#creating histogramm
hist(essw1$agea_w, main = "Respondent's Age", sub = "General sample (2 countries)",
     xlab = "age (years)", col = topo.colors(20), breaks = "Scott")

#compare two countries
summary(essw1$agea_w[essw1$cntry == "DK"])
summary(essw1$agea_w[essw1$cntry == "BE"])

#plotting
par(mfrow = c(2,1))
hist(essw1$agea_w[essw1$cntry == "DK"], main = "Respondent's Age",
     sub = "DK", xlab = "age (years)",
     col = topo.colors(20), breaks = "Scott")
hist(essw1$agea_w[essw1$cntry == "BE"], main = "Respondent's Age",
     sub = "BE", xlab = "age (years)",
     col = topo.colors(20), breaks = "Scott")

#reset plots
dev.off()

#Boxplot
boxplot(essw1$agea_w ~ essw1$cntry, names = c("DK", "BE"), col = topo.colors(2),
        xlab = "Countries", ylab = "Age (years)")

#reset plots
dev.off()

##Employee:---------------------------------------------------------------------------
summary(essw1$empl)
table(essw1$empl)
val_labels(essw1$empl)

#recode for NA and making factor
essw1$empl_factor <- recode(essw1$empl, "NA = 4")
essw1$empl_factor <- factor(essw1$empl_factor, levels = c(1,2,3,4), labels = c("employed", "self-employed", "not in paid work", "not mentioned"))
summary(essw1$empl_factor)
freq(essw1$empl_factor, plot = FALSE)

#barplot
barplot(prop.table(table(essw1$empl_factor))*100, main = "Respondent's Employment Status", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

#crosstab
crosstab(essw1$empl_factor, essw1$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

#plotting
par(mfrow = c(2,1))
barplot(prop.table(table(essw1$empl_factor[essw1$cntry == "DK"]))*100, main = "Respondent's Employment Status",
        ylab = "Percentage (%)", sub = "DK", col = terrain.colors(4))
barplot(prop.table(table(essw1$empl_factor[essw1$cntry == "BE"]))*100, main = "Respondent's Employment Status",
        ylab = "Percentage (%)", sub = "BE", col = terrain.colors(4))

#reset plots
dev.off()

#Financial situation:---------------------------------------------------------------------------
summary(essw1$hincfel)

#recode for NA and making factor
essw1$hincfel_factor <- recode(essw1$hincfel, "NA = 5")
essw1$hincfel_factor <- factor(essw1$hincfel_factor, levels = c(1,2,3,4,5),
                               labels = c("Living comfortably", "Coping", "Difficult", "Very difficult", "No answer"))
summary(essw1$hincfel_factor)
freq(essw1$hincfel_factor, plot = FALSE)

#barplot
barplot(prop.table(table(essw1$hincfel_factor))*100, main = "Respondent's income", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

#crosstab two countries
crosstab(essw1$hincfel_factor, essw1$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

#barplot
par(mfrow = c(2,1))
barplot(prop.table(table(essw1$hincfel_factor[essw1$cntry == "DK"]))*100, main = "Respondent's income",
        ylab = "Percentage (%)", sub = "DK", col = terrain.colors(4))
barplot(prop.table(table(essw1$hincfel_factor[essw1$cntry == "BE"]))*100, main = "Respondent's income",
        ylab = "Percentage (%)", sub = "BE", col = terrain.colors(4))

#Education:---------------------------------------------------------------------------
summary(essw1$edulvla)
table(essw1$edulvla)
val_labels(essw1$edulvla)

#recode for NA and making factor
essw1$edulvla <- recode(essw1$edulvla, "NA = 9")
essw1$edulvla_factor <- factor(essw1$edulvla, levels = c(0,1, 2, 3, 4, 5, 55, 9),
                               labels = c("Not possible to harmonise into 5-level ISCED", 
                                          "ISCED 0-1", "ISCED 2", "ISCED 3", "ISCED 4",
                                          "ISCED 5-6", "Other", "No answer"))
summary(essw1$edulvla_factor)
freq(essw1$edulvla_factor)

#barplot
barplot(prop.table(table(essw1$edulvla_factor))*100, main = "Respondent's education", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))
par(mfrow = c(2,1))
barplot(prop.table(table(essw1$edulvla_factor[essw1$cntry == "DK"]))*100, main = "Respondent's education",
        ylab = "Percentage (%)", sub = "DK", col = terrain.colors(2))
barplot(prop.table(table(essw1$edulvla_factor[essw1$cntry == "BE"]))*100, main = "Respondent's education",
        ylab = "Percentage (%)", sub = "BE", col = terrain.colors(2))

#two countries
crosstab(essw1$edulvla_factor, essw1$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

#Residence:---------------------------------------------------------------------------
summary(essw1$domicil)
table(essw1$domicil)
val_labels(essw1$domicil)

#recode for NA and making factor
essw1$domicil<- recode(essw1$domicil, "NA = 9")
essw1$domicil_factor <- factor(essw1$domicil, levels = c(1, 2, 3, 4,5, 9),
                               labels = c("Big city", "Suburbs or outskirts",
                                          "Small City", "Country village", "Countryside", "No answer"))
summary(essw1$domicil_factor)
freq(essw1$domicil_factor)

#barplot
barplot(prop.table(table(essw1$domicil_factor))*100, main = "Respondent's living", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))
par(mfrow = c(2,1))
barplot(prop.table(table(essw1$domicil_factor[essw1$cntry == "DK"]))*100, main = "Respondent's living",
        ylab = "Percentage (%)", sub = "DK", col = terrain.colors(2))
barplot(prop.table(table(essw1$domicil_factor[essw1$cntry == "BE"]))*100, main = "Respondent's living",
        ylab = "Percentage (%)", sub = "BE", col = terrain.colors(2))

#crosstab 2 countries
crosstab(essw1$domicil_factor, essw1$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

### Part 1. Task 2: ------------------------------------------------------------

# two 5-scaled Likert
essw1$ctbfsmv_factor <- factor(essw1$ctbfsmv, levels = c(1,2,3,4,5), labels = c("Agree strongly","Agree", "Neither agree nor disagree", "Disagree", "Disagree strongly"), ordered = TRUE)
essw1$imrsprc_factor <- factor(essw1$imrsprc, levels = c(1, 2, 3, 4, 5), labels = c("Agree strongly","Agree", "Neither agree nor disagree", "Disagree", "Disagree strongly"), ordered = TRUE)

# study characteristics 
summary(essw1$ctbfsmv)
summary(essw1$ctbfsmv_factor)
sd(as.numeric(essw1$ctbfsmv_factor), na.rm = TRUE)

summary(essw1$imrsprc)
summary(essw1$imrsprc_factor)
sd(as.numeric(essw1$imrsprc), na.rm = TRUE)


summary(essw1$imrsprc_factor[essw1$cntry == "DK"])
summary(essw1$imrsprc_factor[essw1$cntry == "BE"])

#four 4 nominal/continuous/Likert-scale-based
essw1$imdfetn_factor <- factor(essw1$imdfetn, levels = c(1,2,3,4), labels = c("Allow many to come and live here ","Allow some", "Allow a few ", "Allow none "), ordered = TRUE)
essw1$imsmetn_factor <- factor(essw1$imsmetn, levels = c(1,2,3,4), labels = c("Allow many to come and live here ","Allow some", "Allow a few ", "Allow none "), ordered = TRUE)
essw1$eimrcnt_factor <- factor(essw1$eimrcnt, levels = c(1,2,3,4), labels = c("Allow many to come and live here ","Allow some", "Allow a few ", "Allow none "), ordered = TRUE)
essw1$eimpcnt_factor <- factor(essw1$eimpcnt, levels = c(1,2,3,4), labels = c("Allow many to come and live here ","Allow some", "Allow a few ", "Allow none "), ordered = TRUE)

##summary of factors
summary(essw1$imdfetn_factor)
sd(essw1$imdfetn_factor, na.rm = TRUE)

summary(essw1$imsmetn_factor)
sd(essw1$imsmetn_factor, na.rm = TRUE)

summary(essw1$eimrcnt_factor)
sd(essw1$eimrcnt_factor, na.rm = TRUE)

summary(essw1$eimpcnt_factor)
sd(essw1$eimpcnt_factor, na.rm = TRUE)


#Descriptive characteristics: ctbfsmv_factor and imrsprc_factor

par(mfrow = c(2,2))
barplot(table(essw1$ctbfsmv_factor), main = "2 countries: ctbfsmv_factor",
        xlab = "ctbfsmv_factor", col = topo.colors(5))
barplot(table(essw1$imrsprc_factor), main = "2 countries: imrsprc_factor",
        xlab = "eimrcnt_factor", col = topo.colors(5))

boxplot(list(essw1$ctbfsmv_factor[essw1$cntry == "DK"], essw1$imrsprc_factor[essw1$cntry == "DK"]), main = "DK distrib",
        col = topo.colors(2))
boxplot(list(essw1$ctbfsmv_factor[essw1$cntry == "BE"], essw1$imrsprc_factor[essw1$cntry == "BE"]), main = "BE distrib",
        col = topo.colors(2))

### Part 1. Task 3: ------------------------------------------------------------

##download library
library(nortest)

## Anderson-Darling and Shapiro-Wilk tests
ad.test(as.numeric(essw1$ctbfsmv_factor)) # Anderson-Darling test for normality
shapiro.test(as.numeric(essw1$ctbfsmv_factor)) # Shapiro-Wilk test for normality

ad.test(as.numeric(essw1$imrsprc_factor))
shapiro.test(as.numeric(essw1$imrsprc_factor))

### Part 1. Task 4: ------------------------------------------------------------

##

#New dataset
var_names <- c('cntry','gndr_f', 'agea_w', 'edulvla_factor', 'empl_factor','hincfel_factor', 'domicil_factor', 'ctbfsmv', 'imrsprc', 'imsmetn', 'imdfetn','eimrcnt', 'eimpcnt')

#Create the new dataset with only the chosen variables
essw1_small <- essw1[var_names]

#Performing the tests:
#migration variable (ctbfsmv) & gender categories
#Subset data for the two groups of interest
ctbfsmv_male   <- essw1_small$ctbfsmv[essw1_small$gndr_f == "Male"  ]
ctbfsmv_female <- essw1_small$ctbfsmv[essw1_small$gndr_f == "Female"]

# Perform Wilcoxon rank-sum test
wilcox.test(ctbfsmv_male, ctbfsmv_female)


#migration variable (imrsprc) & country categories
ctbfsmv__Cntry1 <- essw1_small$imrsprc[essw1_small$cntry == "DK"]
ctbfsmv__Cntry2 <- essw1_small$imrsprc[essw1_small$cntry == "BE"]
boxplot(list(ctbfsmv__Cntry1, ctbfsmv__Cntry2), col = c("red", "blue"),
        main = "Boxplot of Responsibility to Accept Immigrants by Country",
        names = c("DK", "BE"), xlab = "Country", ylab = "Rich Countries Accepting Immigrants")

wilcox.test(ctbfsmv__Cntry1, ctbfsmv__Cntry2)

#migration variable (imsmetn) & employment status categories
# Subset data for the three groups of interest for Kruskal-Wallis test
group1_empl <- essw1_small$imsmetn[essw1_small$empl_factor == "employed"]
group2_empl <- essw1_small$imsmetn[essw1_small$empl_factor == "self-employed"]
group3_empl <- essw1_small$imsmetn[essw1_small$empl_factor == "not in paid work"]

boxplot(list(group1_empl, group2_empl, group3_empl), col = c("red", "green", "blue"),
        main = "Boxplot of Level of acceptance by employment status", names = c("employed", "self-employed", "not in paid work"),
        xlab = "Employment Status Categories", ylab = "Level of acceptance")

# The Kruskal-Wallis test is used to test to compare multiple groups
kruskal.test(list(group1_empl, group2_empl, group3_empl))


#migration variable (imdfetn) & place of residence of the respondent categories
# Subset data for the five groups of interest
group1_resid <- essw1_small$imdfetn[essw1_small$domicil_factor == "Big city"]
group2_resid <- essw1_small$imdfetn[essw1_small$domicil_factor == "Suburbs or outskirts"]
group3_resid <- essw1_small$imdfetn[essw1_small$domicil_factor == "Small City"]
group4_resid <- essw1_small$imdfetn[essw1_small$domicil_factor == "Country village"]
group5_resid <- essw1_small$imdfetn[essw1_small$domicil_factor == "Countryside"]

# The Kruskal-Wallis test is used to test to compare multiple groups
kruskal.test(list(group1_resid, group2_resid, group3_resid, group4_resid, group5_resid)) # we can use list as a box.

#age and employment
age_empl1 <- essw1_small$agea_w[essw1_small$empl_factor == "employed"]
age_empl2 <- essw1_small$agea_w[essw1_small$empl_factor == "self-employed"]
age_empl3 <- essw1_small$agea_w[essw1_small$empl_factor == "not in paid work"]

#plotting
boxplot(list(age_empl1, age_empl2, age_empl3), col = c("red", "green", "blue"),
        main = "Boxplot of age by employment status", names = c("employed", "self-employed", "not in paid work"),
        xlab = "Employment Status Categories", ylab = "Age")
# The Kruskal-Wallis test is used to test to compare multiple groups
kruskal.test(list(age_empl1, age_empl2, age_empl3))

#plot 3x1
par(mfrow = c(3,1))
boxplot(list(ctbfsmv__Cntry1, ctbfsmv__Cntry2), col = c("red", "blue"),
        main = "Boxplot of Responsibility to Accept Immigrants by Country",
        names = c("DK", "BE"), xlab = "Country", ylab = "Rich Countries Accepting Immigrants")
boxplot(list(group1_empl, group2_empl, group3_empl), col = c("red", "green", "blue"),
        main = "Boxplot of Level of acceptance by employment status", names = c("employed", "self-employed", "not in paid work"),
        xlab = "Employment Status Categories", ylab = "Level of acceptance")
boxplot(list(age_empl1, age_empl2, age_empl3), col = c("red", "green", "blue"),
        main = "Boxplot of age by employment status", names = c("employed", "self-employed", "not in paid work"),
        xlab = "Employment Status Categories", ylab = "Age")
### Part 2. Task 0: ------------------------------------------------------------

## Importing data for Part 1: ESS Wave 2
library(haven)
library(car)
ESS9e_031 <- read_sav("ESS9e03_1.sav")
View(ESS9e03_1)

# Working with Denmark and Montenegro and call the dataset as essw9

essw9 <- subset(ESS9e_031, cntry == "ME" | cntry == "FR")

# Remove original dataset
rm(ESS9e_031)

gc() # collect any garbage and put to trash can

### Part 2. Task 5: ------------------------------------------------------------


# 1)D section bthcld - Ever given birth to/fathered a child
essw9$bthcld_factor <- factor(essw9$bthcld, levels = c(1,2), labels = c('Yes',"No"), ordered = TRUE)
summary(essw9$bthcld_factor)

# 2)D section ggchld - Have any great grandchildren
essw9$ggchld_factor <-factor(essw9$ggchld, levels = c(1,2), labels = c('Yes',"No"), ordered = TRUE)
summary(essw9$ggchld_factor)

# 3)D section iagmr - Get married, ideal age. SPLIT BALLOT
essw9$iagmr <- as.numeric(ifelse(essw9$iagmr %in% c(0, 111), NA, essw9$iagmr)) #delete chr variables for better analysis 
summary(essw9$iagmr)
# 4)D section iagrtr - Retire permanently, ideal age. SPLIT BALLOT
essw9$iagrtr <- as.numeric(ifelse(essw9$iagrtr %in% c(0, 111, 222), NA, essw9$iagrtr)) #delete chr variables for better analysis 
summary(essw9$iagrtr)

# 5)D section  evmar - Are or ever been married
essw9$evmar_factor <- factor(essw9$evmar, levels = c(1,2), labels = c('Yes',"No"), ordered = TRUE)
summary(essw9$evmar_factor)

# 6)G section occinfr - Net [pay/pensions/social benefits] of people same occupation as you in country, how fair
essw9$occinfr_factor <- factor(essw9$occinfr, levels = c(-4, -3, -2, -1, 0, 1, 2, 3,4), labels = c(
  'Low, extremely unfair',"Low, very unfair", "Low, somewhat unfair", "	Low, slightly unfair",
  "Fair", "High, slightly unfair", "High, somewhat unfair", "	High, very unfair",
  "High, extremely unfair"), ordered = TRUE)
summary(essw9$occinfr_factor)

# 7)G section grspfr - Would you say your gross pay is unfairly low, fair, or unfairly high
essw9$grspfr_factor <- factor(essw9$grspfr, levels = c(-4, -3, -2, -1, 0, 1, 2, 3,4), labels = c(
  'Low, extremely unfair',"Low, very unfair", "Low, somewhat unfair", "	Low, slightly unfair",
  "Fair", "High, slightly unfair", "High, somewhat unfair", "	High, very unfair",
  "High, extremely unfair"), ordered = TRUE)
summary(essw9$grspfr_factor)

# 8)G section netifr - Your net [pay/pensions/social benefits] is unfairly low, fair, or unfairly high
essw9$netifr_factor <- factor(essw9$netifr, levels = c(-4, -3, -2, -1, 0, 1, 2, 3,4), labels = c(
  'Low, extremely unfair',"Low, very unfair", "Low, somewhat unfair", "	Low, slightly unfair",
  "Fair", "High, slightly unfair", "High, somewhat unfair", "	High, very unfair",
  "High, extremely unfair"), ordered = TRUE)
summary(essw9$netifr_factor)

# 9)G section netilet - Which letter describes your net [pay/pensions/social benefits]
essw9$netilet_factor <- factor(essw9$netilet, levels = c(1, 2, 3,4,5,6,7,8,9,10), labels = c(
  '	K - 1st category',"	S - 2nd category", "D - 3rd category", "N - 4th category",
  "G - 5th category", "T - 6th category", "L - 7th category", "Q - 8th category",
  "F - 9th category", "J - 10th category"), ordered = TRUE)
summary(essw9$netilet_factor)

# 10)evfrjob - Everyone in country fair chance get job they seek
essw9$evfrjob_factor <- factor(essw9$evfrjob, levels = c(0,1, 2, 3,4,5,6,7,8,9,10), labels = c(
  'Does not apply at all',"1", "2", "3","4", "5", "6", "7","8", "9", "Applies completely"), ordered = TRUE)
summary(essw9$evfrjob_factor)

### Part 2. Task 6: ------------------------------------------------------------

## Age of the respondent
##Number of years of education gained (F16)
##Employment status
##Influence in employment (F27 or F28)
##Number of contracted hours of work (F29)
##5 variables from Section D
##5 variables from Section G



####Age ----------------------------------------------------------------------------------------
# First, study the structure of data:
summary(essw9$agea)

# For further work -- make a "copy" of the variable, and work with that:
essw9$agea_w <- essw9$agea

#characteristics 
summary(essw9$agea_w)
sd(essw9$agea_w, na.rm = TRUE)

#Create histogram
hist(essw9$agea_w, main = "Respondent's Age", sub = "General sample (2 countries)",
     xlab = "age (years)", col = topo.colors(20), breaks = "Scott")

# Compare tendencies across countries
summary(essw9$agea_w[essw9$cntry == "FR"])
summary(essw9$agea_w[essw9$cntry == "ME"])

#Histogram of two countries
par(mfrow = c(2,1))
hist(essw9$agea_w[essw9$cntry == "FR"], main = "Respondent's Age",
     sub = "FR", xlab = "age (years)",
     col = topo.colors(20), breaks = "Scott")
hist(essw9$agea_w[essw9$cntry == "ME"], main = "Respondent's Age",
     sub = "ME", xlab = "age (years)",
     col = topo.colors(20), breaks = "Scott")

dev.off()

# Boxplot *ONLY* for the count (numeric/continuous) data:
boxplot(essw9$agea_w ~ essw9$cntry, names = c("FR", "ME"), col = topo.colors(2),
        xlab = "Countries", ylab = "Age (years)")

# Once you've finished the copy & paste operation -- reset the plotting area for newer content:
dev.off()

####Employment ----------------------------------------------------------------------
summary(essw9$emplrel)
table(essw9$emplrel)
val_labels(essw9$emplrel)

#recode for NA and making factor
essw9$empl_factor <- recode(essw9$emplrel, "NA = 4")
essw9$empl_factor <- factor(essw9$empl_factor, levels = c(1,2,3,4), labels = c("employed", "self-employed", "working for family bussiness", "not mentioned"))
summary(essw9$empl_factor)
freq(essw9$empl_factor, plot = FALSE)

#barplot
barplot(prop.table(table(essw9$empl_factor))*100, main = "Respondent's Employment Status", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

#crosstab for two countries
crosstab(essw9$empl_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

#plotting
par(mfrow = c(2,1))
barplot(prop.table(table(essw9$empl_factor[essw9$cntry == "FR"]))*100, main = "Respondent's Employment Status",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(4))
barplot(prop.table(table(essw9$empl_factor[essw9$cntry == "ME"]))*100, main = "Respondent's Employment Status",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(4))

#reset plots
dev.off()

####wkdcorga ----------------------------------------------------------------------
summary(essw9$wkdcorga)
table(essw9$wkdcorga)

summary(essw9$wkdcorga[essw9$cntry == "FR"])
summary(essw9$wkdcorga[essw9$cntry == "ME"])
#recode for NA and making factor
essw9$wkdcorga_factor <- recode(essw9$wkdcorga, "NA = 55")
essw9$wkdcorga_factor <- factor(essw9$wkdcorga_factor, levels = c(0,1,2,3,4,5,6,7,8,9,10,55),
                               labels = c("I have/had no influence", "1", "2", "3", "4", "5",
                                          "6", "7", "8", "9", "I have/had complete control", "No answer"))
summary(essw9$wkdcorga_factor)
freq(essw9$wkdcorga_factor, plot = FALSE)

#barplot
barplot(prop.table(table(essw9$wkdcorga_factor))*100, main = "Respondent's daily work organised", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))
#crosstab for two countries
crosstab(essw9$wkdcorga_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

#plotting
par(mfrow = c(2,1))
barplot(prop.table(table(essw9$wkdcorga_factor[essw9$cntry == "FR"]))*100, main = "Respondent's daily work organised",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(4))
barplot(prop.table(table(essw9$wkdcorga_factor[essw9$cntry == "ME"]))*100, main = "Respondent's daily work organised",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(4))


####eduyears ----------------------------------------------------------------------
#characteristics
summary(essw9$eduyrs)

summary(essw9$eduyrs[essw9$cntry == "FR"])
summary(essw9$eduyrs[essw9$cntry == "ME"])
#barplot for 2 countries
barplot(prop.table(table(essw9$eduyrs))*100, main = "Respondent's full years education", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

#plotting
par(mfrow = c(2,1))
barplot(prop.table(table(essw9$eduyrs[essw9$cntry == "FR"]))*100, main = "Respondent's full years education",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$eduyrs[essw9$cntry == "ME"]))*100, main = "Respondent's full years education",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))

dev.off()

boxplot(essw9$eduyrs~ essw9$cntry, names = c("FR", "ME"), col = topo.colors(2),
        xlab = "Countries", ylab = "Full years")
####wkhct ----------------------------------------------------------------------
#characteristics
summary(essw9$wkhct)

summary(essw9$wkhct[essw9$cntry == "FR"])
summary(essw9$wkhct[essw9$cntry == "ME"])
#plotting
barplot(prop.table(table(essw9$wkhct))*100, main = "Respondent's contracted hours", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 30))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$wkhct[essw9$cntry == "FR"]))*100, main = "Respondent's contracted hours",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$wkhct[essw9$cntry == "ME"]))*100, main = "Respondent's contracted hours",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))

#crosstab
crosstab(essw9$wkhct, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

#ONLY for num
boxplot(essw9$wkhct ~ essw9$cntry, names = c("FR", "ME"), col = topo.colors(2),
        xlab = "Countries", ylab = "Contracted hours")

#####Next we are plotting (barplot) and compare variables of 2 countries (crosstab)
###Section D
#bthcld ---------------------------------------------------------------------
summary(essw9$bthcld)

summary(essw9$bthcld[essw9$cntry == "FR"])
summary(essw9$bthcld[essw9$cntry == "ME"])
#plotting
barplot(prop.table(table(essw9$bthcld_factor))*100, main = "Ever given birth to/fathered a child", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 70))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$bthcld_factor[essw9$cntry == "FR"]))*100, main = "Ever given birth to/fathered a child",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$bthcld_factor[essw9$cntry == "ME"]))*100, main = "Ever given birth to/fathered a child",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))

#crosstab 2 countries
crosstab(essw9$bthcld_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

##ggchld ----------------------------------------------------------------------
summary(essw9$ggchld)

summary(essw9$ggchld[essw9$cntry == "FR"])
summary(essw9$ggchld[essw9$cntry == "ME"])
#plotting
barplot(prop.table(table(essw9$ggchld_factor))*100, main = "Have any great grandchildren", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 70))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$ggchld_factor[essw9$cntry == "FR"]))*100, main = "Have any great grandchildren",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$ggchld_factor[essw9$cntry == "ME"]))*100, main = "Have any great grandchildren",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$ggchld_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)


##iagmr ----------------------------------------------------------------------
summary(essw9$iagmr)

summary(essw9$iagmr[essw9$cntry == "FR"])
summary(essw9$iagmr[essw9$cntry == "ME"])
#plotting
barplot(prop.table(table(essw9$iagmr))*100, main = "Ideal age to get married", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$iagmr[essw9$cntry == "FR"]))*100, main = "Ideal age to get married",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$iagmr[essw9$cntry == "ME"]))*100, main = "Ideal age to get married",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))

#Only for num
boxplot(essw9$iagmr ~ essw9$cntry, names = c("FR", "ME"), col = topo.colors(2),
        xlab = "Countries", ylab = "Ideal Age to marry (years)")
##iagrtr ----------------------------------------------------------------------
summary(essw9$iagrtr)

summary(essw9$iagrtr[essw9$cntry == "FR"])
summary(essw9$iagrtr[essw9$cntry == "ME"])
#plotting
barplot(prop.table(table(essw9$iagrtr))*100, main = "Ideal age to retire permenantely", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$iagrtr[essw9$cntry == "FR"]))*100, main = "Ideal age to retire permenantely",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$iagrtr[essw9$cntry == "ME"]))*100, main = "Ideal age to retire permenantely",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))

#only for num
boxplot(essw9$iagrtr ~ essw9$cntry, names = c("FR", "ME"), col = topo.colors(2),
        xlab = "Countries", ylab = "Ideal Age to retire (years)")
##evmar ----------------------------------------------------------------------
summary(essw9$evmar)

#plotting
barplot(prop.table(table(essw9$evmar_factor))*100, main = "Are or ever been married", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 70))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$evmar_factor[essw9$cntry == "FR"]))*100, main = "Are or ever been married",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$evmar_factor[essw9$cntry == "ME"]))*100, main = "Are or ever been married",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$evmar_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

###G section ----------------------------------------------------------------------
##occinfr ----------------------------------------------------------------------

#plotting
barplot(prop.table(table(essw9$occinfr_factor))*100, main = "How fair is net of other ppl", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$occinfr_factor[essw9$cntry == "FR"]))*100, main = "How fair is net of other ppl",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$occinfr_factor[essw9$cntry == "ME"]))*100, main = "How fair is net of other ppl",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$occinfr_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

##grspfr ----------------------------------------------------------------------
#plotting
barplot(prop.table(table(essw9$grspfr_factor))*100, main = "Respodent's gross pay", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$grspfr_factor[essw9$cntry == "FR"]))*100, main = "Respodent's gross pay",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$grspfr_factor[essw9$cntry == "ME"]))*100, main = "Respodent's gross pay",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$grspfr_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

##netifr  ----------------------------------------------------------------------
#plotting
barplot(prop.table(table(essw9$netifr_factor))*100, main = "Respodent's net", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 50))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$netifr_factor[essw9$cntry == "FR"]))*100, main = "Respodent's net",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$netifr_factor[essw9$cntry == "ME"]))*100, main = "Respodent's net",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$netifr_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

##netilet ----------------------------------------------------------------------
#plotting
barplot(prop.table(table(essw9$netilet_factor))*100, main = "Which letter describes your net benefits", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 30))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$netilet_factor[essw9$cntry == "FR"]))*100, main = "Which letter describes your net benefits",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$netilet_factor[essw9$cntry == "ME"]))*100, main = "Which letter describes your net benefits",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$netilet_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)

##evfrjob ----------------------------------------------------------------------
#plotting
barplot(prop.table(table(essw9$evfrjob_factor))*100, main = "Fair chance get job they seek", ylab = "Percentage (%)",
        sub = "Sample of 2 countries", col = topo.colors(2), ylim = c(0, 25))

par(mfrow = c(2,1))
barplot(prop.table(table(essw9$evfrjob_factor[essw9$cntry == "FR"]))*100, main = "Fair chance get job they seek",
        ylab = "Percentage (%)", sub = "FR", col = terrain.colors(2))
barplot(prop.table(table(essw9$evfrjob_factor[essw9$cntry == "ME"]))*100, main = "Fair chance get job they seek",
        ylab = "Percentage (%)", sub = "ME", col = terrain.colors(2))
#crosstab
crosstab(essw9$evfrjob_factor, essw9$cntry, prop.c = TRUE, prop.r = TRUE, asresid = TRUE, plot = FALSE, missing.include = TRUE)
### Part 2. Task 7: ------------------------------------------------------------

##download library
library(corrplot)

## Variables from Task 5
Task5__list <- c("ggchld","bthcld","iagmr","iagrtr","occinfr",
                 "grspfr","evfrjob", "netifr","netilet","evmar")

##Select the variables
essw9_small <- essw9[Task5__list]

##As matrix
essw9_small <- as.matrix(essw9_small[1:10])
str(essw9_small)

#Convert all the values to numeric and make a corrplot:
essw9_small <- apply(essw9_small, 2, as.numeric)
essw9_cor <- cor(essw9_small, use = "na.or.complete", method = "pearson")
essw9_smallpv <- cor.mtest(essw9_small, conf.level = 0.95)
corrplot(essw9_cor, p.mat = essw9_smallpv$p, sig.level = 0.10, method = "pie", col = terrain.colors(10))