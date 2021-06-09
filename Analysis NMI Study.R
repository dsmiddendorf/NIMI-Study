##########################################################
# Import data and libraries
##########################################################

# Downloading required libraries for analysis if they have not been downloaded yet
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "https://personality-project.org/r/", type="source")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(jtools)) install.packages("jtools", repos = "http://cran.us.r-project.org")
if(!require(interactions)) install.packages("interactions", repos = "http://cran.us.r-project.org")
if(!require(lm.beta)) install.packages("lm.beta", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(rstatix)) install.packages("rstatix", repos = "http://cran.us.r-project.org")


# Loading libraries used for the following analysis
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)
library(psych)
library(ggthemes)
library(cowplot)
library(corrplot)
library(glmnet)
library(knitr)
library(dplyr)
library(jtools)
library(interactions)
library(lm.beta)
library(readxl)
library(rstatix)

# Loading data 
nimi <- read_excel("C:/Users/Daniel/iCloudDrive/01. College/Honours Bachelor Thesis/Analysis/Analysis Data Daniel.xlsx")

# Make gender/used bci before/ used MI before column numeric (Male/Used MI,BCI before = 1)
nimi$Gender <- ifelse(nimi$Gender == "Male", 1, 0)
nimi$`Used MI before` <- ifelse(nimi$`Used MI before` == "Yes", 1, 0)
nimi$`Used BCI before` <- ifelse(nimi$`Used BCI before` == "Yes", 1, 0)


##########################################################
# Exploratory Data Analysis
##########################################################

# Create a function that computes the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Tables of all the descriptive data regarding the sample
df <- data.frame(Characteristic = c("Gender (Mean)", "Age (Mode)", "Language (Mode)", "Education (Mode)", "Used MI Before (Mean)", "Used BCI Before (Mean)"),
                 Value = c(round(mean(nimi$Gender),2), getmode(nimi$Age), getmode(nimi$Language), getmode(nimi$Education), round(mean(nimi$`Used MI before`),2), round(mean(nimi$`Used BCI before`),2)))

# Table with descriptive variables
descriptives <- kable(df)


# Table Age, Language, Education
table(nimi$Age)
table(nimi$Language)
table(nimi$Education)

# Change Lables Education
nimi$Education[nimi$Education == "Other"] <- "O"
nimi$Education[nimi$Education == "(Technical) Secondary School Diploma"] <- "TSD"
nimi$Education[nimi$Education == "University Degree"] <- "UD"
nimi$Education[nimi$Education == "Doctorate Degree"] <- "DD"
nimi$Education[nimi$Education == "Secondary School"] <- "SS"

# Barplots of all the descriptive data regarding the sample
dGender <- nimi %>% ggplot(aes(Gender)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dAge <- nimi %>% ggplot(aes(Age)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dLanguage <- nimi %>% ggplot(aes(Language)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dEducation <- nimi %>% ggplot(aes(Education)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
dMIbefore <- nimi %>% ggplot(aes(`Used MI before`)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dBCIbefore <- nimi %>% ggplot(aes(`Used BCI before`)) + geom_bar() + theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Fuse all plots together in a density plot grid
density_plots <- plot_grid(dGender, dAge, dLanguage, dEducation, dMIbefore, dBCIbefore)
density_plots

# Sample Size
nrow(nimi)

# Numbers for Conditions 
CHI_V <- c(2, 4, 9, 13)
CHI_W <- c(1, 3, 10, 14)
CMC_V <- c(3, 7, 14, 16)
CMC_W <- c(4, 8, 13, 15)

##########################################################
# Modeling
##########################################################


######### Experiential Realness ##########


# Creating the Experiential Realness Dataframe
ER <- data.frame(ER_V_HI = (nimi$ER_V_01[nimi$Condition %in% CHI_V] + nimi$ER_V_02[nimi$Condition %in% CHI_V] + -1*nimi$ER_V_03[nimi$Condition %in% CHI_V])/3,
                 ER_V_MC = (nimi$ER_V_01[nimi$Condition %in% CMC_V] + nimi$ER_V_02[nimi$Condition %in% CMC_V] + -1*nimi$ER_V_03[nimi$Condition %in% CMC_V])/3, 
                 ER_W_HI = (nimi$ER_W_01[nimi$Condition %in% CHI_W] + nimi$ER_W_02[nimi$Condition %in% CHI_W] + -1*nimi$ER_W_03[nimi$Condition %in% CHI_W])/3,
                 ER_W_MC = (nimi$ER_W_01[nimi$Condition %in% CMC_W] + nimi$ER_W_02[nimi$Condition %in% CMC_W] + -1*nimi$ER_W_03[nimi$Condition %in% CMC_W])/3)

ER <- pivot_longer(ER, 'ER_V_HI':'ER_W_MC', names_to = "Condition", values_to = "ER") # Transform data into long format
ER <- na.omit(ER) # omit NAs
ER <- data.frame(ER[,2], str_split_fixed(ER$Condition, "_", 3)[,c(2,3)]) # String processing of conditions. Create column with Condition and Type
colnames(ER) <- c("ER", "Condition", "Type") # Create new column names

# Transform conditions into numbers
ER$Condition[ER$Condition == "V"] <- 1
ER$Condition[ER$Condition == "W"] <- 0
ER$Type[ER$Type == "HI"] <- 1
ER$Type[ER$Type == "MC"] <- 0
ER$Condition <- as.numeric(ER$Condition) # Character number conditions into number number conditions for Condition
ER$Type <- as.numeric(ER$Type) # Character number conditions into number number conditions for Type

# Creating the Model for ER
modelER <- lm(ER ~ Condition*Type, data = ER) # Regression Model predicting ER by Condition and Type (incl. moderator)
summary(modelER) # More information on the model
probe_interaction(modelER, pred = Condition, modx = Type) # testing the interaction term
t.test(ER$ER[ER$Condition == 1 & ER$Type == 0], ER$ER[ER$Condition == 0 & ER$Type == 0],
       alternative = "greater", paired = T, var.equal = T)

# Effect size
effER <- data.frame(video = ER$ER[ER$Condition == 1 & ER$Type == 0], written = ER$ER[ER$Condition == 0 & ER$Type == 0])
effER <- gather(effER, key = "Condition", value = "ER")
cohens_d(effER, ER ~ Condition, paired = T, var.equal = T)


# Assumptions
## Assumptions for multiple linear regression
plot(modelER) 

## Assumptions for t-test
diff_ER_V <- ER$ER[ER$Condition == 1 & ER$Type == 0] - ER$ER[ER$Condition == 0 & ER$Type == 0]
plot(density(d))
levene_test(effER, ER ~ Condition)




######### Subjective Performance ##########

# Creating the Subjective Performance Dataframe
SP <- data.frame(SP_V_HI = (nimi$SP_V_01[nimi$Condition %in% CHI_V] + nimi$SP_V_02[nimi$Condition %in% CHI_V])/2,
                 SP_V_MC = (nimi$SP_V_01[nimi$Condition %in% CMC_V] + nimi$SP_V_02[nimi$Condition %in% CMC_V])/2, 
                 SP_W_HI = (nimi$SP_W_01[nimi$Condition %in% CHI_W] + nimi$SP_W_02[nimi$Condition %in% CHI_W])/2,
                 SP_W_MC = (nimi$SP_W_01[nimi$Condition %in% CMC_W] + nimi$SP_W_02[nimi$Condition %in% CMC_W])/2)

SP <- pivot_longer(SP, 'SP_V_HI':'SP_W_MC', names_to = "Condition", values_to = "SP") # Transform data into long format
SP <- na.omit(SP) # omit NAs

SP <- data.frame(SP[,2], str_split_fixed(SP$Condition, "_", 3)[,c(2,3)]) # String processing of conditions. Create column with Condition and Type
colnames(SP) <- c("SP", "Condition", "Type") # Create new column names

# Transform conditions into numbers
SP$Condition[SP$Condition == "V"] <- 1
SP$Condition[SP$Condition == "W"] <- 0
SP$Type[SP$Type == "HI"] <- 1
SP$Type[SP$Type == "MC"] <- 0
SP$Condition <- as.numeric(SP$Condition) # Character number conditions into number number conditions for Condition
SP$Type <- as.numeric(SP$Type) # Character number conditions into number number conditions for Type

# Creating the Model for SP
modelSP <- lm(SP ~ Condition*Type, data = SP) # Regression Model predicting ER by Condition and Type (incl. moderator)
summary(modelSP) # More information on the model
probe_interaction(modelSP, pred = Condition, modx = Type) # testing the interaction term

t.test(SP$SP[SP$Condition == 1 & SP$Type == 0], SP$SP[SP$Condition == 0 & SP$Type == 0],
       alternative = "greater", paired = T, var.equal = T)

# Effect size
effSP <- data.frame(video = SP$SP[SP$Condition == 1 & SP$Type == 0], written = SP$SP[SP$Condition == 0 & SP$Type == 0])
effSP <- gather(effSP, key = "Condition", value = "SP")
cohens_d(effSP, SP ~ Condition, paired = T)


# Assumptions
## Assumption multiple regression
plot(modelSP)

## Assumptions for t-test
diff_SP_V <- SP$SP[SP$Condition == 1 & SP$Type == 0] - SP$SP[SP$Condition == 0 & SP$Type == 0]
plot(density(diff_SP_V))
levene_test(effSP, SP ~ Condition) # Testing for homogeneity of variances



######### Usability ##########

# Creating the Usability Dataframe
U <- data.frame(U_V_HI = (nimi$U_V_01[nimi$Condition %in% CHI_V] +
                  -1 * nimi$U_V_02[nimi$Condition %in% CHI_V] +
                  nimi$U_V_03[nimi$Condition %in% CHI_V] + 
                  nimi$U_V_04[nimi$Condition %in% CHI_V] + 
                  -1 *nimi$U_V_05[nimi$Condition %in% CHI_V]+ 
                  nimi$U_V_06[nimi$Condition %in% CHI_V] +
                  nimi$U_V_07[nimi$Condition %in% CHI_V])/7, 
                U_V_MC = (nimi$U_V_01[nimi$Condition %in% CMC_V] +
                  -1 * nimi$U_V_02[nimi$Condition %in% CMC_V] +
                  nimi$U_V_03[nimi$Condition %in% CMC_V] + 
                  nimi$U_V_04[nimi$Condition %in% CMC_V] + 
                  -1 *nimi$U_V_05[nimi$Condition %in% CMC_V]+ 
                  nimi$U_V_06[nimi$Condition %in% CMC_V] +
                  nimi$U_V_07[nimi$Condition %in% CMC_V])/7,
                U_W_HI = (nimi$U_W_01[nimi$Condition %in% CHI_W] +
                  -1 * nimi$U_W_02[nimi$Condition %in% CHI_W] +
                  nimi$U_W_03[nimi$Condition %in% CHI_W] + 
                  nimi$U_W_04[nimi$Condition %in% CHI_W] + 
                  -1 *nimi$U_W_05[nimi$Condition %in% CHI_W]+ 
                  nimi$U_W_06[nimi$Condition %in% CHI_W] +
                  nimi$U_W_07[nimi$Condition %in% CHI_W])/7,
                U_W_MC = (nimi$U_W_01[nimi$Condition %in% CMC_W] +
                  -1 * nimi$U_W_02[nimi$Condition %in% CMC_W] +
                  nimi$U_W_03[nimi$Condition %in% CMC_W] + 
                  nimi$U_W_04[nimi$Condition %in% CMC_W] + 
                  -1 *nimi$U_W_05[nimi$Condition %in% CMC_W]+ 
                  nimi$U_W_06[nimi$Condition %in% CMC_W] +
                  nimi$U_W_07[nimi$Condition %in% CMC_W])/7)

U <- pivot_longer(U, 'U_V_HI':'U_W_MC', names_to = "Condition", values_to = "U") # Transform data into long format
U <- na.omit(U) # omit NAs

U <- data.frame(U[,2], str_split_fixed(U$Condition, "_", 3)[,c(2,3)]) # String processing of conditions. Create column with Condition and Type
colnames(U) <- c("U", "Condition", "Type") # Create new column names

# Transform conditions into numbers
U$Condition[U$Condition == "V"] <- 1
U$Condition[U$Condition == "W"] <- 0
U$Type[U$Type == "HI"] <- 1
U$Type[U$Type == "MC"] <- 0
U$Condition <- as.numeric(U$Condition) # Character number conditions into number number conditions for Condition
U$Type <- as.numeric(U$Type) # Character number conditions into number number conditions for Type

# Creating the Model for Usabiltiy

## Analysis including moderation effect
# modelU <- lm(U ~ Condition*Type, data = U) # Regression Model predicting ER by Condition and Type (incl. moderator)
# summary(modelU) # More information on the model
# probe_interaction(modelU, pred = Condition, modx = Type) # testing the interaction term
# 
# t.test(U$U[U$Condition == 1 & U$Type == 0], U$U[U$Condition == 0 & U$Type == 0],
#        alternative = "greater", paired = T, var.equal = T)

t.test(U$U[U$Condition == 1], U$U[U$Condition == 0],
       alternative = "greater", paired = T)

# Effect size
# effU <- data.frame(video = U$U[U$Condition == 1 & U$Type == 0], written = U$U[U$Condition == 0 & U$Type == 0])
# effU <- gather(effU, key = "Condition", value = "U")
# cohens_d(effU, U ~ Condition, paired = T, var.equal = T)

effU <- data.frame(video = U$U[U$Condition == 1], written = U$U[U$Condition == 0])
effU <- gather(effU, key = "Condition", value = "U")
cohens_d(effU, U ~ Condition, paired = T)

# Assumptions
## Assumptions for Multiple Linear Regression
# plot(modelU)

## Assumptions for t-test
diff_U_V <- U$U[U$Condition == 1 & U$Type == 0] - U$U[ER$Condition == 0 & ER$Type == 0]
plot(density(diff_U_V))
levene_test(effU, U ~ Condition)
