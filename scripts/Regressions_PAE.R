# 
# ---
# title: "PAE analysis - Regressions"
# author: "Taran Raghuram"
# date: "1/11/2022"
# output:
#   html_document: default
# pdf_document: default
# ---

##### Package and data loading

library(sjmisc)
library(tidyverse)
#library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(caret)


### Create results dataframe for all results
df_results <- data.frame(qid=character(), Title=character(),
                         Question=character(), Race_R2=numeric(),
                         Tribe_R2=numeric(), Both_R2=numeric(),
                         All_R2=numeric(), All_Acc=numeric(),
                         Race_Acc=numeric(), Tribe_Acc=numeric(),
                         Both_Acc=numeric(), survey=character(), stringsAsFactors=FALSE)

################ AF Analysis Setup #######################

setwd("/Users/Taran/Desktop/MiC data/")
df<- read.csv("minc0007-american-fabric to use.csv", header = T, na.strings = ".")

# Add race labels
df <- df %>%
  mutate(Race = 
           case_when(
             race == 1 ~ "White",
             race == 2 ~ "Black",
             race == 3 ~ "Hispanic",
             race == 4 ~ "Asian",
             race == 5 ~ "Native American",
             race == 6 ~ "Mixed",
             race == 7 ~ "Other",
             race == 8 ~ "Middle Eastern"
           ))


# Add Tribes
df <- df %>%
  mutate(Hidden_Tribes = 
           case_when(
             HT.Segements.Ordered == 6 ~ "Progressive Activists",
             HT.Segements.Ordered == 5 ~ "Traditional Liberals",
             HT.Segements.Ordered == 4 ~ "Passive Liberals",
             HT.Segements.Ordered == 3 ~ "Politically Disengaged",
             HT.Segements.Ordered == 2 ~ "Moderates",
             HT.Segements.Ordered == 1 ~ "Traditional Conservatives",
             HT.Segements.Ordered == 0 ~ "Devoted Conservatives"
           ))
df$Hidden_Tribes  <- as.factor(df$Hidden_Tribes)
df$Hidden_Tribes <- factor(df$Hidden_Tribes, levels = c("Progressive Activists", "Traditional Liberals",
                                                        "Passive Liberals", "Politically Disengaged",
                                                        "Moderates", "Traditional Conservatives", "Devoted Conservatives"))


# Add Generation
df <- df %>%
  mutate(Generation_text = 
           case_when(
             birthyr <= 1945 ~ "Silent Generation (1928-1945)",
             birthyr > 1945 & birthyr <=1964 ~ "Baby Boomers (1946-1964)",
             birthyr > 1964 & birthyr <=1980 ~ "Gen X (1965-1980)",
             birthyr > 1980 & birthyr <=1996 ~ "Millennials (1981-1996)",
             birthyr > 1996 ~ "Gen Z (1997-)"
           ))
df$Generation_text  <- as.factor(df$Generation_text)
df$Generation_text <- factor(df$Generation_text, levels = c("Silent Generation (1928-1945)","Baby Boomers (1946-1964)",
                                                            "Gen X (1965-1980)", "Millennials (1981-1996)", "Gen Z (1997-)"))


# Gender
df <- df %>%
  mutate(Gender = 
           case_when(
             gender_o == 1 ~ "Male",
             gender_o == 2 ~ "Female"
           ))


df <- df %>%
  mutate(EducationTri = 
           case_when(
             educ <= 2 ~ "HS or less",
             educ == 3 | educ == 4 ~ "Some college, not 4-year",
             educ >= 5 ~ "4-year college or more"
           ))
df$EducationTri  <- as.factor(df$EducationTri)
df$EducationTri  <- factor(df$EducationTri, levels = c("HS or less", "Some college, not 4-year", "4-year college or more"))


df <- df %>%
  mutate(ReligionTri = 
           case_when(
             religpew <= 4 ~ "Christian",
             religpew >= 5 & religpew <= 8 | religpew == 12 ~ "Other Religions",
             religpew >= 9 & religpew <= 11 ~ "Non-religious"
           ))
df$ReligionTri  <- as.factor(df$ReligionTri)
df$ReligionTri  <- factor(df$ReligionTri, levels = c("Christian", "Other Religions", "Non-religious"))




## Import questions
qsdf <- read.csv("questions - AF.csv", header = T)
qs <- as.character(qsdf$Short.name)


## Restrict analysis to White, Black, Hispanic
df <- df[df$race < 4,]


## Create dataframe needed for analysis
dfn <- data.frame(df)
options(digits=3)



######## Regressions - AF General ###########

## Create dataframe with all questions collapsed to binary values (except continuous ones)
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Create some col helper vars
  colm <- as.character(qsdf$Short.name[row])
  x <- paste(colm, "bin", sep = "_") # do this if we really need separate cols
  
  # If grouping required
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # group data into binary values
    l2 <- qsdf[row, "l2"]
    dfn[,x] = ifelse(dfn[,colm]<=l2, 1, 0)
    
  }
  
  else {
    # Make first value 1, second 0
    dfn[,x] = ifelse(dfn[,colm]==1, 1, 0)
  }
  
  ## Loop through questions to create dataframe of accuracy scores/R^2 values with tribe, race and both
  lm_model_race <- lm(dfn[,x] ~ Race, dfn)
  lm_model_tribe <- lm(dfn[,x] ~ Hidden_Tribes, dfn)
  lm_model_both <- lm(dfn[,x] ~ Race + Hidden_Tribes, dfn)
  lm_model_all <- lm(dfn[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, dfn)
  
  # Regs for each Race by Tribe only
  lm_model_tribe_white <- lm(dfn[dfn$race==1,x] ~ Hidden_Tribes, dfn[dfn$race==1,])
  lm_model_tribe_black <- lm(dfn[dfn$race==2,x] ~ Hidden_Tribes, dfn[dfn$race==2,])
  lm_model_tribe_hispanic <- lm(dfn[dfn$race==3,x] ~ Hidden_Tribes, dfn[dfn$race==3,])
  
  # Question info
  df_results[x,"qid"] = x
  df_results[x,"Question"] = as.character(qsdf$Question[row])
  df_results[x,"Title"] = as.character(qsdf$Title[row])
  df_results[x, "survey"] = "AF"
  
  # Summaries of linear models
  df_results[x,"Race_R2"] = summary(lm_model_race)$r.squared
  df_results[x,"Tribe_R2"] = summary(lm_model_tribe)$r.squared
  df_results[x,"Both_R2"] = summary(lm_model_both)$r.squared
  df_results[x,"All_R2"] = summary(lm_model_all)$r.squared
  df_results[x,"Tribe_white_R2"] = summary(lm_model_tribe_white)$r.squared
  df_results[x,"Tribe_black_R2"] = summary(lm_model_tribe_black)$r.squared
  df_results[x,"Tribe_hispanic_R2"] = summary(lm_model_tribe_hispanic)$r.squared
  
  
  ## Logit models
  # Create training sample
  smp_size <- floor(0.75 * nrow(dfn))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dfn)), size = smp_size)
  train <- dfn[train_ind, ]
  test <- dfn[-train_ind, ]
  
  # Fit logit to training data - Race and Tribe
  glm.fit_race <- glm(train[,x] ~ Race, data=train, family='binomial')
  glm.fit_tribe <- glm(train[,x] ~ Hidden_Tribes, data=train, family='binomial')
  glm.fit_both <- glm(train[,x] ~ Race + Hidden_Tribes, data=train, family='binomial')
  glm.fit_all <- glm(train[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, data=train, family='binomial')
  #summary(glm.fit)
  
  # Predict test data
  y_act <- test[,x]
  
  pred_race <- predict(glm.fit_race, newdata = test, type = "response")
  y_pred_num_race <- ifelse(pred_race > 0.5, 1, 0)
  y_pred_race <- factor(y_pred_num_race, levels=c(0, 1))
  
  pred_tribe <- predict(glm.fit_tribe, newdata = test, type = "response")
  y_pred_num_tribe <- ifelse(pred_tribe > 0.5, 1, 0)
  y_pred_tribe <- factor(y_pred_num_tribe, levels=c(0, 1))
  
  pred_both <- predict(glm.fit_both, newdata = test, type = "response")
  y_pred_num_both <- ifelse(pred_both > 0.5, 1, 0)
  y_pred_both <- factor(y_pred_num_both, levels=c(0, 1))
  
  pred_all <- predict(glm.fit_all, newdata = test, type = "response")
  y_pred_num_all <- ifelse(pred_all > 0.5, 1, 0)
  y_pred_all <- factor(y_pred_num_all, levels=c(0, 1))
  
  
  # Save results
  # df_results[x,"Race_Acc"] = mean(y_pred_race == y_act, na.rm = TRUE)
  # df_results[x,"Tribe_Acc"] = mean(y_pred_tribe == y_act, na.rm = TRUE)
  # df_results[x,"Both_Acc"] = mean(y_pred_both == y_act, na.rm = TRUE)
  # df_results[x,"All_Acc"] = mean(y_pred_all == y_act, na.rm = TRUE)
  
  # Alternative metric
  #confusionMatrix(y_pred,as.factor(y_act))
  df_results[x,"Race_Acc"] = confusionMatrix(y_pred_race,as.factor(y_act))$byClass["Balanced Accuracy"]
  df_results[x,"Tribe_Acc"] = confusionMatrix(y_pred_tribe,as.factor(y_act))$byClass["Balanced Accuracy"]
  df_results[x,"Both_Acc"] = confusionMatrix(y_pred_both,as.factor(y_act))$byClass["Balanced Accuracy"]
  df_results[x,"All_Acc"] = confusionMatrix(y_pred_all,as.factor(y_act))$byClass["Balanced Accuracy"]
  
}

## Overall means (add to loop)
mean(dfn$excep_2_bin, na.rm = TRUE)
mean(dfn$opt_nat_5_bin, na.rm = TRUE)
mean(dfn$stranger_bin, na.rm = TRUE)



write.csv(df_results,"AF_results.csv", row.names = FALSE)
AF_results <- data.frame(df_results)



######## Regressions - AF Media ###########

## Import media qs
qsdf <- read.csv("questions - AF media.csv", header = T)
qs <- as.character(qsdf$Short.name)
treatments <- c("Fox News, Wall Street Journal, and the Daily Wire", "MSNBC, CNN, and the New York Times")


## Create dataframe with all questions collapsed to binary values (except continuous ones)
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Create some col helper vars
  colm <- as.character(qsdf$Short.name[row])
  x <- paste(colm, "bin", sep = "_") # do this if we really need separate cols
  
  for (i in 1:2) {
    
    df2 <- dfn[dfn$media_treat==i,]
    y <- paste(x, "-", treatments[i])
    
    # If grouping required
    if (qsdf$Grouping.Required[row] == "Yes") {
      
      # group data into binary values
      l2 <- qsdf[row, "l2"]
      df2[,x] = ifelse(df2[,colm]<=l2, 1, 0)
      
    }
    
    else {
      # Make first value 1, second 0
      df2[,x] = ifelse(df2[,colm]==1, 1, 0)
    }
    
    ## Loop through questions to create dataframe of accuracy scores/R^2 values with tribe, race and both
    lm_model_race <- lm(df2[,x] ~ Race, df2)
    lm_model_tribe <- lm(df2[,x] ~ Hidden_Tribes, df2)
    lm_model_both <- lm(df2[,x] ~ Race + Hidden_Tribes, df2)
    lm_model_all <- lm(df2[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, df2)
    
    # Regs for each Race by Tribe only
    lm_model_tribe_white <- lm(df2[df2$race==1,x] ~ Hidden_Tribes, df2[df2$race==1,])
    lm_model_tribe_black <- lm(df2[df2$race==2,x] ~ Hidden_Tribes, df2[df2$race==2,])
    lm_model_tribe_hispanic <- lm(df2[df2$race==3,x] ~ Hidden_Tribes, df2[df2$race==3,])
    
    # Question info
    df_results[y,"qid"] = y
    df_results[y,"Question"] = paste(as.character(qsdf$Question[row]), "-", treatments[i])
    df_results[y,"Title"] = paste(as.character(qsdf$Title[row]), "-", treatments[i])
    df_results[y, "survey"] = "AF"
    
    # Summaries of linear models
    df_results[y,"Race_R2"] = summary(lm_model_race)$r.squared
    df_results[y,"Tribe_R2"] = summary(lm_model_tribe)$r.squared
    df_results[y,"Both_R2"] = summary(lm_model_both)$r.squared
    df_results[y,"All_R2"] = summary(lm_model_all)$r.squared
    df_results[y,"Tribe_white_R2"] = summary(lm_model_tribe_white)$r.squared
    df_results[y,"Tribe_black_R2"] = summary(lm_model_tribe_black)$r.squared
    df_results[y,"Tribe_hispanic_R2"] = summary(lm_model_tribe_hispanic)$r.squared
    
    
    ## Logit models
    # Create training sample
    smp_size <- floor(0.75 * nrow(df2))
    set.seed(123)
    train_ind <- sample(seq_len(nrow(df2)), size = smp_size)
    train <- df2[train_ind, ]
    test <- df2[-train_ind, ]
    
    # Fit logit to training data - Race and Tribe
    glm.fit_race <- glm(train[,x] ~ Race, data=train, family='binomial')
    glm.fit_tribe <- glm(train[,x] ~ Hidden_Tribes, data=train, family='binomial')
    glm.fit_both <- glm(train[,x] ~ Race + Hidden_Tribes, data=train, family='binomial')
    glm.fit_all <- glm(train[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, data=train, family='binomial')
    #summary(glm.fit)
    
    # Predict test data
    y_act <- test[,x]
    
    pred_race <- predict(glm.fit_race, newdata = test, type = "response")
    y_pred_num_race <- ifelse(pred_race > 0.5, 1, 0)
    y_pred_race <- factor(y_pred_num_race, levels=c(0, 1))
    
    pred_tribe <- predict(glm.fit_tribe, newdata = test, type = "response")
    y_pred_num_tribe <- ifelse(pred_tribe > 0.5, 1, 0)
    y_pred_tribe <- factor(y_pred_num_tribe, levels=c(0, 1))
    
    pred_both <- predict(glm.fit_both, newdata = test, type = "response")
    y_pred_num_both <- ifelse(pred_both > 0.5, 1, 0)
    y_pred_both <- factor(y_pred_num_both, levels=c(0, 1))
    
    pred_all <- predict(glm.fit_all, newdata = test, type = "response")
    y_pred_num_all <- ifelse(pred_all > 0.5, 1, 0)
    y_pred_all <- factor(y_pred_num_all, levels=c(0, 1))
    
    
    # Save results
    df_results[y,"Race_Acc"] = mean(y_pred_race == y_act, na.rm = TRUE)
    df_results[y,"Tribe_Acc"] = mean(y_pred_tribe == y_act, na.rm = TRUE)
    df_results[y,"Both_Acc"] = mean(y_pred_both == y_act, na.rm = TRUE)
    df_results[y,"All_Acc"] = mean(y_pred_all == y_act, na.rm = TRUE)
    #confusionMatrix(y_pred,as.factor(y_act))
  }
}

write.csv(df_results,"AF_results.csv", row.names = FALSE)



######## Regressions - AF Message Testing ###########

qsdf <- read.csv("questions - AF MT.csv", header = T)
qs <- as.character(qsdf$Short.name)

## No logit models for continuous dependent vars
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Create some col helper vars
  colm <- as.character(qsdf$Short.name[row])
  x <- paste(colm, "bin", sep = "_") # do this if we really need separate cols
  
  # If grouping required
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # group data into binary values
    l2 <- qsdf[row, "l2"]
    dfn[,x] = ifelse(dfn[,colm]<=l2, 1, 0)
    
  }
  
  else {
    # Make first value 1, second 0
    dfn[,x] = ifelse(dfn[,colm]==1, 1, 0)
  }
  
  ## Loop through questions to create dataframe of accuracy scores/R^2 values with tribe, race and both
  lm_model_race <- lm(dfn[,x] ~ Race, dfn)
  lm_model_tribe <- lm(dfn[,x] ~ Hidden_Tribes, dfn)
  lm_model_both <- lm(dfn[,x] ~ Race + Hidden_Tribes, dfn)
  lm_model_all <- lm(dfn[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, dfn)
  
  # Regs for each Race by Tribe only
  lm_model_tribe_white <- lm(dfn[dfn$race==1,x] ~ Hidden_Tribes, dfn[dfn$race==1,])
  lm_model_tribe_black <- lm(dfn[dfn$race==2,x] ~ Hidden_Tribes, dfn[dfn$race==2,])
  lm_model_tribe_hispanic <- lm(dfn[dfn$race==3,x] ~ Hidden_Tribes, dfn[dfn$race==3,])
  
  # Question info
  df_results[x,"qid"] = x
  df_results[x,"Question"] = as.character(qsdf$Question[row])
  df_results[x,"Title"] = as.character(qsdf$Title[row])
  df_results[x,"survey"] = "AF"
  
  # Summaries of linear models
  df_results[x,"Race_R2"] = summary(lm_model_race)$r.squared
  df_results[x,"Tribe_R2"] = summary(lm_model_tribe)$r.squared
  df_results[x,"Both_R2"] = summary(lm_model_both)$r.squared
  df_results[x,"All_R2"] = summary(lm_model_all)$r.squared
  df_results[x,"Tribe_white_R2"] = summary(lm_model_tribe_white)$r.squared
  df_results[x,"Tribe_black_R2"] = summary(lm_model_tribe_black)$r.squared
  df_results[x,"Tribe_hispanic_R2"] = summary(lm_model_tribe_hispanic)$r.squared
  
}

write.csv(df_results,"AF_results.csv", row.names = FALSE)
AF_results <- data.frame(df_results)



################ HT Analysis Setup #######################

setwd("/Users/Taran/Desktop/MiC data/")
df<- read.csv("Hidden Tribes Data - Levels for Sharing.csv", header = T, na.strings = ".")

# Add race labels
df <- df %>%
  mutate(Race = 
           case_when(
             race == 1 ~ "White",
             race == 2 ~ "Black",
             race == 3 ~ "Hispanic",
             race == 4 ~ "Asian",
             race == 5 ~ "Native American",
             race == 6 ~ "Mixed",
             race == 7 ~ "Other",
             race == 8 ~ "Middle Eastern"
           ))
df$Race <- as.factor(df$Race)
df$Race <- factor(df$Race, levels = c("White", "Black", "Asian", "Hispanic", "Native American", "Mixed", "Other", "Middle Eastern"))

# Add Tribes
df$Hidden_Tribes  <- as.factor(df$Segment)
df$Hidden_Tribes <- factor(df$Hidden_Tribes, levels = c("Progressive Activists", "Traditional Liberals",
                                                        "Passive Liberals", "Politically Disengaged",
                                                        "Moderates", "Traditional Conservatives", "Devoted Conservatives"))

# Add Generation
df <- df %>%
  mutate(Generation_text = 
           case_when(
             birthyr <= 1945 ~ "Silent Generation (1928-1945)",
             birthyr > 1945 & birthyr <=1964 ~ "Baby Boomers (1946-1964)",
             birthyr > 1964 & birthyr <=1980 ~ "Gen X (1965-1980)",
             birthyr > 1980 & birthyr <=1996 ~ "Millennials (1981-1996)",
             birthyr > 1996 ~ "Gen Z (1997-)"
           ))
df$Generation_text  <- as.factor(df$Generation_text)
df$Generation_text <- factor(df$Generation_text, levels = c("Silent Generation (1928-1945)","Baby Boomers (1946-1964)",
                                                            "Gen X (1965-1980)", "Millennials (1981-1996)", "Gen Z (1997-)"))


# Gender
df <- df %>%
  mutate(Gender = 
           case_when(
             gender == 1 ~ "Male",
             gender == 2 ~ "Female"
           ))


df <- df %>%
  mutate(EducationTri = 
           case_when(
             educ <= 2 ~ "HS or less",
             educ == 3 | educ == 4 ~ "Some college, not 4-year",
             educ >= 5 ~ "4-year college or more"
           ))
df$EducationTri  <- as.factor(df$EducationTri)
df$EducationTri  <- factor(df$EducationTri, levels = c("HS or less", "Some college, not 4-year", "4-year college or more"))



df <- df %>%
  mutate(ReligionTri = 
           case_when(
             religpew <= 4 ~ "Christian",
             religpew >= 5 & religpew <= 8 | religpew == 12 ~ "Other Religions",
             religpew >= 9 & religpew <= 11 ~ "Non-religious"
           ))
df$ReligionTri  <- as.factor(df$ReligionTri)
df$ReligionTri  <- factor(df$ReligionTri, levels = c("Christian", "Other Religions", "Non-religious"))


# import questions file
qsdf <- read.csv("questions - HT.csv", header = T)
qs <- as.character(qsdf$Short.name)

# Restrict to only black, white, hispaniic
df <- df[df$race < 4,]




######### HT Regressions #########

## Create dataframes needed for analysis
dfn <- data.frame(df)
df_results <- data.frame(qid=character(), Title=character(),
                         Question=character(), Race_R2=numeric(),
                         Tribe_R2=numeric(), Both_R2=numeric(),
                         All_R2=numeric(), All_Acc=numeric(),
                         Race_Acc=numeric(), Tribe_Acc=numeric(),
                         Both_Acc=numeric(), stringsAsFactors=FALSE) 
options(digits=3)

# Create dataframe with all questions collapsed to binary values (except continuous ones)
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Create some col helper vars
  colm <- as.character(qsdf$Short.name[row])
  x <- paste(colm, "bin", sep = "_") # do this if we really need separate cols
  
  # If grouping required
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # group data into binary values
    l2 <- qsdf[row, "l2"]
    dfn[,x] = ifelse(dfn[,colm]<=l2, 1, 0)
    
  }
  
  else {
    # Make first value 1, second 0
    dfn[,x] = ifelse(dfn[,colm]==1, 1, 0)
  }
  
  ## Loop through questions to create dataframe of accuracy scores/R^2 values with tribe, race and both
  lm_model_race <- lm(dfn[,x] ~ Race, dfn)
  lm_model_tribe <- lm(dfn[,x] ~ Hidden_Tribes, dfn)
  lm_model_both <- lm(dfn[,x] ~ Race + Hidden_Tribes, dfn)
  try(lm_model_all <- lm(dfn[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, dfn), silent = TRUE)
  
  # Regs for each Race by Tribe only
  lm_model_tribe_white <- lm(dfn[dfn$race==1,x] ~ Hidden_Tribes, dfn[dfn$race==1,])
  lm_model_tribe_black <- lm(dfn[dfn$race==2,x] ~ Hidden_Tribes, dfn[dfn$race==2,])
  lm_model_tribe_hispanic <- lm(dfn[dfn$race==3,x] ~ Hidden_Tribes, dfn[dfn$race==3,])
  
  # Question info
  df_results[x,"qid"] = x
  df_results[x,"Question"] = as.character(qsdf$Question[row])
  df_results[x,"Title"] = as.character(qsdf$Title[row])
  df_results[x,"survey"] = "HT"
  
  # Summaries of linear models
  df_results[x,"Race_R2"] = summary(lm_model_race)$r.squared
  df_results[x,"Tribe_R2"] = summary(lm_model_tribe)$r.squared
  df_results[x,"Both_R2"] = summary(lm_model_both)$r.squared
  try(df_results[x,"All_R2"] <- summary(lm_model_all)$r.squared, silent = TRUE)
  df_results[x,"Tribe_white_R2"] = summary(lm_model_tribe_white)$r.squared
  df_results[x,"Tribe_black_R2"] = summary(lm_model_tribe_black)$r.squared
  df_results[x,"Tribe_hispanic_R2"] = summary(lm_model_tribe_hispanic)$r.squared
  
  
  ## Logit models
  # Create training sample
  smp_size <- floor(0.75 * nrow(dfn))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dfn)), size = smp_size)
  train <- dfn[train_ind, ]
  test <- dfn[-train_ind, ]
  
  # Fit logit to training data - Race and Tribe
  glm.fit_race <- glm(train[,x] ~ Race, data=train, family='binomial')
  glm.fit_tribe <- glm(train[,x] ~ Hidden_Tribes, data=train, family='binomial')
  glm.fit_both <- glm(train[,x] ~ Race + Hidden_Tribes, data=train, family='binomial')
  try(glm.fit_all <- glm(train[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, data=train, family='binomial'), silent = TRUE)
  #summary(glm.fit)
  
  # Predict test data
  y_act <- test[,x]
  
  pred_race <- predict(glm.fit_race, newdata = test, type = "response")
  y_pred_num_race <- ifelse(pred_race > 0.5, 1, 0)
  y_pred_race <- factor(y_pred_num_race, levels=c(0, 1))
  
  pred_tribe <- predict(glm.fit_tribe, newdata = test, type = "response")
  y_pred_num_tribe <- ifelse(pred_tribe > 0.5, 1, 0)
  y_pred_tribe <- factor(y_pred_num_tribe, levels=c(0, 1))
  
  pred_both <- predict(glm.fit_both, newdata = test, type = "response")
  y_pred_num_both <- ifelse(pred_both > 0.5, 1, 0)
  y_pred_both <- factor(y_pred_num_both, levels=c(0, 1))
  
  try(pred_all <- predict(glm.fit_all, newdata = test, type = "response"), silent = TRUE)
  try(y_pred_num_all <- ifelse(pred_all > 0.5, 1, 0), silent = TRUE)
  try(y_pred_all <- factor(y_pred_num_all, levels=c(0, 1)), silent = TRUE)
  
  
  # Save results
  df_results[x,"Race_Acc"] <- mean(y_pred_race == y_act, na.rm = TRUE)
  df_results[x,"Tribe_Acc"] <- mean(y_pred_tribe == y_act, na.rm = TRUE)
  df_results[x,"Both_Acc"] <- mean(y_pred_both == y_act, na.rm = TRUE)
  try(df_results[x,"All_Acc"] <- mean(y_pred_all == y_act, na.rm = TRUE), silent = TRUE)
  #confusionMatrix(y_pred,as.factor(y_act))
}

write.csv(df_results,"HT_results.csv", row.names = FALSE)
HT_results <- data.frame(df_results)





################ EIP Analysis Setup #######################

setwd("/Users/Taran/Desktop/MiC data/")
df<- read.csv("minc0019-eip-wave-1-3  to use.csv", header = T)

# Add race labels
df <- df %>%
  mutate(Race = 
           case_when(
             race == 1 ~ "White",
             race == 2 ~ "Black",
             race == 3 ~ "Hispanic",
             race == 4 ~ "Asian",
             race == 5 ~ "Native American",
             race == 6 ~ "Mixed",
             race == 7 ~ "Other",
             race == 8 ~ "Middle Eastern"
           ))


# Add Tribes
df <- df %>%
  mutate(Hidden_Tribes = 
           case_when(
             Hidden.Tribes_Correct == 0 ~ "Progressive Activists",
             Hidden.Tribes_Correct == 1 ~ "Traditional Liberals",
             Hidden.Tribes_Correct == 2 ~ "Passive Liberals",
             Hidden.Tribes_Correct == 3 ~ "Politically Disengaged",
             Hidden.Tribes_Correct == 4 ~ "Moderates",
             Hidden.Tribes_Correct == 5 ~ "Traditional Conservatives",
             Hidden.Tribes_Correct == 6 ~ "Devoted Conservatives"
           ))
df$Hidden_Tribes  <- as.factor(df$Hidden_Tribes)
df$Hidden_Tribes <- factor(df$Hidden_Tribes, levels = c("Progressive Activists", "Traditional Liberals",
                                                        "Passive Liberals", "Politically Disengaged",
                                                        "Moderates", "Traditional Conservatives", "Devoted Conservatives"))

# Add Generation
df <- df %>%
  mutate(Generation_text = 
           case_when(
             birthyr <= 1945 ~ "Silent Generation (1928-1945)",
             birthyr > 1945 & birthyr <=1964 ~ "Baby Boomers (1946-1964)",
             birthyr > 1964 & birthyr <=1980 ~ "Gen X (1965-1980)",
             birthyr > 1980 & birthyr <=1996 ~ "Millennials (1981-1996)",
             birthyr > 1996 ~ "Gen Z (1997-)"
           ))
df$Generation_text  <- as.factor(df$Generation_text)
df$Generation_text <- factor(df$Generation_text, levels = c("Silent Generation (1928-1945)","Baby Boomers (1946-1964)",
                                                            "Gen X (1965-1980)", "Millennials (1981-1996)", "Gen Z (1997-)"))


# Gender
df <- df %>%
  mutate(Gender = 
           case_when(
             gender == 1 ~ "Male",
             gender == 2 ~ "Female"
           ))


df <- df %>%
  mutate(EducationTri = 
           case_when(
             educ <= 2 ~ "HS or less",
             educ == 3 | educ == 4 ~ "Some college, not 4-year",
             educ >= 5 ~ "4-year college or more"
           ))
df$EducationTri  <- as.factor(df$EducationTri)
df$EducationTri  <- factor(df$EducationTri, levels = c("HS or less", "Some college, not 4-year", "4-year college or more"))


df <- df %>%
  mutate(ReligionTri = 
           case_when(
             religpew <= 4 ~ "Christian",
             religpew >= 5 & religpew <= 8 | religpew == 12 ~ "Other Religions",
             religpew >= 9 & religpew <= 11 ~ "Non-religious"
           ))
df$ReligionTri  <- as.factor(df$ReligionTri)
df$ReligionTri  <- factor(df$ReligionTri, levels = c("Christian", "Other Religions", "Non-religious"))




# Import questions file
qsdf <- read.csv("questions - EIP.csv", header = T)
qs <- as.character(qsdf$Short.name)

# restrict analysis to only black, white, hispanic
df <- df[df$race < 4,]


######### EIP Regressions #########

## Create dataframes needed for analysis
dfn <- data.frame(df)
df_results <- data.frame(qid=character(), Title=character(),
                         Question=character(), Race_R2=numeric(),
                         Tribe_R2=numeric(), Both_R2=numeric(),
                         All_R2=numeric(), All_Acc=numeric(),
                         Race_Acc=numeric(), Tribe_Acc=numeric(),
                         Both_Acc=numeric(), stringsAsFactors=FALSE) 
options(digits=3)

# Create dataframe with all questions collapsed to binary values (except continuous ones)
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Create some col helper vars
  colm <- as.character(qsdf$Short.name[row])
  x <- paste(colm, "bin", sep = "_") # do this if we really need separate cols
  
  # If grouping required
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # group data into binary values
    l2 <- qsdf[row, "l2"]
    dfn[,x] = ifelse(dfn[,colm]<=l2, 1, 0)
    
  }
  
  else {
    # Make first value 1, second 0
    dfn[,x] = ifelse(dfn[,colm]==1, 1, 0)
  }
  
  ## Loop through questions to create dataframe of accuracy scores/R^2 values with tribe, race and both
  lm_model_race <- lm(dfn[,x] ~ Race, dfn)
  lm_model_tribe <- lm(dfn[,x] ~ Hidden_Tribes, dfn)
  lm_model_both <- lm(dfn[,x] ~ Race + Hidden_Tribes, dfn)
  lm_model_all <- lm(dfn[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, dfn)
  
  # Regs for each Race by Tribe only
  lm_model_tribe_white <- lm(dfn[dfn$race==1,x] ~ Hidden_Tribes, dfn[dfn$race==1,])
  lm_model_tribe_black <- lm(dfn[dfn$race==2,x] ~ Hidden_Tribes, dfn[dfn$race==2,])
  lm_model_tribe_hispanic <- lm(dfn[dfn$race==3,x] ~ Hidden_Tribes, dfn[dfn$race==3,])
  
  # Question info
  df_results[x,"qid"] = x
  df_results[x,"Question"] = as.character(qsdf$Question[row])
  df_results[x,"Title"] = as.character(qsdf$Title[row])
  df_results[x,"survey"] = "EIP"
  
  # Summaries of linear models
  df_results[x,"Race_R2"] = summary(lm_model_race)$r.squared
  df_results[x,"Tribe_R2"] = summary(lm_model_tribe)$r.squared
  df_results[x,"Both_R2"] = summary(lm_model_both)$r.squared
  df_results[x,"All_R2"] = summary(lm_model_all)$r.squared
  df_results[x,"Tribe_white_R2"] = summary(lm_model_tribe_white)$r.squared
  df_results[x,"Tribe_black_R2"] = summary(lm_model_tribe_black)$r.squared
  df_results[x,"Tribe_hispanic_R2"] = summary(lm_model_tribe_hispanic)$r.squared
  
  
  ## Logit models
  # Create training sample
  smp_size <- floor(0.75 * nrow(dfn))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(dfn)), size = smp_size)
  train <- dfn[train_ind, ]
  test <- dfn[-train_ind, ]
  
  # Fit logit to training data - Race and Tribe
  glm.fit_race <- glm(train[,x] ~ Race, data=train, family='binomial')
  glm.fit_tribe <- glm(train[,x] ~ Hidden_Tribes, data=train, family='binomial')
  glm.fit_both <- glm(train[,x] ~ Race + Hidden_Tribes, data=train, family='binomial')
  glm.fit_all <- glm(train[,x] ~ Race + Hidden_Tribes + Generation_text + ReligionTri + EducationTri + Gender, data=train, family='binomial')
  #summary(glm.fit)
  
  # Predict test data
  y_act <- test[,x]
  
  pred_race <- predict(glm.fit_race, newdata = test, type = "response")
  y_pred_num_race <- ifelse(pred_race > 0.5, 1, 0)
  y_pred_race <- factor(y_pred_num_race, levels=c(0, 1))
  
  pred_tribe <- predict(glm.fit_tribe, newdata = test, type = "response")
  y_pred_num_tribe <- ifelse(pred_tribe > 0.5, 1, 0)
  y_pred_tribe <- factor(y_pred_num_tribe, levels=c(0, 1))
  
  pred_both <- predict(glm.fit_both, newdata = test, type = "response")
  y_pred_num_both <- ifelse(pred_both > 0.5, 1, 0)
  y_pred_both <- factor(y_pred_num_both, levels=c(0, 1))
  
  pred_all <- predict(glm.fit_all, newdata = test, type = "response")
  y_pred_num_all <- ifelse(pred_all > 0.5, 1, 0)
  y_pred_all <- factor(y_pred_num_all, levels=c(0, 1))
  
  
  # Save results
  df_results[x,"Race_Acc"] = mean(y_pred_race == y_act, na.rm = TRUE)
  df_results[x,"Tribe_Acc"] = mean(y_pred_tribe == y_act, na.rm = TRUE)
  df_results[x,"Both_Acc"] = mean(y_pred_both == y_act, na.rm = TRUE)
  df_results[x,"All_Acc"] = mean(y_pred_all == y_act, na.rm = TRUE)
  #confusionMatrix(y_pred,as.factor(y_act))
}

write.csv(df_results,"EIP_results.csv", row.names = FALSE)
EIP_results <- data.frame(df_results)



#### Combine results ####

all_results <- rbind(AF_results, HT_results, EIP_results)
write.csv(all_results,"All_regressions.csv", row.names = FALSE)
