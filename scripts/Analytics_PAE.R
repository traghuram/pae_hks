# 
# ---
# title: "PAE analysis - Charts"
# author: "Taran Raghuram"
# date: "1/11/2022"
# output:
#   html_document: default
# pdf_document: default
# ---

############### Import libraries ##################
# Run Daniel's R file
source("C:/Users/Taran/OneDrive - Harvard University/HKS/Coursework/5 - Fall 2021/0 - PAE/Week 4 - Analysis/R Functions.R")
library(stringr)

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

title_width = 75
title_size = 14
x_size = 10



############### American Fabric #######################

##### Data Setup ####

## Load Dataset
setwd("/Users/Taran/Desktop/MiC data/")
df<- read.csv("minc0007-american-fabric to use.csv", header = T, na.strings = ".")


### Add columns

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
#df$Race <- as.factor(df$Race)
#df$Race <- factor(df$Race, levels = c("White", "Black", "Hispanic", "Asian", "Native American", "Mixed", "Other", "Middle Eastern"))



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

# Add Abbr Tribes
df <- df %>%
  mutate(Hidden_Tribes_abbr = 
           case_when(
             HT.Segements.Ordered == 6 ~ "Prog Acts",
             HT.Segements.Ordered == 5 ~ "Trad Libs",
             HT.Segements.Ordered == 4 ~ "Pass Libs",
             HT.Segements.Ordered == 3 ~ "Pol Dis",
             HT.Segements.Ordered == 2 ~ "Mods",
             HT.Segements.Ordered == 1 ~ "Trad Cons",
             HT.Segements.Ordered == 0 ~ "Dev Cons"
           ))
df$Hidden_Tribes_abbr  <- as.factor(df$Hidden_Tribes_abbr)
df$Hidden_Tribes_abbr <- factor(df$Hidden_Tribes_abbr, levels = c("Prog Acts", "Trad Libs",
                                                        "Pass Libs", "Pol Dis",
                                                        "Mods", "Trad Cons", "Dev Cons"))


## Restrict analysis to White, Black, Hispanic
df <- df[df$race < 4,]




##### Create charts #####

###### Normal questions

## Import questions
qsdf <- read.csv("questions - AF.csv", header = T)
qs <- as.character(qsdf$Short.name)

## Create one chart for each row of qsdf file
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Col name and question
  colm <- as.character(qsdf$Short.name[row])
  print(qsdf[row,"Question"])
  print(qsdf[row,"Title"])
  
  ## collapse data in this column into binary responses
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # Get data ranges for each response
    l1 <- qsdf[row, "l1"]
    l2 <- qsdf[row, "l2"]
    r1 <- qsdf[row, "r1"]
    r2 <- qsdf[row, "r2"]
    
    # Replace values outside range with NAs
    df[df[,colm]>r2 & !is.na(df[,colm]), colm] <- NA
    
    # If column one, convert all responses between l1 and l2 to 1, and r1 and r2 to 0
    if (qsdf[row,"Col.to.show"] == 1) {
      
      df[df[,colm]<=l2 & df[,colm]>=l1 & !is.na(df[,colm]), colm] <- 1
      df[df[,colm]<=r2 & df[,colm]>=r1 & !is.na(df[,colm]), colm] <- 0
    }
    
    # Else, convert l1 to l2 to 0, r1 to r2 to 1
    else {
      
      df[df[,colm]<=l2 & df[,colm]>=l1 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]<=r2 & df[,colm]>=r1 & !is.na(df[,colm]), colm] <- 1
    }
    
  }
  
  else {
    
    # if column 1, change values of 2 to 0
    if (qsdf[row,"Col.to.show"] == 1) {
      df[df[,colm]==2 & !is.na(df[,colm]), colm] <- 0
    }
    
    # else subtract one from both values
    else {
      df[df[,colm]==1 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]==2 & !is.na(df[,colm]), colm] <- 1
    }
    
  }
  
  
  # Grab a few chart parameters from the questions file
  title = str_wrap(qsdf[row,"Title"])
  question = str_wrap(qsdf[row,"Question"])
  y_lab = qsdf[row,"Y.axis.Label"]
  #df[,"tribes_wrap"] = strwrap(df[,"Hidden_Tribes"])
  
  ## Create chart using Daniel's code
  print(my.grouped.boxplot(df,x="Hidden_Tribes_abbr", y=colm, group="Race", ylab = y_lab) +
          ggtitle(wrapper(title, width = title_width)) +
          theme(plot.title = element_text(size = title_size), axis.text.x = element_text(size = x_size)) +
          labs(caption = "(American Fabric, July 2020)"))
  
  print(title)
  print(question)
}




###### Media questions

## Import questions
qsdf <- read.csv("questions - AF media.csv", header = T)
qs <- as.character(qsdf$Short.name)


## Create one chart for each row of qsdf file
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Col name and question
  colm <- as.character(qsdf$Short.name[row])
  print(qsdf[row,"Question"])
  print(qsdf[row,"Title"])
  treatments <- c("Fox News, Wall Street Journal, and the Daily Wire", "MSNBC, CNN, and the New York Times")
  
  
  ## for each media treatment, run charts
  for (i in 1:2) {
    
    df2 <- df[df$media_treat==i,]
  
    ## collapse data in this column into binary responses
    if (qsdf$Grouping.Required[row] == "Yes") {
      
      # Get data ranges for each response
      l1 <- qsdf[row, "l1"]
      l2 <- qsdf[row, "l2"]
      r1 <- qsdf[row, "r1"]
      r2 <- qsdf[row, "r2"]
      
      # Replace values outside range with NAs
      df2[df2[,colm]>r2 & !is.na(df2[,colm]), colm] <- NA
      
      # If column one, convert all responses between l1 and l2 to 1, and r1 and r2 to 0
      if (qsdf[row,"Col.to.show"] == 1) {
        
        df2[df2[,colm]<=l2 & df2[,colm]>=l1 & !is.na(df2[,colm]), colm] <- 1
        df2[df2[,colm]<=r2 & df2[,colm]>=r1 & !is.na(df2[,colm]), colm] <- 0
      }
      
      # Else, convert l1 to l2 to 0, r1 to r2 to 1
      else {
        
        df2[df2[,colm]<=l2 & df2[,colm]>=l1 & !is.na(df2[,colm]), colm] <- 0
        df2[df2[,colm]<=r2 & df2[,colm]>=r1 & !is.na(df2[,colm]), colm] <- 1
      }
      
    }
    
    else {
      
      # if column 1, change values of 2 to 0
      if (qsdf[row,"Col.to.show"] == 1) {
        df2[df2[,colm]==2 & !is.na(df2[,colm]), colm] <- 0
      }
      
      # else subtract one from both values
      else {
        df2[df2[,colm]==1 & !is.na(df2[,colm]), colm] <- 0
        df2[df2[,colm]==2 & !is.na(df2[,colm]), colm] <- 1
      }
      
    }
    
    
    # Grab a few chart parameters from the questions file
    title = paste(qsdf[row,"Title"], treatments[i], sep = " - ")
    question = str_wrap(qsdf[row,"Question"])
    y_lab = qsdf[row,"Y.axis.Label"]
    #df[,"tribes_wrap"] = strwrap(df[,"Hidden_Tribes"])
    
    ## Create chart using Daniel's code
    print(my.grouped.boxplot(df2,x="Hidden_Tribes_abbr", y=colm, group="Race", ylab = y_lab) +
            ggtitle(wrapper(title, width = title_width)) +
            theme(plot.title = element_text(size = title_size), axis.text.x = element_text(size = x_size)) +
            labs(caption = "(American Fabric, July 2020)"))
   
    print(title)
    print(question) 
  
  }
  
}




###### Message testing

## Import questions
qsdf <- read.csv("questions - AF MT.csv", header = T)
qs <- as.character(qsdf$Short.name)

## Create one chart for each row of qsdf file
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Col name and question
  colm <- as.character(qsdf$Short.name[row])
  print(qsdf[row,"Question"])
  print(qsdf[row,"Title"])
  
  ## Grab a few chart parameters from the questions file
  title = str_wrap(qsdf[row,"Title"])
  question = str_wrap(qsdf[row,"Question"])
  y_lab = qsdf[row,"Y.axis.Label"]
  
  
  ## Create chart using Daniel's code
  print(my.grouped.boxplot(df,x="Hidden_Tribes_abbr", y=colm, group="Race", ylab = y_lab) +
          ggtitle(wrapper(title, width = title_width)) +
          theme(plot.title = element_text(size = title_size), axis.text.x = element_text(size = x_size)) +
          labs(caption = "(American Fabric, December 2020)"))
  
  print(title)
  print(question)
}





################ EIP ################

##### Data Setup #####

## Load Dataset
setwd("/Users/Taran/Desktop/MiC data/")
df<- read.csv("minc0019-eip-wave-1-3  to use.csv", header = T, na.strings = ".")


### Add columns

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
             Hidden.Tribes_Correct == 6 ~ "Progressive Activists",
             Hidden.Tribes_Correct == 5 ~ "Traditional Liberals",
             Hidden.Tribes_Correct == 4 ~ "Passive Liberals",
             Hidden.Tribes_Correct == 3 ~ "Politically Disengaged",
             Hidden.Tribes_Correct == 2 ~ "Moderates",
             Hidden.Tribes_Correct == 1 ~ "Traditional Conservatives",
             Hidden.Tribes_Correct == 0 ~ "Devoted Conservatives"
           ))
df$Hidden_Tribes  <- as.factor(df$Hidden_Tribes)
df$Hidden_Tribes <- factor(df$Hidden_Tribes, levels = c("Progressive Activists", "Traditional Liberals",
                                                        "Passive Liberals", "Politically Disengaged",
                                                        "Moderates", "Traditional Conservatives", "Devoted Conservatives"))

# Add Abbr Tribes
df <- df %>%
  mutate(Hidden_Tribes_abbr =
           case_when(
             Hidden.Tribes_Correct == 6 ~ "Prog Acts",
             Hidden.Tribes_Correct == 5 ~ "Trad Libs",
             Hidden.Tribes_Correct == 4 ~ "Pass Libs",
             Hidden.Tribes_Correct == 3 ~ "Pol Dis",
             Hidden.Tribes_Correct == 2 ~ "Mods",
             Hidden.Tribes_Correct == 1 ~ "Trad Cons",
             Hidden.Tribes_Correct == 0 ~ "Dev Cons"
           ))
df$Hidden_Tribes_abbr  <- as.factor(df$Hidden_Tribes_abbr)
df$Hidden_Tribes_abbr <- factor(df$Hidden_Tribes_abbr, levels = c("Prog Acts", "Trad Libs",
                                                                  "Pass Libs", "Pol Dis",
                                                                  "Mods", "Trad Cons", "Dev Cons"))


## Restrict analysis to White, Black, Hispanic
df <- df[df$race < 4,]


##### Create charts #####

## Import questions
qsdf <- read.csv("questions - EIP.csv", header = T)
qs <- as.character(qsdf$Short.name)


## Create one chart for each row of qsdf file
for (row in 1:nrow(qsdf)) { #nrow(qsdf)

  # Col name and question
  colm <- as.character(qsdf$Short.name[row])

  ## collapse data in this column into binary responses
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # Get data ranges for each response
    l1 <- qsdf[row, "l1"]
    l2 <- qsdf[row, "l2"]
    r1 <- qsdf[row, "r1"]
    r2 <- qsdf[row, "r2"]
    
    # Replace values outside range with NAs
    df[df[,colm]>r2 & !is.na(df[,colm]), colm] <- NA

    # If column one, convert all responses between l1 and l2 to 1, and r1 and r2 to 0
    if (qsdf[row,"Col.to.show"] == 1) {

      df[df[,colm]<=l2 & df[,colm]>=l1 & !is.na(df[,colm]), colm] <- 1
      df[df[,colm]<=r2 & df[,colm]>=r1 & !is.na(df[,colm]), colm] <- 0
    }

    # Else, convert l1 to l2 to 0, r1 to r2 to 1
    else {

      df[df[,colm]<=l2 & df[,colm]>=l1 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]<=r2 & df[,colm]>=r1 & !is.na(df[,colm]), colm] <- 1
    }

  }

  else {

    # if column 1, change values of 2 to 0
    if (qsdf[row,"Col.to.show"] == 1) {
      df[df[,colm]==2 & !is.na(df[,colm]), colm] <- 0
    }

    # else subtract one from both values
    else {
      df[df[,colm]==1 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]==2 & !is.na(df[,colm]), colm] <- 1
    }

  }


  # Grab a few chart parameters from the questions file
  title = str_wrap(qsdf[row,"Title"])
  question = str_wrap(qsdf[row,"Question"])
  y_lab = qsdf[row,"Y.axis.Label"]

  ## Create chart using Daniel's code
  print(my.grouped.boxplot(df,x="Hidden_Tribes_abbr", y=colm, group="Race", ylab = y_lab) +
          ggtitle(wrapper(title, width = title_width)) +
          theme(plot.title = element_text(size = title_size), axis.text.x = element_text(size = x_size)) +
          labs(caption = "(Democracy for President, Sept 2020)"))

  print(title)
  print(question)
  
}






################ HT ################

##### Data Setup #####

## Load Dataset
setwd("/Users/Taran/Desktop/MiC data/")
df<- read.csv("Hidden Tribes Data - Levels for Sharing.csv", header = T, na.strings = ".")


### Add columns

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
df$Hidden_Tribes  <- as.factor(df$Segment)
df$Hidden_Tribes <- factor(df$Hidden_Tribes, levels = c("Progressive Activists", "Traditional Liberals",
                                                        "Passive Liberals", "Politically Disengaged",
                                                        "Moderates", "Traditional Conservatives", "Devoted Conservatives"))

# Add Abbr Tribes
df <- df %>%
  mutate(Hidden_Tribes_abbr =
           case_when(
             Hidden_Tribes == "Progressive Activists" ~ "Prog Acts",
             Hidden_Tribes == "Traditional Liberals" ~ "Trad Libs",
             Hidden_Tribes == "Passive Liberals" ~ "Pass Libs",
             Hidden_Tribes == "Politically Disengaged" ~ "Pol Dis",
             Hidden_Tribes == "Moderates" ~ "Mods",
             Hidden_Tribes == "Traditional Conservatives" ~ "Trad Cons",
             Hidden_Tribes == "Devoted Conservatives"  ~ "Dev Cons"
           ))
df$Hidden_Tribes_abbr  <- as.factor(df$Hidden_Tribes_abbr)
df$Hidden_Tribes_abbr <- factor(df$Hidden_Tribes_abbr, levels = c("Prog Acts", "Trad Libs",
                                                                  "Pass Libs", "Pol Dis",
                                                                  "Mods", "Trad Cons", "Dev Cons"))


## Restrict analysis to White, Black, Hispanic
df <- df[df$race < 4,]


##### Create charts #####

## Import questions
qsdf <- read.csv("questions - HT.csv", header = T)
qs <- as.character(qsdf$Short.name)


## Create one chart for each row of qsdf file
for (row in 1:nrow(qsdf)) { #nrow(qsdf)
  
  # Col name
  colm <- as.character(qsdf$Short.name[row])
  
  ## collapse data in this column into binary responses
  if (qsdf$Grouping.Required[row] == "Yes") {
    
    # Get data ranges for each response
    l1 <- qsdf[row, "l1"]
    l2 <- qsdf[row, "l2"]
    r1 <- qsdf[row, "r1"]
    r2 <- qsdf[row, "r2"]
    
    # Replace values outside range with NAs
    df[df[,colm]>r2 & !is.na(df[,colm]), colm] <- NA
    
    # If column one, convert all responses between l1 and l2 to 1, and r1 and r2 to 0
    if (qsdf[row,"Col.to.show"] == 1) {
      
      df[df[,colm]<=l2 & df[,colm]>=l1 & !is.na(df[,colm]), colm] <- 1
      df[df[,colm]<=r2 & df[,colm]>=r1 & !is.na(df[,colm]), colm] <- 0
    }
    
    # Else, convert l1 to l2 to 0, r1 to r2 to 1
    else {
      
      df[df[,colm]<=l2 & df[,colm]>=l1 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]<=r2 & df[,colm]>=r1 & !is.na(df[,colm]), colm] <- 1
    }
    
  }
  
  else {
    
    # if column 1, change values of 2 to 0
    if (qsdf[row,"Col.to.show"] == 1) {
      df[df[,colm]==2 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]==3 & !is.na(df[,colm]), colm] <- 0
    }
    
    # else subtract one from both values
    else {
      df[df[,colm]==1 & !is.na(df[,colm]), colm] <- 0
      df[df[,colm]==2 & !is.na(df[,colm]), colm] <- 1
    }
    
  }
  
  
  # Grab a few chart parameters from the questions file
  title = str_wrap(qsdf[row,"Title"])
  question = str_wrap(qsdf[row,"Question"])
  y_lab = qsdf[row,"Y.axis.Label"]
  
  ## Create chart using Daniel's code
  print(my.grouped.boxplot(df,x="Hidden_Tribes_abbr", y=colm, group="Race", ylab = y_lab) +
          ggtitle(wrapper(title, width = title_width)) +
          theme(plot.title = element_text(size = title_size), axis.text.x = element_text(size = x_size)) +
          labs(caption = "(Hidden Tribes, January 2018)"))
  
  print(title)
  print(question)
  
}

