load("C:/Users/siddh/R/Projects/hourly_data/P19_10days_merged_Clean.Rda")

# Please note current script is tailored toward specific DV and IV values for a current Mirzadeh lab project 
# Changes would have to be made to ensure your variables are the subject of the script

# The script is designed such that each "section" of the script CREATES and then RUNS "mini-functions" that address parts of the ANOVA process
## Library Importing
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(readxl)
library(scales)




# User Input: Step 1
data_new <- as_tibble(df.hourly)

## Running this line will package the entire script into one function. 
# You instead have the option of manually "creating" and running each function for assumptions, ANOVA, post hoc, and visualization
anova_general <- function(data_new) {
  ## This is something to consider!! Take NOTE: THE FACTOR CONVERSION IS EXTREMELY SPECIFIC 
  
  # Step 2 - Assumptions:
  

  # Assumption: Outlier Check
  # Below Function tests for outliers. 
  # If outliers undetected, script proceeds
  # If outliers detected, script presents outliers, and user is given option of eliminating or keeping the outlier in their analysis
  
  option_outliers <- function(data_new) {
    outliers <- data_new %>%
      identify_outliers(FoodIn.cum.kcal)
    # Identifies outliers
    if (nrow(outliers) > 0) {
      # Tests if outliers were produced by seeing if the outliers function produces a size > 0
      
      cat("Outliers detected:\n")
      print(outliers)
      # Shows outliers
      
      cat("\nOptions:\n")
      cat("1. Eliminate detected outliers\n")
      cat("2. Keep detected outliers\n")
      option <- as.integer(trimws(readline("Enter your choice (1 or 2): ")))
      # Offer options for treatment
      
      if (option == 1) {
        data_no_outlier <- anti_join(data_new, outliers)
        View(data_no_outlier)
        assign('data_new', data_no_outlier, envir=.GlobalEnv)
      }
      # Recreate dataframe without outlier index. It will continue to be called data_new
      else if (option == 2) {
        message("Script will proceed!")
        return(NULL)
        # Return original data, keeping outlier in the data without elimination
      }
    }
    else {
      message("No outliers, script will proceed!")
      return(NULL)
      # No outlier pathway
    }
  }
  option_outliers(data_new)
  
  
  # Assumption: Normality
  # Tests normality through 3 possible ways, with descriptions and information in "cat" message presentations
  check_normals <- function(data_new) {
    cat("\nPlease Select:\n")
    cat("1. qqplot, meant for sample size>50 (prone to unreadable visualization if data is too large)\n")
    cat("2. Shapiro Test, prone to over-sensitivity in minor deviations for large data\n")
    cat("3. Histogram, for subjective self-reference, if data is too large for above options")
    choose <- as.integer(trimws(readline("Enter your choice (1, 2, or 3): ")))
    if (choose == 1) { # qqplot choice
      # Specify faceting 
      cat("\nPlease Select how you would like your data to be faceted in Time:\n")
      cat("1. Facet by unique time points in dataframe (please note too many unique points will create an unreadable plot)\n")
      cat("2. If time points are in hours, facet by days. Please note both options may take some time to generate the plot\n")
      qqchoose <- as.integer(trimws(readline("Enter your choice (1 or 2): ")))
      if (qqchoose == 1) {## qqplot choice 1
      p <- ggqqplot(data_new, "FoodIn.cum.kcal", ggtheme = theme_bw()) +
        facet_grid(Group~Time)
      print(p)
      cat("\nPlease see plot printed. Are you satisfied with Normality?:\n")
      cat("1. Yes\n")
      cat("2. No\n")
        normals <- as.integer(trimws(readline("Enter your choice (1 or 2): ")))
        if (normals == 1) {
        message("Normality Assumption Satisfied")
      }
        if (normals == 2) {
        message("The 2-way Mixed ANOVA is fairly robust even under slight normality violations. Depending on the perceived normality from the qqplot, you also have the option of continuing with this script. Otherwise, alternate statistical analysis may need to be considered")
        }
    }
    if (qqchoose == 2) { ## qqplot choice 2
        ## Conversion to Days
        data_qqtemp <- data_new
        data_qqtemp$days <- data_new$Time/24
        data_qqtemp$days <- floor(data_qqtemp$days)
        p1 <- ggqqplot(data_qqtemp, "FoodIn.cum.kcal", ggtheme = theme_bw()) +
          facet_grid(Group~days)
        print(p1)
        cat("\nPlease see plot printed. Are you satisfied with Normality?:\n")
        cat("1. Yes\n")
        cat("2. No\n")
        normalsqq <- as.integer(trimws(readline("Enter your choice (1 or 2): ")))
        if (normalsqq == 1) {
          message("Normality Assumption Satisfied")
        }
        if (normalsqq == 2) {
          message("The 2-way Mixed ANOVA is fairly robust even under slight normality violations. Depending on the perceived normality from the qqplot, you also have the option of continuing with this script. Otherwise, alternate statistical analysis may need to be considered")
        }
    }
    }
    if (choose == 2) { ## shapiro test choice
      c <- shapiro.test(data_new$FoodIn.cum.kcal)
      print(c)
      cat("\nPlease see p-value printed. A p-value under 0.05 according to the Shapiro test, is statistically significant and deviates from normality. Please keep in mind this test is extremely sensitive to deviation. Are you satisfied with Normality?:\n")
      cat("1. Yes\n")
      cat("2. No\n")
        normals2 <- as.integer(trimws(readline("Enter your choice (1 or 2): ")))
        if (normals2 == 1) {
        message("Normality Assumption Satisfied")
      }
        if (normals2 == 2) {
        message("The 2-way Mixed ANOVA is fairly robust even under slight normality violations. The shapiro test is extremely sensitive to deviations, especially for larger data sets. You also have the option of continuing with this script. Otherwise, alternate statistical analysis may need to be considered")
      }
    }
    if (choose == 3) { ## basic histogram choice
      hist(data_new$FoodIn.cum.kcal, xlab = "Food Intake (kcal)", main="Histogram of Food Intake")
        cat("\nPlease see the histogram modelling your data. Are you satisfied with Normality?:\n")
        cat("1. Yes\n")
        cat("2. No\n")
        normals2 <- as.integer(trimws(readline("Enter your choice (1 or 2): ")))
        if (normals2 == 1) {
          message("Normality Assumption Satisfied")
        }
        if (normals2 == 2) {
          message("The 2-way Mixed ANOVA is fairly robust even under slight normality violations. Depending on the perceived normality from the plot, you also have the option of continuing with this script. Otherwise, alternate statistical analysis may need to be considered")
        }
      }
  }
  check_normals(data_new) # Run above function
  
  # Conversion to Factor (do only post-outlier and post-normality)
  data_new <- data_new %>%
    convert_as_factor(Animal, Time, Group)
  
  # Assumption: Homogeneity of Variance
  # The Levene Test is used for checking this
  check_variance <- function(data_new) {
    levene <- data_new %>%
      group_by(Time) %>%
      levene_test(FoodIn.cum.kcal ~ Group)
    detected_rows <- levene %>%
      filter(p < 0.05)
    # A value under 0.05 for p is usually an indicator of violation of this assumption
    if (nrow(detected_rows) > 0) {
      message("Levene Test Violation Detected:")
      print(detected_rows)
      message("This data does not pass the Levene Homogeneity of Variance Test. Alternate statistical analysis may need to be considered. You also have the option of proceeding.")
      
    } else {
      message("Levene Test Assumption Satisfied")
    }
  }
  
  check_variance(data_new)
  
  # Assumption: Homogeneity of Covariance
  # The Box's M Test is used here. Due to the extreme sensitivity of this test, the qualification to pass is p>0.001. Even if data does not meet this low threshold, however, many will continue the ANOVA analysis
  check_covariance <- function(data_new){
    cov <- box_m(data_new[, "FoodIn.cum.kcal", drop = FALSE], data_new$Group)
    print(cov)
    if (cov$p.value < 0.001) {
      # A value under 0.001 for p is usually an indicator of violation of this assumption
      
      message("This data does not pass the Box's M Homogeneity of Covariance Test. Alternate statistical analysis may need to be considered. Often the violation may just need to be noted, but the Mixed ANOVA test can be run anyway. Additionally, the Greenhouse-Geisser correction will be later applied in this program, which can often help account slightly for this violation.")
    }
    else {
      message("Box's M Test Assumption Satisfied")
    }
  }
  
  check_covariance(data_new)
  
# Now, ANOVA test is run and checks for any effects. It also applies corrections (specified below)
  run_anova <- function(data_new) {
    ## Applying Greenhouse-Geisser sphericity correction and Mauchly Test for Sphericity
    res.aov <- anova_test(
      data = data_new, dv = FoodIn.cum.kcal, wid = Animal,
      between = Group, within = Time
    )
    message("Mauchly Test internally applied in res.aov")
    
    message("get_anova_table: the Greenhouse-Geisser sphericity correction is automatically applied to factors violating the sphericity assumption.")
    aov <- get_anova_table(res.aov)
    print(aov)
    if (aov[1,6] == '*') {
      message("Group Main Effect Detected")
    }
    if (aov[2,6] == '*') {
      message("Time Main Effect Detected")
    }
    if(aov[3,6] == '*') {
      message("Interaction Between Group and Time Detected")
    }
    assign('aov', aov, envir=.GlobalEnv)
    return(aov)
    # returns an ANOVA dataframe with key details. This can be retrieved later as "aov". 
  }
  run_anova(data_new)
  
  # Post Hoc, seeing Group effects at every time point
  treat_maineffect<- function(data_new) {
    print("Now running post-hoc tests for signifcance by time point. Depending on data size, this may take slightly longer than expected")
    one_way_test <- data_new %>%
      group_by(Time) %>%
      anova_test(dv = FoodIn.cum.kcal, wid = Animal, between = Group) %>%
      get_anova_table() %>%
      adjust_pvalue(method = "bonferroni") %>%
      as_tibble()
    View(one_way_test)
    colnames(one_way_test)[7] <- "Significance"
    # Find Significance from dataframe. Below line builds a dataframe based only on significance.
    # Then a row count is done to check if significance is present
    detected_sig <- subset(one_way_test, grepl("\\*", Significance))
    
    
    print(one_way_test)
    if (nrow(detected_sig) > 0) {
      message("Significance in Group main effect was detected at certain time points")
      message("Please see complete table above to record time points where signfificance was seen")
    }
    else {
      message("No Significance was detected on the dependent variable between groups at any time point")
    }
  }
  treat_maineffect(data_new)
  
  ## Pairwise Data frame Construction for Plotting. This function accomplishes the same thing as the post-hoc test above, but does so for the prupose of visualization. This table is also retreivable
  make_pairwise <- function(data_new) {
    Max_mass <- data_new %>%
      group_by(Time) %>%
      summarize(max(FoodIn.cum.kcal))
    # The above code generates a data frame for maximum DV value  to be added to the pairwise function. 
    # This is so that the mass function can be carried to significant dataframes for plotting of significance indicators later
    pw <<- data_new %>%
      group_by(Time) %>%
      pairwise_t_test(FoodIn.cum.kcal ~ Group, p.adjust.method = "bonferroni") %>%
      add_column(yplacement = Max_mass$'max(FoodIn.cum.kcal)')
    # The above last line is for adding a coordinate for assisting in placement of significance labels in the pw dataframe
    View(pw)
    pw <- print(pw)
    return(pw)
  }
  make_pairwise(data_new)
  
  #NOTE: THERE IS POTENTIAL TO REPEAT THIS POST HOC TEST, BUT GROUPED BY BETWEEN VARIABLE AND MEASURING THE EFFECT OF TIME
  
  ### PLEASE RUN THIS FUNCTION IF YOU WOULD AN UNALTERED GRAPH (THIS MAY TAKE SLIGHTLY LONG)
  ## Use this pairwise dataframe to generate final boxplot
  final_box1 <- function(pw) {
    singlesig <- pw %>%
      filter(p.signif == "*") %>%
      select(Time, p, yplacement)
    
    ## Above and below code blocks are for setting the y-value placement of the asterisk (signficance indicators)
    
    doublesig <- pw %>%
      filter(p.signif == "**") %>%
      select(Time, p, yplacement)
    
    cat("\nNow making visualization. Please choose interval size for Time (x-axis):\n")
    int_chosen <- as.integer(trimws(readline("Enter your choice as a numeric value based on how far apart you would like your time variable spaced: ")))
    ## Choosing interval size to avoid unreadable x-axis
    data_new$Time <- as.numeric(as.character(data_new$Time))
    ## Conversion to numeric for processing in labeling adjustment
    
    # Choosing F-statistic presentation
    cat("\nPlease choose the precise F-statistic type you would like on your plot:\n")
    cat("1. Group Main Effect\n")
    cat("2. Time Main Effect\n")
    cat("3. Group-Time Interaction Effect\n")
    effect_type <- as.integer(trimws(readline("Enter your choice as 1, 2, or 3: ")))
    if (effect_type == 1) {
      f_chosen = 1
      Effect_F <- aov$'F'[1]
      Effect_F
    }
    if (effect_type == 2) {
      f_chosen = 2
      Effect_F <- aov$'F'[2]
      Effect_F
    }
    if (effect_type == 3) {
      f_chosen = 3
      Effect_F <- aov$'F'[3]
      Effect_F
    }
  fin_bxp <- ggboxplot(
    data_new, x = "Time", y = "FoodIn.cum.kcal",
    color = "Group", palette = "jco") +
    geom_text(x = max(as.numeric(data_new$Time)), y = min(data_new$FoodIn.cum.kcal),
              label = sprintf("F(%.2f) = %.3f", aov$DFn[f_chosen], Effect_F),
              # The printed value on the plot represents the DF and F statistic value. 
              # For a breakdown on p-values by time point, please reference the "pw" DF
              hjust = 1, vjust = 1, color = "red") +
    geom_text(data = singlesig, aes(x=Time, y=yplacement, label = "*"), size = 5, hjust = 0.5, vjust = -0.8, color = "red") +
    geom_text(data = doublesig, aes(x=Time, y=yplacement, label = "**"), size = 5, hjust = 0.5, vjust = -0.8, color = "red") +
    labs(x = "Time (hours)", y = "Food Intake (kcal)") +
    scale_x_discrete(breaks = seq(0, max(data_new$Time), by = int_chosen))+      
    theme_minimal()
  fin_bxp <- fin_bxp + expand_limits(y = 0)
  fin_bxp
}
final_box1(pw)
  
###################################
summary_data <- data_new %>%
  group_by(Group, Time) %>%
  summarise(mean_y = mean(FoodIn.cum.kcal),
            se_y = sd(FoodIn.cum.kcal) / sqrt(n()))

singlesig <- pw %>%
  filter(p.signif == "*") %>%
  select(Time, p, yplacement)
singlesig$Time <- as.numeric(as.character(singlesig$Time))


## Above and below code blocks are for setting the y-value placement of the asterisk (signficance indicators)

doublesig <- pw %>%
  filter(p.signif == "**") %>%
  select(Time, p, yplacement)
doublesig$Time <- as.numeric(as.character(doublesig$Time))
###################################

## PLEASE RUN THIS FUNCTION IF YOU WOULD LIKE A SMOOTHED GRAPH (MAY TAKE SLIGHTLY LONG)
  final_box2 <- function(pw) {
    singlesig <- pw %>%
      filter(p.signif == "*") %>%
      select(Time, p, yplacement)
    singlesig$Time <- as.numeric(as.character(singlesig$Time))
    
    
    ## Above and below code blocks are for setting the y-value placement of the asterisk (signficance indicators)
    
    doublesig <- pw %>%
      filter(p.signif == "**") %>%
      select(Time, p, yplacement)
    doublesig$Time <- as.numeric(as.character(doublesig$Time))
    
    
    cat("\nNow making visualization. Please choose interval size for Time (x-axis):\n")
    int_chosen <- as.integer(trimws(readline("Enter your choice as a numeric value based on how far apart you would like your time variable spaced: ")))
    ## Choosing interval size to avoid unreadable x-axis
    summary_data$Time <- as.numeric(as.character(summary_data$Time))
    data_new$Time <- as.numeric(as.character(data_new$Time))
    ## Conversion to numeric for processing in labeling adjustment
   
    # Choosing F-statistic presentation
    cat("\nPlease choose the precise F-statistic type you would like on your plot:\n")
    cat("1. Group Main Effect\n")
    cat("2. Time Main Effect\n")
    cat("3. Group-Time Interaction Effect\n")
    effect_type <- as.integer(trimws(readline("Enter your choice as 1, 2, or 3: ")))
    if (effect_type == 1) {
      f_chosen = 1
      Effect_F <- aov$'F'[1]
      Effect_F
    }
    if (effect_type == 2) {
      f_chosen = 2
      Effect_F <- aov$'F'[2]
      Effect_F
    }
    if (effect_type == 3) {
      f_chosen = 3
      Effect_F <- aov$'F'[3]
      Effect_F
    }
    fin_bxp2 <- 
      ggplot(summary_data, aes(x = Time, y = mean_y, color = Group)) +
      stat_summary(data = summary_data, fun = "mean", geom = "line", aes(group = Group, x=Time, y=mean_y)) +
      stat_summary(data = summary_data, aes(x=Time, y = mean_y, group = Group), fun.data = mean_se, geom = "ribbon", alpha = 0.2) +
      geom_ribbon(data = summary_data, aes(ymin = mean_y - se_y, ymax = mean_y + se_y, fill = Group), alpha = 0.2, show.legend = FALSE) +
      geom_text(x = max(as.numeric(data_new$Time)), y = min(data_new$FoodIn.cum.kcal),
                label = sprintf("F(%.2f) = %.3f", aov$DFn[f_chosen], Effect_F),
                # The printed value on the plot represents the DF and F statistic value. 
                # For a breakdown on p-values by time point, please reference the "pw" DF
                hjust = 1, vjust = 1, color = "red") +
      geom_text(data = singlesig, aes(x=Time, y=yplacement, label = "*"), size = 5, hjust = 0.5, vjust = -0.8, color = "red") +
      geom_text(data = doublesig, aes(x=Time, y=yplacement, label = "**"), size = 5, hjust = 0.5, vjust = -0.8, color = "red") +
      labs(x = "Time (hours)", y = "Food Intake (kcal)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(0, max(data_new$Time), by = int_chosen)) 

    fin_bxp2 <- fin_bxp2 + expand_limits(y = 0)
    fin_bxp2
  }
  final_box2(pw)
}

# Below line is for running the "mega" function which encapsulates each section into one. See above instructions at the top of the script. 
anova_general(data_new)



