
## Library Importing
library(tidyverse)
library(dplyr)
library(magrittr)
library(ggrepel)
library(ggpubr)
library(rstatix)
library(readxl)


# User Input: 
filename <- "2020-01-07_tx03_DailyMeasures - Sheet1.csv"
data <- as_tibble(read.csv(filename))
first_date <- "2020-01-07"
# Please ensure:
  # Date is given as month/day/year, and column is labelled "date" 
  # Treatment column is labelled as "treatment"
  # Body mass column is labelled body_mass_g
  # Subject column is labelled "sbj", and is 

## Data tidying
# data <- data %>%
  # gather(key = "date", value = c("mass", "treatment", 3:9))
         
# New Table will be given in:
  # days time elapsed called "time_elapsed"

## Formatting Data
anova_general <- function(filename, data, first_date) {
dt_t_tm <- function (filename, orig_dt) {
  data %<>% 
    mutate(date = as.POSIXct(date, format="%m/%d/%Y")) 
  ## Use POSIXct for changing date from char to date
  original_date <- as.Date(orig_dt) 
  time_elapsed <- difftime(data$date, original_date, units = 'days')
  # Conversion from date points to time elapsed since original date in days 
  time_elapsed <- round(time_elapsed)
  time_elapsed <- as.numeric(time_elapsed)
  data_new <- data %>%
    mutate(time_elapsed)
  return(data_new)
  # Use time_elapsed rounded 
}
data_new <- dt_t_tm("2020-01-07_tx03_DailyMeasures - Sheet1.csv", first_date)
View(data_new)

## This is something to consider!! Take NOTE: THE FACTOR CONVERSION IS EXTREMELY SPECIFIC 

# Step 1: Conversion to Factor
data_new <- data_new %>%
  convert_as_factor(sbj, time_elapsed, treatment)
# Step 2 - Assumptions:
# Outlier Check: 
# I want if no, proceed. If yes, then sidetrack and show outliers, offering deletion or inclusion
# Below Function tests for outliers. 
# If outliers undetected, script proceeds
# If outliers detected, script presents outliers, and user is given option of elmiinating or keeping the outlier in their analysis

option_outliers <- function(data_new) {
  outliers <- data_new %>%
    identify_outliers(body_mass_g)
  # Identifies outliers
  if (nrow(outliers) > 0) {
    # Tests if outliers were prodcued
    
    cat("Outliers detected:\n")
    print(outliers)
    # Show outliers
    
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
    # Return original data
   }
  }
  else {
    message("Script will proceed!")
    return(NULL)
  }
}
option_outliers(data_new)


# Normality

# Normality May need added check : Dataset greater than n=50? 
# For now, stick with depiction of qqplot 
check_normals <- function(data_new) {
p <- ggqqplot(data_new, "body_mass_g", ggtheme = theme_bw()) +
  facet_grid(time_elapsed ~ treatment)
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
check_normals(data_new)

# Homogeneity of Variance
check_variance <- function(data_new) {
levene <- data_new %>%
  group_by(time_elapsed) %>%
  levene_test(body_mass_g ~ treatment)
detected_rows <- levene %>%
  filter(p < 0.05)
if (nrow(detected_rows) > 0) {
  message("Levene Test Violation Detected:")
  print(detected_rows)
  message("This data does not pass the Levene Homogeneity of Variance Test. Alternate statistical analysis may need to be considered")
  
} else {
  message("Levene Test Assumption Satisfied")
}
}

check_variance(data_new)

# Homogeneity of Covariance
check_covariance <- function(data_new){
  cov <- box_m(data_new[, "body_mass_g", drop = FALSE], data_new$treatment)
  print(cov)
  if (cov$p.value < 0.001) {
  message("This data does not pass the Box's M Homogeneity of Covariance Test. Alternate statistical analysis may need to be considered")
  }
  else {
    message("Box's M Test Assumption Satisfied")
  }
}

check_covariance(data_new)

run_anova <- function(data_new) {
## Applying Greenhouse-Geisser sphericity correction and Mauchly Test for Sphericity
res.aov <- anova_test(
  data = data_new, dv = body_mass_g, wid = sbj,
  between = treatment, within = time_elapsed
)
message("Mauchly Test internally applied in res.aov")

message("get_anova_table: the Greenhouse-Geisser sphericity correction is automatically applied to factors violating the sphericity assumption.")
aov <- get_anova_table(res.aov)
print(aov)
if (aov[1,6] == '*') {
  message("Treatment Main Effect Detected")
}
if (aov[2,6] == '*') {
  message("Time Elapsed Main Effect Detected")
}
if(aov[3,6] == '*') {
  message("Interaction Between Treatment and Time Detected")
}
assign('aov', aov, envir=.GlobalEnv)
return(aov)
}
run_anova(data_new)

# Post Hoc
# Treatment Effects At Every Time Point
treat_maineffect<- function(data_new) {
one_way_test <- data_new %>%
  group_by(time_elapsed) %>%
  anova_test(dv = body_mass_g, wid = sbj, between = treatment) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni") %>%
  as_tibble()
one_way_test
colnames(one_way_test)[7] <- "Significance"
# Find Signficance from dataframe. Below line builds a dataframe based only on significance.
# Then a row count is done to check if signficance is present
detected_sig <- subset(one_way_test, grepl("\\*", Significance))


print(one_way_test)
if (nrow(detected_sig) > 0) {
  message("Signficiance in Treatment main effect was detected at certain time points")
  message("Please see complete table above to record time points where signfificance was seen")
}
else {
  message("No Significance was detected on body mass between treatments at any time point")
}
}
treat_maineffect(data_new)

## Pairwise Data frame Construction for Plotting 
make_pairwise <- function(data_new) {
Max_mass <- data_new %>%
  group_by(time_elapsed) %>%
  summarize(max(body_mass_g))
# The above code generates a data frame for maximum body mass to be added to pairwise function. 
# This is so that the mass function can be carried to significant dataframes and plotted later
pw <<- data_new %>%
  group_by(time_elapsed) %>%
  pairwise_t_test(body_mass_g ~ treatment, p.adjust.method = "bonferroni") %>%
  add_column(yplacement = Max_mass$'max(body_mass_g)')
# this last line is for adding a coordinate for assisting in placement of significance labels
pw <- print(pw)
return(pw)
}
make_pairwise(data_new)

#NOTE: THERE IS POTENTIAL TO REPEAT THIS POST HOC TEST, BUT GROUPED BY BETWEEN VARIABLE AND MEASURING THE EFFECT OF TIM

## Use this pairwise dataframe to generate final boxplot

final_box <- function(pw) {
  treatment_maineffect_p <- aov$'p'[1]
  treatment_maineffect_p
  ## Asterix at significant time points from pairwise results
  
  ## After extracting each p-value to its corresponding time point and making a dataframe of it, 
  ## use a new boxplot generation. In that, for geomtext, add it so that it is labelling these dataframes
  singlesig <- pw %>%
    filter(p.signif == "*") %>%
    select(time_elapsed, p, yplacement)

  ## Above and below code blocks are for setting the y-value placement of the asterisk

  doublesig <- pw %>%
    filter(p.signif == "**") %>%
    select(time_elapsed, p, yplacement)
  
  
  fin_bxp <- ggboxplot(
    data_new, x = "time_elapsed", y = "body_mass_g",
    color = "treatment", palette = "jco") +
    geom_text(x = max(as.numeric(data_new$time_elapsed)), y = min(data_new$body_mass_g),
              label = sprintf("F(%.2f) = %.3f", aov$DFn[1], treatment_maineffect_p),
              hjust = 1, vjust = 1, color = "red") +
    geom_text(data = singlesig, aes(x=time_elapsed, y=yplacement, label = "*"), size = 5, hjust = 0.5, vjust = -0.8, color = "red") +
    geom_text(data = doublesig, aes(x=time_elapsed, y=yplacement, label = "**"), size = 5, hjust = 0.5, vjust = -0.8, color = "red") +
    labs(x = "Age (Days)", y = "Body Mass (g)") +
    theme_minimal()
  fin_bxp <- fin_bxp + expand_limits(y = 0)
  fin_bxp
}
final_box(pw)
}
anova_general(filename, data, first_date)


