# PHASE 1: DATA FACTORING
# IN THIS PART OF THE CODE WE TURN CODED CATEGORICAL VASRIABLES INTO LABLES
# PLEASE REFER TO THE README FILE IN THE ZIP FOLDER FOR EASIER RUNNING OF THE CODE
# THIS CODE WILL SAVE THE LABLED DATASET AS A CSV FILE AND SEVERAL PLOTS AND SUMMARY TABLES IN ITS FOLDER WHEN RUN

#############################################################################


# Installing relevant libraries

install.packages("patchwork")
install.packages("readr")
install.packages("dplyr")
install.packages("survey")
install.packages("gt")
install.packages("ggcorrplot")
install.packages("ggbiplot")
install.packages("lm.beta")
install.packages("doParallel")
#...
# depends on the R version you are using and the libraries you have already installed


# Importing relevant libraries

# Data manipulation
library(tidyverse)  
library(data.table)

# Visualization
library(patchwork)
library(scales)    
library(ggcorrplot)
library(RColorBrewer)  
library(gt)
library(fmsb)
library(factoextra)

# Modeling and machine learning
library(caret)
library(xgboost)
library(randomForest)
library(recipes)
library(cluster)

# Survey analysis
library(survey)
library(mitools)

# Parallel processing
library(doParallel)  

# Handling missing data
library(mice)

# Importing the raw data set downloaded from the ATUS website

raw_data <- read_csv("D:/SL/ATUS CODED DATA.csv")


# Some initial checks on the data


str(raw_data)
colnames(raw_data)

# The following colomns will not be of use to our analysis and we will drop them


columns_to_drop <- c("YEAR", "CASEID", "SERIAL", "PERNUM", "LINENO")
raw_data <- raw_data[, !(colnames(raw_data) %in% columns_to_drop)]



# EVERY SINGLE CATEGIRICAL VARIABLE IS CHANGED WITH REGARDS TO ITS CODE IN THE DATASET MANUAL
# ALSO ALL IRRELAVANT ANSWERS ARE CHANGED TO NA (MISSING VALUES)

# RECODE REGION - Region of residence

raw_data$REGION <- factor(raw_data$REGION,
                    levels = c(1, 2, 3, 4),
                    labels = c("Northeast", "Midwest", "South", "West"))


# RECODE FAMINCOME - Family income
raw_data$FAMINCOME[raw_data$FAMINCOME %in% c(997, 998)] <- NA

raw_data$FAMINCOME <- factor(raw_data$FAMINCOME,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                    11, 12, 13, 14, 15, 16),
                         labels = c("Less than $5,000",
                                    "$5,000 to $7,499",
                                    "$7,500 to $9,999",
                                    "$10,000 to $12,499",
                                    "$12,500 to $14,999",
                                    "$15,000 to $19,999",
                                    "$20,000 to $24,999",
                                    "$25,000 to $29,999",
                                    "$30,000 to $34,999",
                                    "$35,000 to $39,999",
                                    "$40,000 to $49,999",
                                    "$50,000 to $59,999",
                                    "$60,000 to $74,999",
                                    "$75,000 to $99,999",
                                    "$100,000 to $149,999",
                                    "$150,000 and over"))


# Recode 99 (NIU) in HHTENURE to NA
raw_data$HHTENURE[raw_data$HHTENURE == 99] <- NA

# HHTENURE - Tenure of housing unit
raw_data$HHTENURE <- factor(raw_data$HHTENURE,
                        levels = c(1, 2, 3),
                        labels = c("Owned by household member",
                                   "Rented for cash",
                                   "No cash rent"))


# Recode 99 (NIU) in HHSIZE to NA
raw_data$HH_SIZE[raw_data$HH_SIZE == 99] <- NA

# recode 99 (niu) in HH_CHILD to NA
raw_data$HH_CHILD[raw_data$HH_CHILD == 99] <- NA


# recode 999 (nin) in AGEYCHILD to NA
raw_data$AGEYCHILD[raw_data$AGEYCHILD == 999] <- NA

# HOUSETYPE - Type of housing unit
raw_data$HOUSETYPE <- factor(raw_data$HOUSETYPE,
                         levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                         labels = c("House, apartment, flat",
                                    "Hotel/motel (nontransient)",
                                    "Permanent in transient hotel/motel",
                                    "Rooming house",
                                    "Mobile home w/o room added",
                                    "Mobile home w/ 1+ rooms added",
                                    "Housing unit not specified",
                                    "Quarters not housing unit",
                                    "Unit not permanent",
                                    "Tent site or trailer",
                                    "Student dorm",
                                    "Other not specified"))


# Recode 99 (NIU) in HH_CHILD to NA before converting to factor
raw_data$HH_CHILD[raw_data$HH_CHILD == 99] <- NA

# HH_CHILD - Any children under 18 in household
raw_data$HH_CHILD <- factor(raw_data$HH_CHILD,
                        levels = c(0, 1),
                        labels = c("No", "Yes"))

# HH_NUMKIDS - Replace NIU with NA
raw_data$HH_NUMKIDS[raw_data$HH_NUMKIDS == 99] <- NA

# HH_NUMADULTS - Replace NIU code (99) with NA
raw_data$HH_NUMADULTS[raw_data$HH_NUMADULTS == 99] <- NA

# Convert DAY to a labeled factor
raw_data$DAY[raw_data$DAY == 99] <- NA

# Convert DAY to a labeled factor
raw_data$DAY <- factor(raw_data$DAY,
                   levels = 1:7,
                   labels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                              "Thursday", "Friday", "Saturday"))


# AGE - Replace missing codes (996, 997, 999) with NA
raw_data$AGE[raw_data$AGE %in% c(996, 997, 999)] <- NA

# SEX - Gender - Recode NIU (99) to NA before converting to factor
raw_data$SEX[raw_data$SEX == 99] <- NA
raw_data$SEX <- factor(raw_data$SEX,
                   levels = c(1, 2),
                   labels = c("Male", "Female"))



# THE RACE WAS TOO DETAILED AND UNFORTUNATELY I HAD TO SIMPLIFY IT 

# Clean Hispanic column first (remove NIU)
raw_data$HISPAN <- ifelse(raw_data$HISPAN == 9999, NA, raw_data$HISPAN)

# We make a new varable RACE4 - Custom simplified race/ethnicity for analysis
raw_data$RACE4 <- case_when(
  !is.na(raw_data$HISPAN) & raw_data$HISPAN != 0100 ~ "Hispanic/Latino",                # From HISPAN
  raw_data$RACE == 0100 ~ "White",                                                  # White only
  raw_data$RACE == 0110 ~ "Black/African American",                                 # Black only
  raw_data$RACE == 0131 ~ "Asian",                                                  # Asian only
  TRUE ~ "Other / Mixed"                                                        # Everything else
)

# Convert to factor for ordered graphing
raw_data$RACE4 <- factor(raw_data$RACE4,
                     levels = c("White", "Black/African American", "Asian", "Hispanic/Latino", "Other / Mixed"))


# EDUC - Highest level of school completed or degree received
# Replace NIU (99) with NA

raw_data$EDUC[raw_data$EDUC == 999] <- NA 

raw_data$EDUC <- case_when(
    raw_data$EDUC %in% c(010, 011, 012, 013, 014, 015, 016, 017) ~ "Less than HS diploma",
    raw_data$EDUC %in% c(020, 021) ~ "HS diploma, no college",
    raw_data$EDUC %in% c(030, 031, 032) ~ "Some college",
    raw_data$EDUC %in% c(040, 041, 042, 043) ~ "College degree +",
    TRUE ~ NA_character_ # Assign NA to any other values, including the original 999
)

# Convert EDUC to a factor with specified levels
raw_data$EDUC <- factor(raw_data$EDUC,
                                        levels = c("Less than HS diploma", "HS diploma, no college",
                                                             "Some college", "College degree +"))


# MARST - Marital status
raw_data$MARST[raw_data$MARST == 99] <- NA

raw_data$MARST <- factor(raw_data$MARST,
                     levels = c(1, 2, 3, 4, 5, 6),
                     labels = c("Married - spouse present",
                                "Married - spouse absent",
                                "Widowed",
                                "Divorced",
                                "Separated",
                                "Never married"))


# EMPSTAT - Labor force status
raw_data$EMPSTAT[raw_data$EMPSTAT == 99] <- NA

# Convert EMPSTAT to a labeled factor
raw_data$EMPSTAT <- factor(raw_data$EMPSTAT,
                       levels = c(1, 2, 3, 4, 5),
                       labels = c("Employed - at work",
                                  "Employed - absent",
                                  "Unemployed - on layoff",
                                  "Unemployed - looking",
                                  "Not in labor force"))


# MULTJOBS - Has more than one job
raw_data$MULTJOBS[raw_data$MULTJOBS == 99] <- NA

# Convert MULTJOBS to a labeled factor
raw_data$MULTJOBS <- factor(raw_data$MULTJOBS,
                        levels = c(0, 1),
                        labels = c("No", "Yes"))



# WHYABSNT - Reason for absence
raw_data$WHYABSNT[raw_data$WHYABSNT == 999] <- NA

# Convert WHYABSNT to a labeled factor
raw_data$WHYABSNT <- factor(raw_data$WHYABSNT,
                        levels = 1:14, # Levels 1 through 14
                        labels = c("Layoff",
                                   "Slack work/business conditions",
                                   "Waiting for new job",
                                   "Vacation/personal days",
                                   "Illness/injury/medical",
                                   "Childcare problems",
                                   "Family/personal obligation (other)",
                                   "Maternity/paternity leave",
                                   "Labor dispute",
                                   "Weather affected job",
                                   "School/training",
                                   "Civic/military duty",
                                   "Does not work in business",
                                   "Family/personal obligation (other #2)"))




# FULLPART - Full/part time - Recode NIU (99) to NA before converting to factor
raw_data$FULLPART[raw_data$FULLPART == 99] <- NA

# Convert FULLPART to a labeled factor
raw_data$FULLPART <- factor(raw_data$FULLPART,
                        levels = c(1, 2),
                        labels = c("Full time", "Part time"))



# UHRSWORKT - Hours worked per week (clean 9995 and 9999)
raw_data$UHRSWORKT[raw_data$UHRSWORKT %in% c(9995, 9999)] <- NA

# Clean EARNWEEK - Weekly earnings (clean 99999)
raw_data$EARNWEEK[raw_data$EARNWEEK >= 99999] <- NA

# Clean SPEARNWEEK - Spouse's weekly earnings (clean 99999)
raw_data$SPEARNWEEK[raw_data$SPEARNWEEK >= 99999] <- NA


# Recode SPOUSEPRES: presence of spouse or partner
raw_data$SPOUSEPRES[raw_data$SPOUSEPRES == 99] <- NA

# Convert SPOUSEPRES to a labeled factor
raw_data$SPOUSEPRES <- factor(raw_data$SPOUSEPRES,
                          levels = c(1, 2, 3),
                          labels = c("Spouse present",
                                     "Unmarried partner present",
                                     "No spouse/partner present"))


# Clean SPAGE - Age of spouse/partner (996, 997, 998, 999 to NA)
raw_data$SPAGE[raw_data$SPAGE %in% c(996, 997, 998, 999)] <- NA

# Convert SPSEX to a labeled factor
raw_data$SPSEX[raw_data$SPSEX == 99] <- NA

# Convert SPSEX to a labeled factor
raw_data$SPSEX <- factor(raw_data$SPSEX,
                     levels = c(1, 2),
                     labels = c("Male", "Female"))

# Clean SPHISPAN - Hispanic/Latino status of spouse/partner
raw_data$sp_race4 <- case_when(
    !is.na(raw_data$SPHISPAN) & raw_data$SPHISPAN >= 200 & raw_data$SPHISPAN <= 299 ~ "Hispanic/Latino",
    raw_data$SPRACE == 0100 ~ "White",
    raw_data$SPRACE == 0110 ~ "Black/African American",
    raw_data$SPRACE %in% c(0130, 0131, 0132) ~ "Asian",  # Asian or Pacific Islander
    TRUE ~ "Other / Mixed"
)

# Convert to factor for graphing (ordered levels)
raw_data$sp_race4 <- factor(raw_data$sp_race4,
                                                levels = c("Hispanic/Latino", "White", "Black/African American", "Asian", "Other / Mixed"))



# Clean SPEDUC - Education level of spouse/partner
raw_data$SPEDUC[raw_data$SPEDUC %in% c(998, 999)] <- NA

# Clean SPEDUC - Education level of spouse/partner
raw_data$SPEDUC <- case_when(
    raw_data$SPEDUC %in% c(010, 011, 012, 013, 014, 015, 016, 017) ~ "Less than HS diploma",
    raw_data$SPEDUC %in% c(020, 021) ~ "HS diploma, no college",
    raw_data$SPEDUC %in% c(030, 031, 032) ~ "Some college",
    raw_data$SPEDUC %in% c(040, 041, 042, 043) ~ "College degree +",
    TRUE ~ NA_character_ # Handles any remaining values, including original NAs
)

# Convert to factor for graphing
raw_data$SPEDUC <- factor(raw_data$SPEDUC,
                                            levels = c("Less than HS diploma", "HS diploma, no college",
                                                                 "Some college", "College degree +"))



# Clean SPEMPSTAT - Employment status of spouse/partner
raw_data$SPEMPSTAT[raw_data$SPEMPSTAT %in% c(96, 97, 98, 99)] <- NA

raw_data$SPEMPSTAT <- case_when(
    raw_data$SPEMPSTAT == 01 ~ "Employed - at work",
    raw_data$SPEMPSTAT == 02 ~ "Employed - not at work",
    raw_data$SPEMPSTAT == 03 ~ "Not employed",
    raw_data$SPEMPSTAT == 04 ~ "Retired",
    raw_data$SPEMPSTAT %in% c(05, 06) ~ "Unable to work",
    TRUE ~ NA_character_ # Handles any remaining values, including original NAs
)

# Convert SPEMPSTAT to a factor
raw_data$SPEMPSTAT <- factor(raw_data$SPEMPSTAT,
                                                 levels = c("Employed - at work",
                                                                        "Employed - not at work",
                                                                        "Not employed",
                                                                        "Retired",
                                                                        "Unable to work"))




#  Clean SPUSUALHRS - Usual hours worked per week of spouse/partner
raw_data$SPUSUALHRS[raw_data$SPUSUALHRS %in% c(995, 999)] <- NA


# DIFFANY - Any difficulty (hearing, vision, cognitive, ambulatory, self-care, independent living)
raw_data$DIFFANY[raw_data$DIFFANY == 99] <- NA

raw_data$DIFFANY <- factor(raw_data$DIFFANY,
                           levels = c(1, 2),
                           labels = c("No difficulty", "Has difficulty"))



# ECPRIOR - Prior elder care provided
raw_data$ECPRIOR[raw_data$ECPRIOR %in% c(96, 97, 99)] <- NA

# Convert ECPRIOR to a labeled factor
raw_data$ECPRIOR <- factor(raw_data$ECPRIOR,
                            levels = c(0, 1),
                            labels = c("No", "Yes"))




# AFTER REDEFINING THE RACE WE CAN GET RID OF THE OLD COLUMNS

raw_data <- raw_data %>% select(-RACE, -HISPAN, -ASIAN, -SPRACE, -SPHISPAN)


# saving the cleaned data to a new csv file
write.csv(raw_data, "ATUS LABLED DATA.csv", row.names = FALSE)


####################################################################################
# PHHASE 2: EDA
####################################################################################




# the data is now labled and we dont have any coded variables
# importing the labled data

data <- read.csv("ATUS LABLED DATA.csv")


# Checking for missing values 

missing_counts <- colSums(is.na(data))
print(missing_counts)


# Get dimensions

print(dim(data))
summary(data)



# As mentioned in the report, based on existing literature, we will be using the following variables for defining **unpaid_labor**

data <- data %>%
    mutate(unpaid_labor = BLS_CAREHH + BLS_CARENHH + BLS_HHACT + BLS_PURCH)



# Calculate usual hours worked per week in minutes per day
# Since we also have BLS_WORK later (not mentioned in the report), we will compare them and will land on deciding to use BLS_WORK


data <- data %>%
    mutate(UHRSWORKT_DAILY_MIN = (UHRSWORKT / 7) * 60)





# Taking a look at our variables of interest (TIME USE) (DEMOGRAPHIC VARIABLES - CATEGORICAL AND NUMERICAL)

bls_time_use_variables <- c("unpaid_labor", "BLS_COMM", "BLS_EDUC", "BLS_FOOD", "BLS_LEIS", "BLS_PCARE", "BLS_SOCIAL", "BLS_WORK")

categorical_demographic_vars <- list(
  "REGION", 
  "FAMINCOME", 
  "HHTENURE", 
  "HOUSETYPE", 
  "HH_CHILD", 
  "DAY", 
  "SEX", 
  "RACE4", 
  "EDUC", 
  "MARST", 
  "EMPSTAT", 
  "MULTJOBS", 
  "FULLPART", 
  "SPOUSEPRES", 
  "SPSEX", 
  "sp_race4", 
  "SPEDUC", 
  "SPEMPSTAT", 
  "DIFFANY", 
  "ECPRIOR"
)

numerical_demographic_vars <- list(
  "HH_SIZE", 
  "HH_NUMKIDS", 
  "HH_NUMADULTS", 
  "AGEYCHILD", 
  "AGE", 
  "UHRSWORKT", 
  "EARNWEEK", 
  "SPEARNWEEK", 
  "SPAGE", 
  "SPUSUALHRS", 
  "UHRSWORKT_DAILY_MIN" 
)



# WEIGHTS
# The ATUS uses a complex system of weights to ensure the sample represents the population accurately, so we need to use svydesign


des <- svydesign(ids = ~1, weights = ~WT06, data = data)

# a function for weighted table of demographics


weighted_prop_table <- function(var, design) {
  fmla <- as.formula(paste0("~", var))
  tbl <- svytable(fmla, design)
  pct <- prop.table(tbl)
  data.frame(
    Category = names(tbl),
    WeightedCount = as.numeric(tbl),
    WeightedPercent = round(100 * as.numeric(pct), 1)
  )
}


# List of demographic variables
dem_vars <- c("SEX", "RACE4", "HH_CHILD", "MARST", "EDUC", "FAMINCOME")

# Building and saving tables for each demographic variable


create_and_save_gt_table <- function(var_name, design) {
  prop_tab <- weighted_prop_table(var_name, design)
  prop_tab_gt <- gt(prop_tab) %>%
    tab_header(
      title = paste(var_name, "Distribution (Weighted)")
    ) %>%
    fmt_number(
      columns = vars(WeightedCount),
      decimals = 0,
      use_seps = TRUE
    ) %>%
    fmt_number(
      columns = vars(WeightedPercent),
      decimals = 1
    ) %>%
    cols_label(
      Category = var_name,
      WeightedCount = "Weighted N",
      WeightedPercent = "Weighted %"
    )
  filename <- paste0(tolower(var_name), "_tab.png")
  gtsave(prop_tab_gt, filename = filename)
}
lapply(dem_vars, create_and_save_gt_table, design = des)



#PLOTS
#PLOT 1 -donus plot for major variables


create_weighted_donut_plot <- function(data, var_name, title, weight_var = "WT06") {
  var_formula <- as.formula(paste0("~", var_name))
  weighted_counts <- svytable(var_formula, des)
    df <- data.frame(
    category = names(weighted_counts),
    count = as.numeric(weighted_counts)
  ) %>%
    filter(!is.na(category)) %>%
    mutate(
      percentage = count / sum(count) * 100,
      ymax = cumsum(percentage),
      ymin = c(0, head(ymax, -1)),
      label = paste0(round(percentage, 1), "%")
    )
  
    ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(0, 4)) +
    theme_minimal() +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_blank() 
    ) +
    labs(
      title = title,
      fill = var_name
    )
}


p1 <- create_weighted_donut_plot(data, "SEX", "Distribution by Gender")
p2 <- create_weighted_donut_plot(data, "FAMINCOME", "Distribution by Family Income") 
p3 <- create_weighted_donut_plot(data, "HH_CHILD", "Households with Children")
p4 <- create_weighted_donut_plot(data, "MARST", "Marital Status")
p5 <- create_weighted_donut_plot(data, "EDUC", "Education Level")
p6 <- create_weighted_donut_plot(data, "RACE4", "Race/Ethnicity")

combined_plot <- (p1 | p6) / (p3 | p4) / (p5 | p2) +
  plot_annotation(
    title = "Demographic Distributions (Weighted)",
    subtitle = "Based on American Time Use Survey",
    caption = "Source: ATUS data with survey weights applied",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1)
    )
  )


# displaying and saving the plot 
print(combined_plot)
ggsave("demographic_distributions.jpg", combined_plot, width = 14, height = 20, dpi = 300)




#PLOT2

calculate_weighted_means_by_sex <- function(var_name, design) {
  fmla <- as.formula(paste0("~", var_name))
  means_by_sex <- svyby(fmla, ~SEX, design, svymean, na.rm = TRUE, vartype = "se")
  current_names <- names(means_by_sex)
  
  if (length(current_names) >= 3) {
    mean_col_index <- which(!current_names %in% c("SEX") & !startsWith(current_names, "se"))
    se_col_index <- which(startsWith(current_names, "se"))
    
    names(means_by_sex)[mean_col_index] <- "MeanTime"
    names(means_by_sex)[se_col_index] <- "SE"
  } else {
    names(means_by_sex) <- c("SEX", "MeanTime", "SE")[1:ncol(means_by_sex)]
  }
  
  return(as.data.frame(means_by_sex))
}

create_gender_comparison_plot <- function(means_df, var_name) {
    title_var <- gsub("BLS_", "", var_name) 
    title_var <- gsub("_", " ", title_var)
    if (var_name == "unpaid_labor") {
        title_var <- "Unpaid Labor"
    } else {
        title_var <- tools::toTitleCase(tolower(title_var))
    }


p <- ggplot(means_df, aes(x = SEX, y = MeanTime, fill = SEX)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = MeanTime - 1.96 * SE, ymax = MeanTime + 1.96 * SE),
                                width = 0.25, position = position_dodge(0.9)) +
    scale_fill_brewer(palette = "Accent") + 
    labs(
        title = paste("Mean Time Spent on", title_var), 
        x = NULL,
        y = "Mean Minutes per Day",
        fill = "Gender",
        caption = NULL
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size=10), 
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
    )
return(p)
}


plot_list_gender <- list() 
for (variable in bls_time_use_variables) {
    cat("working:", variable, "\n")
    means_data <- calculate_weighted_means_by_sex(variable, des)
    if (!is.null(means_data)) {
        plot <- create_gender_comparison_plot(means_data, variable)
        plot_list_gender[[variable]] <- plot 
        file_name <- paste0("gender_comparison_plots/", tolower(variable), "_gender_comparison.png")
        ggsave(file_name, plot, width = 5, height = 4, dpi = 300) 
        cat("Saved plot:", file_name, "\n")
    } else {
        cat("Skipping plot for", variable, "due to issues in mean calculation.\n")
    }
}


n_plots <- length(plot_list_gender)
n_cols <- 2
n_rows <- ceiling(n_plots / n_cols)

combined_gender_plots <- wrap_plots(plot_list_gender, ncol = n_cols, nrow = n_rows) +
  plot_annotation(
    title = "Comparison of Weighted Mean Time Use by Gender Across Activities",
    subtitle = "Minutes per Day (ATUS)",
    caption = "Source: ATUS data with survey weights (WT06). Error bars represent 95% confidence intervals."
  ) & theme(plot.margin = margin(10, 10, 10, 10))
print(combined_gender_plots)
ggsave("gender_comparison_plots/combined_gender_time_use.png", combined_gender_plots, width = 10, height = n_rows * 4, dpi = 300)





#VIOLIN PLOTS
# unpaid labor by having kids or not and gender


plot_data_violin <- data %>%
    filter(!is.na(unpaid_labor) & !is.na(SEX) & !is.na(HH_CHILD)) %>%
    mutate(
        Parenthood = factor(HH_CHILD, levels = c("No", "Yes"), labels = c("No Children < 18", "Children < 18 Present"))
    )

violin_plot_unpaid_labor <- ggplot(plot_data_violin, aes(x = SEX, y = unpaid_labor, fill = SEX)) +
    geom_violin(trim = FALSE, alpha = 0.8) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.7) +
    facet_wrap(~ Parenthood) +
    scale_fill_brewer(palette = "Accent") +
    scale_y_continuous(limits = c(0, quantile(plot_data_violin$unpaid_labor, 0.95, na.rm = TRUE))) +
    labs(
        title = "Distribution of Daily Unpaid Labor Time",
        subtitle = "By Gender and Presence of Children Under 18 in Household",
        x = "Gender",
        y = "Minutes Spent on Unpaid Labor per Day",
        fill = "Gender",
        caption = "Source: ATUS Data. Y-axis capped at 95th percentile for visualization."
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"), 
        legend.position = "bottom" 
    )

print(violin_plot_unpaid_labor)
ggsave("gender_comparison_plots/unpaid_labor_violin_gender_parenthood.jpg", violin_plot_unpaid_labor, width = 10, height = 7, dpi = 300)


#summary table

mean_stats_parenthood <- svyby(~unpaid_labor, ~SEX + HH_CHILD, des, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
names(mean_stats_parenthood)[names(mean_stats_parenthood) == "unpaid_labor"] <- "Mean"
names(mean_stats_parenthood)[startsWith(names(mean_stats_parenthood), "se")] <- "SE"

median_stats_parenthood <- svyby(~unpaid_labor, ~SEX + HH_CHILD, des, svyquantile,
                                 quantiles = 0.5, keep.var = FALSE, ci = FALSE, na.rm = TRUE, drop.empty.groups = FALSE)
if(ncol(median_stats_parenthood) >= 3) names(median_stats_parenthood)[3] <- "Median"

p90_stats_parenthood <- svyby(~unpaid_labor, ~SEX + HH_CHILD, des, svyquantile,
                              quantiles = 0.9, keep.var = FALSE, ci = FALSE, na.rm = TRUE, drop.empty.groups = FALSE)
if(ncol(p90_stats_parenthood) >= 3) names(p90_stats_parenthood)[3] <- "P90"

parenthood_summary <- mean_stats_parenthood %>%
  select(SEX, HH_CHILD, Mean) %>%
  left_join(median_stats_parenthood %>% select(SEX, HH_CHILD, Median), by = c("SEX", "HH_CHILD")) %>%
  left_join(p90_stats_parenthood %>% select(SEX, HH_CHILD, P90), by = c("SEX", "HH_CHILD")) %>%
  filter(!is.na(SEX) & !is.na(HH_CHILD)) %>%
  arrange(HH_CHILD, SEX) 

unpaid_summary_parenthood_gt <- gt(parenthood_summary, groupname_col = "HH_CHILD") %>% 
  tab_header(
    title = "Unpaid Work by Gender and Presence of Children (<18)",
    subtitle = "Minutes per day, weighted estimates"
  ) %>%
  fmt_number(
    columns = c(Mean, Median, P90),
    decimals = 1
  ) %>%
  cols_label(
    SEX = "Gender",
    Mean = "Mean",
    Median = "Median",
    P90 = "90th Percentile"
  ) %>%
   tab_options(
      column_labels.font.weight = "bold"
   ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).")

print(unpaid_summary_parenthood_gt)
gtsave(unpaid_summary_parenthood_gt, "unpaid_work_by_gender_parenthood.png")





# summary table for unpaid work by gender 

mean_stats <- svyby(~unpaid_labor, ~SEX, des, svymean, na.rm = TRUE)
names(mean_stats)[2:3] <- c("Mean", "SE")
median_stats <- svyby(~unpaid_labor, ~SEX, des, svyquantile,
                      quantiles = 0.5, keep.var = FALSE, na.rm = TRUE)
names(median_stats)[2] <- "Median" 
p90_stats <- svyby(~unpaid_labor, ~SEX, des, svyquantile,
                   quantiles = 0.9, keep.var = FALSE, na.rm = TRUE)
names(p90_stats)[2] <- "P90" 
gender_summary <- mean_stats %>%
  select(SEX, Mean) %>%
  left_join(median_stats %>% select(SEX, Median), by = "SEX") %>%
  left_join(p90_stats %>% select(SEX, P90), by = "SEX")
unpaid_summary_gt <- gt(gender_summary) %>%
  tab_header(
    title = "Unpaid Work by Gender",
    subtitle = "Minutes per day, weighted estimates"
  ) %>%
  fmt_number(
    columns = c(Mean, Median, P90),
    decimals = 1 
  ) %>%
  cols_label(
    SEX = "Gender",
    Mean = "Mean",
    Median = "Median",
    P90 = "90th Percentile" 
  ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).")

print(unpaid_summary_gt)
gtsave(unpaid_summary_gt, "unpaid_work_by_gender_no_zero.png")







#unpaid labor by gender and employment status

plot_data_violin_emp <- data %>%
    filter(!is.na(unpaid_labor) & !is.na(SEX) & !is.na(EMPSTAT)) %>%
    mutate(EMPSTAT_short = fct_recode(EMPSTAT,
                                      "Employed-Work" = "Employed - at work",
                                      "Employed-Absent" = "Employed - absent",
                                      "Unemp-Layoff" = "Unemployed - on layoff",
                                      "Unemp-Looking" = "Unemployed - looking",
                                      "Not in Labor Force" = "Not in labor force"
                                      ))

y_limit_emp <- quantile(plot_data_violin_emp$unpaid_labor, 0.95, na.rm = TRUE)

violin_plot_unpaid_labor_emp <- ggplot(plot_data_violin_emp, aes(x = SEX, y = unpaid_labor, fill = SEX)) +
    geom_violin(trim = FALSE, alpha = 0.8) +
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.7) + 
    facet_wrap(~ EMPSTAT_short, ncol = 3) + 
    scale_fill_brewer(palette = "Accent") + 
    coord_cartesian(ylim = c(0, y_limit_emp)) +
    labs(
        title = "Distribution of Daily Unpaid Labor Time",
        subtitle = "By Gender and Employment Status (Sample Distribution)",
        x = "Gender",
        y = "Minutes Spent on Unpaid Labor per Day",
        fill = "Gender",
        caption = paste("Source: ATUS Data. Y-axis capped at", round(y_limit_emp), "mins (95th percentile) for visualization.")
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold", size = 8),
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 45, hjust = 1)
    )

print(violin_plot_unpaid_labor_emp)
ggsave("gender_comparison_plots/unpaid_labor_violin_gender_empstat.jpg", violin_plot_unpaid_labor_emp, width = 12, height = 8, dpi = 300)




#summary table for gender and employment status

mean_stats_emp <- svyby(~unpaid_labor, ~EMPSTAT, des, svymean, na.rm = TRUE)
mean_col_index_emp <- which(!names(mean_stats_emp) %in% c("EMPSTAT") & !startsWith(names(mean_stats_emp), "se"))
se_col_index_emp <- which(startsWith(names(mean_stats_emp), "se"))
if(length(mean_col_index_emp) == 1) names(mean_stats_emp)[mean_col_index_emp] <- "Mean"
if(length(se_col_index_emp) == 1) names(mean_stats_emp)[se_col_index_emp] <- "SE"

median_stats_emp <- svyby(~unpaid_labor, ~EMPSTAT, des, svyquantile,
                          quantiles = 0.5, keep.var = FALSE, ci = FALSE, na.rm = TRUE)
if(ncol(median_stats_emp) >= 2) names(median_stats_emp)[2] <- "Median"

p90_stats_emp <- svyby(~unpaid_labor, ~EMPSTAT, des, svyquantile,
                       quantiles = 0.9, keep.var = FALSE, ci = FALSE, na.rm = TRUE)
if(ncol(p90_stats_emp) >= 2) names(p90_stats_emp)[2] <- "P90"


empstat_summary <- mean_stats_emp %>%
  select(EMPSTAT, Mean) %>%
  left_join(median_stats_emp %>% select(EMPSTAT, Median), by = "EMPSTAT") %>%
  left_join(p90_stats_emp %>% select(EMPSTAT, P90), by = "EMPSTAT") %>%
  filter(!is.na(EMPSTAT))

unpaid_summary_empstat_gt <- gt(empstat_summary) %>%
  tab_header(
    title = "Unpaid Work by Employment Status",
    subtitle = "Minutes per day, weighted estimates"
  ) %>%
  fmt_number(
    columns = c(Mean, Median, P90),
    decimals = 1 
  ) %>%
  cols_label(
    EMPSTAT = "Employment Status",
    Mean = "Mean",
    Median = "Median",
    P90 = "90th Percentile" 
  ) %>%
   tab_options(
      column_labels.font.weight = "bold" 
   ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).")

print(unpaid_summary_empstat_gt)
gtsave(unpaid_summary_empstat_gt, "unpaid_work_by_empstat.png") 


 



#Unpaid Labor by Gender and Full or Part-Time Status 

plot_data_violin_ftpt <- data %>%
    filter(!is.na(unpaid_labor) & !is.na(SEX) & !is.na(FULLPART))
y_limit_ftpt <- quantile(plot_data_violin_ftpt$unpaid_labor, 0.95, na.rm = TRUE)

violin_plot_unpaid_labor_ftpt <- ggplot(plot_data_violin_ftpt, aes(x = SEX, y = unpaid_labor, fill = SEX)) +
    geom_violin(trim = FALSE, alpha = 0.8) + 
    geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.7) + 
    facet_wrap(~ FULLPART) + 
    scale_fill_brewer(palette = "Accent") +
    coord_cartesian(ylim = c(0, y_limit_ftpt)) +
    labs(
        title = "Distribution of Daily Unpaid Labor Time",
        subtitle = "By Gender and Full/Part-Time Employment Status (Sample Distribution)",
        x = "Gender",
        y = "Minutes Spent on Unpaid Labor per Day",
        fill = "Gender",
        caption = paste("Source: ATUS Data. Y-axis capped at", round(y_limit_ftpt), "mins (95th percentile) for visualization.")
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"), 
        legend.position = "bottom"
    )

print(violin_plot_unpaid_labor_ftpt)
ggsave("gender_comparison_plots/unpaid_labor_violin_gender_fullpart.jpg", violin_plot_unpaid_labor_ftpt, width = 10, height = 7, dpi = 300)


#summary by Gender and Full or Part-Time Status

mean_stats_ftpt <- svyby(~unpaid_labor, ~FULLPART, des, svymean, na.rm = TRUE)
mean_col_index_ftpt <- which(!names(mean_stats_ftpt) %in% c("FULLPART") & !startsWith(names(mean_stats_ftpt), "se"))
se_col_index_ftpt <- which(startsWith(names(mean_stats_ftpt), "se"))
if(length(mean_col_index_ftpt) == 1) names(mean_stats_ftpt)[mean_col_index_ftpt] <- "Mean"
if(length(se_col_index_ftpt) == 1) names(mean_stats_ftpt)[se_col_index_ftpt] <- "SE"

median_stats_ftpt <- svyby(~unpaid_labor, ~FULLPART, des, svyquantile,
                           quantiles = 0.5, keep.var = FALSE, ci = FALSE, na.rm = TRUE) 
if(ncol(median_stats_ftpt) >= 2) names(median_stats_ftpt)[2] <- "Median"

p90_stats_ftpt <- svyby(~unpaid_labor, ~FULLPART, des, svyquantile,
                        quantiles = 0.9, keep.var = FALSE, ci = FALSE, na.rm = TRUE)
if(ncol(p90_stats_ftpt) >= 2) names(p90_stats_ftpt)[2] <- "P90"

ftpt_summary <- mean_stats_ftpt %>%
  select(FULLPART, Mean) %>%
  left_join(median_stats_ftpt %>% select(FULLPART, Median), by = "FULLPART") %>%
  left_join(p90_stats_ftpt %>% select(FULLPART, P90), by = "FULLPART") %>%
  filter(!is.na(FULLPART)) 
unpaid_summary_ftpt_gt <- gt(ftpt_summary) %>%
  tab_header(
    title = "Unpaid Work by Full/Part-Time Status",
    subtitle = "Minutes per day, weighted estimates"
  ) %>%
  fmt_number(
    columns = c(Mean, Median, P90),
    decimals = 1 
  ) %>%
  cols_label(
    FULLPART = "Employment Status",
    Mean = "Mean",
    Median = "Median",
    P90 = "90th Percentile" 
  ) %>%
   tab_options(
      column_labels.font.weight = "bold" 
   ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).") 

print(unpaid_summary_ftpt_gt)
gtsave(unpaid_summary_ftpt_gt, "unpaid_work_by_fullpart.png") 




#Unpaid Labor by Age and Gender

plot_data_age_gender <- data %>%
    filter(!is.na(unpaid_labor) & !is.na(AGE) & !is.na(WT06) & !is.na(SEX) & AGE >= 15)
plot_unpaid_labor_age_gender <- ggplot(plot_data_age_gender, aes(x = AGE, y = unpaid_labor, color = SEX, fill = SEX)) +
    geom_smooth(aes(weight = WT06), method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, alpha = 0.2) +
    scale_color_brewer(palette = "Accent", name = "Gender") +
    scale_fill_brewer(palette = "Accent", name = "Gender") +  
    labs(
        title = "Trend in Daily Unpaid Labor Time by Age and Gender",
        subtitle = "Weighted estimate using GAM smoother",
        x = "Age",
        y = "Mean Minutes Spent on Unpaid Labor per Day",
        caption = "Source: ATUS Data with survey weights (WT06). Shaded area represents 95% CI."
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
    )

print(plot_unpaid_labor_age_gender)
ggsave("gender_comparison_plots/unpaid_labor_by_age_gender_smooth.jpg", plot_unpaid_labor_age_gender, width = 9, height = 7, dpi = 300)


# summary tabele by Age and Gender (I had to make age intervals)

if (!"AGE_GROUP" %in% names(data)) {
    data <- data %>%
      mutate(AGE_GROUP = cut(AGE,
                             breaks = c(14, 24, 34, 44, 54, 64, Inf), 
                             labels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
                             right = TRUE, 
                             include.lowest = TRUE)) 

    des <- update(des, AGE_GROUP = data$AGE_GROUP)
}


mean_stats_age_gender <- svyby(~unpaid_labor, ~AGE_GROUP + SEX, des, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
names(mean_stats_age_gender)[names(mean_stats_age_gender) == "unpaid_labor"] <- "Mean"
names(mean_stats_age_gender)[startsWith(names(mean_stats_age_gender), "se")] <- "SE"


median_stats_age_gender <- svyby(~unpaid_labor, ~AGE_GROUP + SEX, des, svyquantile,
                                 quantiles = 0.5, keep.var = FALSE, ci = FALSE, na.rm = TRUE, drop.empty.groups = FALSE)
if(ncol(median_stats_age_gender) >= 3) names(median_stats_age_gender)[3] <- "Median"

p90_stats_age_gender <- svyby(~unpaid_labor, ~AGE_GROUP + SEX, des, svyquantile,
                              quantiles = 0.9, keep.var = FALSE, ci = FALSE, na.rm = TRUE, drop.empty.groups = FALSE)
if(ncol(p90_stats_age_gender) >= 3) names(p90_stats_age_gender)[3] <- "P90"

age_gender_summary <- mean_stats_age_gender %>%
  select(AGE_GROUP, SEX, Mean) %>%
  left_join(median_stats_age_gender %>% select(AGE_GROUP, SEX, Median), by = c("AGE_GROUP", "SEX")) %>%
  left_join(p90_stats_age_gender %>% select(AGE_GROUP, SEX, P90), by = c("AGE_GROUP", "SEX")) %>%
  filter(!is.na(AGE_GROUP) & !is.na(SEX)) %>% 
  arrange(AGE_GROUP, SEX) 

unpaid_summary_age_gender_gt <- gt(age_gender_summary, groupname_col = "SEX") %>% 
  tab_header(
    title = "Unpaid Work by Age Group and Gender",
    subtitle = "Minutes per day, weighted estimates"
  ) %>%
  fmt_number(
    columns = c(Mean, Median, P90),
    decimals = 1
  ) %>%
  cols_label(
    AGE_GROUP = "Age Group",
    Mean = "Mean",
    Median = "Median",
    P90 = "90th Percentile"
  ) %>%
   tab_options(
      column_labels.font.weight = "bold"
   ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).")

print(unpaid_summary_age_gender_gt)
gtsave(unpaid_summary_age_gender_gt, "unpaid_work_by_age_group_gender.png")





#unpaid labor by race - bar plot

mean_stats_race_gender <- svyby(~unpaid_labor, ~RACE4 + SEX, des, svymean, na.rm = TRUE, drop.empty.groups = FALSE)

if (!is.null(mean_stats_race_gender) && nrow(mean_stats_race_gender) > 0) {
    names(mean_stats_race_gender)[names(mean_stats_race_gender) == "unpaid_labor"] <- "Mean"
    names(mean_stats_race_gender)[startsWith(names(mean_stats_race_gender), "se")] <- "SE"
    mean_stats_race_gender <- mean_stats_race_gender %>% filter(!is.na(RACE4) & !is.na(SEX))

    plot_unpaid_labor_race_gender <- ggplot(mean_stats_race_gender, aes(x = RACE4, y = Mean, fill = SEX)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        geom_errorbar(aes(ymin = Mean - 1.96 * SE, ymax = Mean + 1.96 * SE),
                      width = 0.25, position = position_dodge(width = 0.9)) +
        scale_fill_brewer(palette = "Accent", name = "Gender") +
        labs(
            title = "Mean Daily Unpaid Labor Time by Race and Gender",
            subtitle = "Weighted estimate (Minutes per Day)",
            x = "Race/Ethnicity",
            y = "Mean Minutes per Day",
            caption = "Source: ATUS Data with survey weights (WT06). Error bars represent 95% CI."
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
        )

    print(plot_unpaid_labor_race_gender)
    ggsave("gender_comparison_plots/unpaid_labor_by_race_gender_bar.jpg", plot_unpaid_labor_race_gender, width = 10, height = 7, dpi = 300)

}


#summary table for unpaid labor by race 

if (!is.null(mean_stats_race_gender) && nrow(mean_stats_race_gender) > 0) {
    median_stats_race_gender <- svyby(~unpaid_labor, ~RACE4 + SEX, des, svyquantile,
                                     quantiles = 0.5, keep.var = FALSE, ci = FALSE, na.rm = TRUE, drop.empty.groups = FALSE)
    if(ncol(median_stats_race_gender) >= 3) names(median_stats_race_gender)[3] <- "Median"
    p90_stats_race_gender <- svyby(~unpaid_labor, ~RACE4 + SEX, des, svyquantile,
                                  quantiles = 0.9, keep.var = FALSE, ci = FALSE, na.rm = TRUE, drop.empty.groups = FALSE)
    if(ncol(p90_stats_race_gender) >= 3) names(p90_stats_race_gender)[3] <- "P90"
    race_gender_summary <- mean_stats_race_gender %>%
      select(RACE4, SEX, Mean) %>%
      left_join(median_stats_race_gender %>% select(RACE4, SEX, Median), by = c("RACE4", "SEX")) %>%
      left_join(p90_stats_race_gender %>% select(RACE4, SEX, P90), by = c("RACE4", "SEX")) %>%
      filter(!is.na(RACE4) & !is.na(SEX)) %>% 
      arrange(RACE4, SEX) 
    unpaid_summary_race_gender_gt <- gt(race_gender_summary, groupname_col = "RACE4") %>%
      tab_header(
        title = "Unpaid Work by Race/Ethnicity and Gender",
        subtitle = "Minutes per day, weighted estimates"
      ) %>%
      fmt_number(
        columns = c(Mean, Median, P90),
        decimals = 1
      ) %>%
      cols_label(
        SEX = "Gender",
        Mean = "Mean",
        Median = "Median",
        P90 = "90th Percentile"
      ) %>%
       tab_options(
          column_labels.font.weight = "bold"
       ) %>%
      tab_source_note("Source: ATUS data with survey weights (WT06).")
    print(unpaid_summary_race_gender_gt)
    gtsave(unpaid_summary_race_gender_gt, "unpaid_work_by_race_gender.png")

} 



# now we calculate gender gap to get a better look 
# unpaid labor gap by race

if (!is.null(mean_stats_race_gender) && nrow(mean_stats_race_gender) > 0 && all(c("RACE4", "SEX", "Mean") %in% names(mean_stats_race_gender))) {
    gender_gap_data <- mean_stats_race_gender %>%
        select(RACE4, SEX, Mean) %>%
        pivot_wider(names_from = SEX, values_from = Mean) %>%
        { if (!"Male" %in% names(.)) mutate(., Male = NA) else . } %>%
        { if (!"Female" %in% names(.)) mutate(., Female = NA) else . } %>%
        mutate(GenderGap = Female - Male) %>%
        filter(!is.na(GenderGap)) 
    plot_gender_gap_race <- ggplot(gender_gap_data, aes(x = RACE4, y = GenderGap, fill = RACE4)) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
        scale_fill_brewer(palette = "Set2", guide = "none") +
        labs(
            title = "Gender Gap in Mean Daily Unpaid Labor Time",
            subtitle = "Difference (Female - Male) in Weighted Minutes per Day, by Race/Ethnicity",
            x = "Race/Ethnicity",
            y = "Gender Gap (Female - Male Minutes)",
            caption = "Source: ATUS Data with survey weights (WT06)."
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10),
            axis.text.x = element_text(angle = 45, hjust = 1) 
        )
    print(plot_gender_gap_race)
    ggsave("gender_comparison_plots/gender_gap_unpaid_labor_by_race.jpg", plot_gender_gap_race, width = 8, height = 6, dpi = 300)

} 


#summary table for gender gap

if (!is.null(gender_gap_data) && nrow(gender_gap_data) > 0 && all(c("RACE4", "GenderGap") %in% names(gender_gap_data))) {
    gender_gap_summary <- gender_gap_data %>%
        select(RACE4, GenderGap) %>%
        arrange(RACE4) 
    gender_gap_summary_gt <- gt(gender_gap_summary) %>%
      tab_header(
        title = "Gender Gap in Mean Daily Unpaid Labor Time by Race/Ethnicity",
        subtitle = "Difference (Female - Male) in weighted minutes per day"
      ) %>%
      fmt_number(
        columns = GenderGap,
        decimals = 1
      ) %>%
      cols_label(
        RACE4 = "Race/Ethnicity",
        GenderGap = "Gender Gap (Female - Male)"
      ) %>%
       tab_options(
          column_labels.font.weight = "bold" 
       ) %>%
      tab_source_note("Source: ATUS data with survey weights (WT06).")
    print(gender_gap_summary_gt)
    gtsave(gender_gap_summary_gt, "gender_gap_unpaid_labor_by_race.png") # Save as PNG

} 


# unpaid labor gap for interracial couples vs same race couples

partnered_data <- data %>%
    filter(SPOUSEPRES %in% c("Spouse present", "Unmarried partner present")) %>%
    filter(!is.na(RACE4) & !is.na(sp_race4)) %>% 
    mutate(
        CoupleRaceType = case_when(
            as.character(RACE4) == as.character(sp_race4) ~ "Same-Race Couple",
            as.character(RACE4) != as.character(sp_race4) ~ "Interracial Couple",
            TRUE ~ NA_character_ 
        )
    ) %>%
    filter(!is.na(CoupleRaceType)) 
if(nrow(partnered_data) == 0) {
    print("check the data -")
} else {

des_partnered <- svydesign(ids = ~1, weights = ~WT06, data = partnered_data)
des_partnered <- update(des_partnered, CoupleRaceType = partnered_data$CoupleRaceType)
mean_stats_couple_race <- svyby(~unpaid_labor, ~CoupleRaceType + SEX, des_partnered, svymean, na.rm = TRUE, drop.empty.groups = FALSE)

if (!is.null(mean_stats_couple_race) && nrow(mean_stats_couple_race) > 0) {
names(mean_stats_couple_race)[names(mean_stats_couple_race) == "unpaid_labor"] <- "Mean"
names(mean_stats_couple_race)[startsWith(names(mean_stats_couple_race), "se")] <- "SE"
mean_stats_couple_race <- mean_stats_couple_race %>% filter(!is.na(CoupleRaceType) & !is.na(SEX))

gender_gap_couple_race_data <- mean_stats_couple_race %>%
    select(CoupleRaceType, SEX, Mean) %>%
    pivot_wider(names_from = SEX, values_from = Mean) %>%
      { if (!"Male" %in% names(.)) mutate(., Male = NA) else . } %>%
      { if (!"Female" %in% names(.)) mutate(., Female = NA) else . } %>%
    mutate(GenderGap = Female - Male) %>%
    filter(!is.na(GenderGap))

if(nrow(gender_gap_couple_race_data) > 0) {

plot_gender_gap_couple_race <- ggplot(gender_gap_couple_race_data, aes(x = CoupleRaceType, y = GenderGap, fill = CoupleRaceType)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
    scale_fill_brewer(palette = "Accent", guide = "none") +
    labs(
        title = "Gender Gap in Mean Daily Unpaid Labor Time",
        subtitle = "Difference (Female - Male) by Couple Race Type (Weighted Minutes)",
        x = "Couple Race Type",
        y = "Gender Gap (Female - Male Minutes)",
        caption = "Source: ATUS Data (Partnered Individuals) with survey weights (WT06)."
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
    )

print(plot_gender_gap_couple_race)
ggsave("gender_comparison_plots/gender_gap_unpaid_labor_by_couple_race_type.jpg", plot_gender_gap_couple_race, width = 7, height = 6, dpi = 300)



#summary table
gender_gap_couple_race_summary_gt <- gt(gender_gap_couple_race_data %>% select(CoupleRaceType, GenderGap)) %>%
  tab_header(
    title = "Gender Gap in Mean Daily Unpaid Labor by Couple Race Type",
    subtitle = "Difference (Female - Male) in weighted minutes per day"
  ) %>%
  fmt_number(
    columns = GenderGap,
    decimals = 1
  ) %>%
  cols_label(
    CoupleRaceType = "Couple Race Type",
    GenderGap = "Gender Gap (Female - Male)"
  ) %>%
    tab_options(
      column_labels.font.weight = "bold"
    ) %>%
  tab_source_note("Source: ATUS data (Partnered Individuals) with survey weights (WT06).")

print(gender_gap_couple_race_summary_gt)
gtsave(gender_gap_couple_race_summary_gt, "gender_gap_unpaid_labor_by_couple_race_type.png")





#woemns unpaid labor by earnings

women_data_earnings <- data %>%
    filter(SEX == "Female", !is.na(unpaid_labor), !is.na(EARNWEEK), !is.na(WT06)) %>%
    mutate(
        EARN_BRACKET = cut(EARNWEEK,
                           breaks = c(-Inf, 249, 499, 749, 999, 1499, Inf),
                           labels = c("< $250", "$250-$499", "$500-$749", "$750-$999", "$1000-$1499", "$1500+"),
                           right = TRUE, 
                           include.lowest = TRUE) 
    ) %>%
    filter(!is.na(EARN_BRACKET)) 


unweighted_counts <- women_data_earnings %>%
    count(EARN_BRACKET, name = "N_Unweighted")

des_women_earnings <- svydesign(ids = ~1, weights = ~WT06, data = women_data_earnings)
des_women_earnings <- update(des_women_earnings, EARN_BRACKET = women_data_earnings$EARN_BRACKET)
mean_stats_women_earnings <- svyby(~unpaid_labor, ~EARN_BRACKET, des_women_earnings, svymean, na.rm = TRUE, drop.empty.groups = FALSE)

if (!is.null(mean_stats_women_earnings) && nrow(mean_stats_women_earnings) > 0) {
    names(mean_stats_women_earnings)[names(mean_stats_women_earnings) == "unpaid_labor"] <- "Mean"
    names(mean_stats_women_earnings)[startsWith(names(mean_stats_women_earnings), "se")] <- "SE"
    mean_stats_women_earnings <- mean_stats_women_earnings %>% filter(!is.na(EARN_BRACKET))

    summary_women_earnings <- mean_stats_women_earnings %>%
        left_join(unweighted_counts, by = "EARN_BRACKET")

plot_women_unpaid_labor_earnings <- ggplot(summary_women_earnings, aes(x = EARN_BRACKET, y = Mean, fill = EARN_BRACKET)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Mean - 1.96 * SE, ymax = Mean + 1.96 * SE),
                width = 0.25, position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "Accent", guide = "none") +
  labs(
      title = "Women's Mean Daily Unpaid Labor Time by Weekly Earnings",
      subtitle = "Weighted estimate (Minutes per Day)",
      x = "Weekly Earnings Bracket",
      y = "Mean Minutes per Day",
      caption = "Source: ATUS Data (Female Respondents) with survey weights (WT06). Error bars represent 95% CI."
  ) +
  theme_minimal() +
  theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(plot_women_unpaid_labor_earnings)
ggsave("gender_comparison_plots/women_unpaid_labor_by_earnings.jpg", plot_women_unpaid_labor_earnings, width = 9, height = 7, dpi = 300)



#summary table
women_earnings_summary_gt <- gt(summary_women_earnings %>% select(EARN_BRACKET, Mean, SE, N_Unweighted)) %>%
  tab_header(
    title = "Women's Mean Daily Unpaid Labor Time by Weekly Earnings",
    subtitle = "Weighted estimates and unweighted sample size"
  ) %>%
  fmt_number(
    columns = c(Mean, SE),
    decimals = 1
  ) %>%
  fmt_integer(
    columns = N_Unweighted
  ) %>%
  cols_label(
    EARN_BRACKET = "Weekly Earnings Bracket",
    Mean = "Mean (Minutes)",
    SE = "Std. Error",
    N_Unweighted = "N (Unweighted)"
  ) %>%
    tab_options(
      column_labels.font.weight = "bold"
    ) %>%
  tab_source_note("Source: ATUS data (Female Respondents) with survey weights (WT06).")

print(women_earnings_summary_gt)
gtsave(women_earnings_summary_gt, "women_unpaid_labor_by_earnings_summary.png")


#unpaid labor components

unpaid_components <- c("BLS_CAREHH", "BLS_CARENHH", "BLS_HHACT", "BLS_PURCH")
unpaid_component_labels <- c(
    "BLS_CAREHH" = "Care for Household Members",
    "BLS_CARENHH" = "Care for Non-Household Members",
    "BLS_HHACT" = "Household Activities",
    "BLS_PURCH" = "Purchasing Goods/Services"
)

formula_str <- paste("~", paste(unpaid_components, collapse = " + "))
fmla_components <- as.formula(formula_str)
mean_stats_components <- svyby(fmla_components, ~SEX, des, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
if (!is.null(mean_stats_components) && nrow(mean_stats_components) > 0) {

    plot_data_components <- mean_stats_components %>%
        select(SEX, all_of(unpaid_components)) %>%
        pivot_longer(cols = -SEX, names_to = "Component", values_to = "MeanTime") %>%
        filter(!is.na(SEX) & !is.na(MeanTime)) %>% 
        mutate(ComponentLabel = factor(recode(Component, !!!unpaid_component_labels),
                                       levels = unpaid_component_labels)) 
plot_unpaid_components_gender <- ggplot(plot_data_components, aes(x = SEX, y = MeanTime, fill = ComponentLabel)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_brewer(palette = "Set3", name = "Unpaid Labor Component") +
    labs(
        title = "Composition of Mean Daily Unpaid Labor Time by Gender",
        subtitle = "Weighted estimate (Minutes per Day)",
        x = "Gender",
        y = "Mean Minutes per Day",
        caption = "Source: ATUS Data with survey weights (WT06)."
    ) +
    theme_minimal() +
    theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom",
            legend.title = element_text(face="bold")
        ) +
        guides(fill = guide_legend(nrow = 2))

    print(plot_unpaid_components_gender)
    ggsave("gender_comparison_plots/unpaid_labor_components_stacked_gender.jpg", plot_unpaid_components_gender, width = 8, height = 7, dpi = 300)

} 






#paid vs unpaid labor

plot_data_paid_unpaid <- data %>%
    filter(!is.na(BLS_WORK) & !is.na(unpaid_labor) & !is.na(SEX) & !is.na(WT06)) %>%
    filter(BLS_WORK >= 0 & unpaid_labor >= 0)
    x_q99 <- quantile(plot_data_paid_unpaid$BLS_WORK, 0.99, na.rm = TRUE)
    y_q99 <- quantile(plot_data_paid_unpaid$unpaid_labor, 0.99, na.rm = TRUE)
    plot_paid_vs_unpaid <- ggplot(plot_data_paid_unpaid, aes(x = BLS_WORK, y = unpaid_labor, color = SEX)) +
        geom_point(alpha = 0.1, size = 0.5) +
        geom_smooth(aes(weight = WT06, fill = SEX), method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, alpha = 0.2) +
        scale_color_brewer(palette = "Accent", name = "Gender") +
        scale_fill_brewer(palette = "Accent", name = "Gender") +
        coord_cartesian(xlim = c(0, x_q99), ylim = c(0, y_q99)) +
        labs(
            title = "Relationship Between Paid Work and Unpaid Labor Time by Gender",
            subtitle = "Weighted estimate using GAM smoother (Minutes per Day)",
            x = "Minutes Spent on Paid Work (BLS_WORK)",
            y = "Minutes Spent on Unpaid Labor",
            caption = paste("Source: ATUS Data with survey weights (WT06). Axes capped at 99th percentile (",
                            round(x_q99), " mins paid, ", round(y_q99), " mins unpaid).", sep="")
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "bottom"
        )

    print(plot_paid_vs_unpaid)
    ggsave("gender_comparison_plots/paid_vs_unpaid_labor_scatter_weighted.jpg", plot_paid_vs_unpaid, width = 10, height = 8, dpi = 300)

}


# HERE WE DECIDE TO USE BLS_WORK OR UHRSWORKT_DAILY_MIN 

plot_data_work_compare <- data %>%
    filter(!is.na(BLS_WORK) & !is.na(UHRSWORKT_DAILY_MIN) & !is.na(WT06)) %>%
    filter(BLS_WORK >= 0 & UHRSWORKT_DAILY_MIN >= 0)



x_q99_comp <- quantile(plot_data_work_compare$UHRSWORKT_DAILY_MIN, 0.99, na.rm = TRUE)
y_q99_comp <- quantile(plot_data_work_compare$BLS_WORK, 0.99, na.rm = TRUE)
max_limit <- max(x_q99_comp, y_q99_comp, na.rm = TRUE) 
    plot_work_comparison <- ggplot(plot_data_work_compare, aes(x = UHRSWORKT_DAILY_MIN, y = BLS_WORK)) +
        geom_point(alpha = 0.1, size = 0.5) +
        geom_smooth(aes(weight = WT06), method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, color = "blue", fill = "lightblue", alpha = 0.3) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
        coord_cartesian(xlim = c(0, max_limit), ylim = c(0, max_limit)) +
        labs(
            title = "Comparison of Diary Day Work Time vs. Usual Daily Work Time",
            subtitle = "Weighted estimate using GAM smoother (Minutes per Day)",
            x = "Usual Daily Work Minutes (Calculated from UHRSWORKT)",
            y = "Diary Day Work Minutes (BLS_WORK)",
            caption = paste("Source: ATUS Data with survey weights (WT06). Red dashed line is y=x.\nAxes capped near 99th percentile (",
                            round(max_limit), " mins).", sep="")
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "none" 
        )

    print(plot_work_comparison)
    ggsave("gender_comparison_plots/work_time_comparison_scatter_weighted.jpg", plot_work_comparison, width = 9, height = 8, dpi = 300)

}



# RESULT? we use bls_work for unsuoervised bcause it is getting modeled along side other bls variables but for supervised we use UHRSWORKT_DAILY_MIN


# THIS PART WAS ***NOT*** MENTIONED IN THE REPORT BECAUSE I DIDNT THINK IT WOULD MATTER/MEAN MUCH 
#unpaid labor by day

mean_stats_day_gender <- svyby(~unpaid_labor, ~DAY + SEX, des, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
if (!is.null(mean_stats_day_gender) && nrow(mean_stats_day_gender) > 0) {
    names(mean_stats_day_gender)[names(mean_stats_day_gender) == "unpaid_labor"] <- "Mean"
    names(mean_stats_day_gender)[startsWith(names(mean_stats_day_gender), "se")] <- "SE"
    plot_data_day_gender <- mean_stats_day_gender %>% filter(!is.na(DAY) & !is.na(SEX))
    plot_data_day_gender$DAY <- factor(plot_data_day_gender$DAY,
                                       levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                                       ordered = TRUE)

    plot_unpaid_labor_day_gender <- ggplot(plot_data_day_gender, aes(x = DAY, y = Mean, color = SEX, group = SEX)) +
        geom_line(linewidth = 1) 
        geom_point(size = 2) +
        scale_color_brewer(palette = "Accent", name = "Gender") +
        labs(
            title = "Mean Daily Unpaid Labor Time by Day of Week and Gender",
            subtitle = "Weighted estimate (Minutes per Day)",
            x = "Day of the Week",
            y = "Mean Minutes per Day",
            caption = "Source: ATUS Data with survey weights (WT06)."
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
        )
    print(plot_unpaid_labor_day_gender)
    ggsave("gender_comparison_plots/unpaid_labor_by_day_gender_line.jpg", plot_unpaid_labor_day_gender, width = 9, height = 6, dpi = 300)

#summary table

unweighted_counts_day_gender <- data %>%
    filter(!is.na(unpaid_labor) & !is.na(DAY) & !is.na(SEX)) %>%
    count(DAY, SEX, name = "N_Unweighted")
summary_day_gender <- plot_data_day_gender %>%
    select(DAY, SEX, Mean, SE) %>%
    left_join(unweighted_counts_day_gender, by = c("DAY", "SEX")) %>%
    arrange(DAY, SEX)
unpaid_summary_day_gender_gt <- gt(summary_day_gender, groupname_col = "DAY") %>%
  tab_header(
    title = "Mean Daily Unpaid Labor by Day of Week and Gender",
    subtitle = "Weighted estimates (Minutes per day) and unweighted sample size"
  ) %>%
  fmt_number(
    columns = c(Mean, SE),
    decimals = 1
  ) %>%
  fmt_integer(
    columns = N_Unweighted 
  ) %>%
  cols_label(
    SEX = "Gender",
    Mean = "Mean Minutes",
    SE = "Std. Error",
    N_Unweighted = "N (Unweighted)" 
  ) %>%
    tab_options(
      column_labels.font.weight = "bold"
    ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).")

print(unpaid_summary_day_gender_gt)
gtsave(unpaid_summary_day_gender_gt, "unpaid_work_by_day_gender_summary.png")


} 




#corrolation matrix for time use variables

existing_vars <- intersect(bls_time_use_variables, names(des$variables))
    formula_corr <- as.formula(paste("~", paste(existing_vars, collapse = "+")))
    weighted_cov <- svyvar(formula_corr, design = des, na.rm = TRUE)
    if (!is.null(weighted_cov) && all(dim(weighted_cov) > 0)) {
        weighted_corr <- cov2cor(as.matrix(weighted_cov)) 
        clean_names <- gsub("BLS_", "", rownames(weighted_corr))
        clean_names[clean_names == "unpaid_labor"] <- "Unpaid Labor"
        clean_names[clean_names == "WORK"] <- "Paid Work"
        rownames(weighted_corr) <- clean_names
        colnames(weighted_corr) <- clean_names
        corr_plot <- ggcorrplot(
            weighted_corr,
            method = "square",   
            type = "lower",       
            lab = TRUE,           
            lab_size = 3,         
            colors = c("#6D9EC1", "white", "#E46726"), 
            title = "Weighted Correlation Matrix of Time Use Categories",
            ggtheme = theme_minimal()
        ) +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8)
        ) +
        labs(caption = "Source: ATUS Data with survey weights (WT06). Pairwise deletion for NA.")

        print(corr_plot)
        ggsave("gender_comparison_plots/time_use_correlation_heatmap_weighted.jpg", corr_plot, width = 8, height = 7, dpi = 300)
}


#densirt plots for time use variables


time_vars_for_density <- bls_time_use_variables
density_plot_list <- list()
for (variable in time_vars_for_density) {
    title_var <- gsub("BLS_", "", variable)
    title_var <- gsub("_", " ", title_var)
    if (variable == "unpaid_labor") {
        title_var <- "Unpaid Labor"
    } else {
        title_var <- tools::toTitleCase(tolower(title_var))
    }
    plot_data_density <- data %>%
        filter(!is.na(.data[[variable]]) & !is.na(WT06) & WT06 > 0)

    if (nrow(plot_data_density) == 0) {
        warning(paste("no valid data for", variable, "after filtering missing value Skipping density plot"))
        next
    }
    p <- ggplot(plot_data_density, aes(x = .data[[variable]], weight = WT06)) +
        geom_density(na.rm = TRUE, fill = "violet", alpha = 0.7) +
        labs(
            title = title_var,
            x = "Minutes per Day",
            y = "Weighted Density"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, hjust = 0.5))

    density_plot_list[[variable]] <- p
}

    if (length(density_plot_list) > 0) {
        n_plots_density <- length(density_plot_list)
        n_cols_density <- 2 
        n_rows_density <- ceiling(n_plots_density / n_cols_density)

        combined_density_plots <- wrap_plots(density_plot_list, ncol = n_cols_density, nrow = n_rows_density) +
            plot_annotation(
                title = "Weighted Density Distributions of Time Use Categories",
                subtitle = "Minutes per Day (ATUS)",
                caption = "Source: ATUS data with survey weights (WT06)."
            ) & theme(plot.margin = margin(10, 10, 10, 10))

        print(combined_density_plots)
        ggsave("gender_comparison_plots/combined_time_use_density.png", combined_density_plots, width = 10, height = n_rows_density * 3.5, dpi = 300)

    } 




#summary table for time use variables

time_vars_to_summarize <- bls_time_use_variables 
summary_list <- list()

for (variable in time_vars_to_summarize) {
    cat(" summary stats for:", variable, "\n")
    if (!variable %in% names(des$variables)) {
        warning(paste("variable", variable, "error"))
        next
    }
    mean_formula <- as.formula(paste("~", variable))
    mean_result <- svymean(mean_formula, des, na.rm = TRUE)
    current_mean <- tryCatch(coef(mean_result)[1], error = function(e) NA_real_)
    current_se <- tryCatch(SE(mean_result)[1], error = function(e) NA_real_)

    median_formula <- as.formula(paste("~", variable))
    median_result <- svyquantile(median_formula, des, quantiles = 0.5, ci = FALSE, na.rm = TRUE)
    current_median <- NA_real_
    if (is.numeric(median_result) && length(median_result) >= 1) {
        current_median <- median_result[1]
    } else if (is.list(median_result) && length(median_result) >= 1) {
         first_element <- median_result[[1]]
         if (is.matrix(first_element) && nrow(first_element) == 1 && ncol(first_element) == 1) {
             current_median <- first_element[1, 1]
         } else if (is.numeric(first_element) && length(first_element) >= 1) {
             current_median <- first_element[1]
         }
    } else if (is.matrix(median_result) && nrow(median_result) == 1 && ncol(median_result) == 1) {
         current_median <- median_result[1, 1]
    }

    current_n <- sum(!is.na(data[[variable]]))

    summary_list[[variable]] <- data.frame(
        Variable = variable,
        Mean = current_mean,
        SE = current_se,
        Median = current_median,
        N_Unweighted = current_n
    )
}

if (length(summary_list) > 0) {
    combined_summary_time_use <- bind_rows(summary_list)
    combined_summary_time_use <- combined_summary_time_use %>%
        mutate(
            Variable_Label = gsub("BLS_", "", Variable),
            Variable_Label = gsub("_", " ", Variable_Label),
            Variable_Label = ifelse(Variable == "unpaid_labor", "Unpaid Labor", tools::toTitleCase(tolower(Variable_Label)))
        ) %>%
        select(Variable_Label, Mean, SE, Median, N_Unweighted) 
    time_use_summary_gt <- gt(combined_summary_time_use) %>%
      tab_header(
        title = "Summary Statistics for Time Use Variables",
        subtitle = "Weighted estimates (Minutes per Day) and unweighted sample size"
      ) %>%
      fmt_number(
        columns = c(Mean, SE, Median),
        decimals = 1
      ) %>%
      fmt_integer(
        columns = N_Unweighted
      ) %>%
      cols_label(
        Variable_Label = "Time Use Category",
        Mean = "Mean",
        SE = "Std. Error",
        Median = "Median",
        N_Unweighted = "N (Unweighted)"
      ) %>%
       tab_options(
          column_labels.font.weight = "bold"
       ) %>%
      tab_source_note("Source: ATUS data with survey weights (WT06). N is the unweighted count of non-missing observations.")

    print(time_use_summary_gt)
    gtsave(time_use_summary_gt, "time_use_variables_summary.png")

} 





#unpaid labor by partner presence

plot_data_partner <- data %>%
    filter(!is.na(unpaid_labor) & !is.na(SEX) & !is.na(SPOUSEPRES) & !is.na(WT06)) %>%
    mutate(Partner_Status = factor(SPOUSEPRES, levels = c("Spouse present", "Unmarried partner present", "No spouse/partner present")))

des_partner <- svydesign(ids = ~1, weights = ~WT06, data = plot_data_partner)
des_partner <- update(des_partner, Partner_Status = plot_data_partner$Partner_Status)
mean_stats_partner_gender <- svyby(~unpaid_labor, ~Partner_Status + SEX, des_partner, svymean, na.rm = TRUE, drop.empty.groups = FALSE)
if (!is.null(mean_stats_partner_gender) && nrow(mean_stats_partner_gender) > 0) {
    names(mean_stats_partner_gender)[names(mean_stats_partner_gender) == "unpaid_labor"] <- "Mean"
    names(mean_stats_partner_gender)[startsWith(names(mean_stats_partner_gender), "se")] <- "SE"
    mean_stats_partner_gender <- mean_stats_partner_gender %>% filter(!is.na(Partner_Status) & !is.na(SEX))
    plot_unpaid_labor_partner_gender <- ggplot(mean_stats_partner_gender, aes(x = Partner_Status, y = Mean, fill = SEX)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        geom_errorbar(aes(ymin = Mean - 1.96 * SE, ymax = Mean + 1.96 * SE),
                      width = 0.25, position = position_dodge(width = 0.9)) +
        scale_fill_brewer(palette = "Accent", name = "Gender") +
        labs(
            title = "Mean Daily Unpaid Labor Time by Partner Presence and Gender",
            subtitle = "Weighted estimate (Minutes per Day)",
            x = "Partner Presence Status",
            y = "Mean Minutes per Day",
            caption = "Source: ATUS Data with survey weights (WT06). Error bars represent 95% CI."
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "bottom"
        )

print(plot_unpaid_labor_partner_gender)
ggsave("gender_comparison_plots/unpaid_labor_by_partner_status_gender.jpg", plot_unpaid_labor_partner_gender, width = 9, height = 7, dpi = 300)


#summary table
summary_partner_gender <- mean_stats_partner_gender %>%
    select(Partner_Status, SEX, Mean, SE) %>%
    arrange(Partner_Status, SEX)
unpaid_summary_partner_gender_gt <- gt(summary_partner_gender, groupname_col = "Partner_Status") %>%
  tab_header(
    title = "Mean Daily Unpaid Labor by Partner Presence and Gender",
    subtitle = "Weighted estimates (Minutes per day)"
  ) %>%
  fmt_number(
    columns = c(Mean, SE),
    decimals = 1
  ) %>%
  cols_label(
    SEX = "Gender",
    Mean = "Mean Minutes",
    SE = "Std. Error"
  ) %>%
    tab_options(
      column_labels.font.weight = "bold"
    ) %>%
  tab_source_note("Source: ATUS data with survey weights (WT06).")

print(unpaid_summary_partner_gender_gt)
gtsave(unpaid_summary_partner_gender_gt, "unpaid_work_by_partner_status_gender_summary.png")

} 



#density plots for demographic variables - numerical

demog_vars_for_density <- numerical_demographic_vars
density_plot_list_demog <- list()
for (variable in demog_vars_for_density) {
    if (!variable %in% names(data)) {
        warning(paste("Variable", variable, "not found in data. Skipping density plot."))
        next
    }

    title_var_demog <- gsub("_", " ", variable)
    if (variable == "UHRSWORKT_DAILY_MIN") {
        title_var_demog <- "Usual Daily Work Mins"
    } else {
        title_var_demog <- tools::toTitleCase(tolower(title_var_demog))
    }

    plot_data_density_demog <- data %>%
        filter(!is.na(.data[[variable]]) & !is.na(WT06) & WT06 > 0)

    x_limit_demog <- quantile(plot_data_density_demog[[variable]], 0.99, na.rm = TRUE, weight = plot_data_density_demog$WT06)
    if(all(plot_data_density_demog[[variable]] >= 0, na.rm=TRUE)) {
        x_limit_demog <- max(0, x_limit_demog)
    }

    p_demog <- ggplot(plot_data_density_demog, aes(x = .data[[variable]], weight = WT06)) +
        geom_density(na.rm = TRUE, fill = "violet", alpha = 0.7) +
        # Apply x-limit if it's reasonable (not NA/Inf and greater than min)
        { if (!is.na(x_limit_demog) && is.finite(x_limit_demog) && x_limit_demog > min(plot_data_density_demog[[variable]], na.rm=TRUE)) coord_cartesian(xlim = c(min(plot_data_density_demog[[variable]], na.rm=TRUE), x_limit_demog)) } +
        labs(
            title = title_var_demog,
            x = "Value",
            y = "Weighted Density"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, hjust = 0.5)) # Adjust title size

    density_plot_list_demog[[variable]] <- p_demog
}

if (length(density_plot_list_demog) > 0) {
    n_plots_density_demog <- length(density_plot_list_demog)
    n_cols_density_demog <- ceiling(sqrt(n_plots_density_demog))
    n_rows_density_demog <- ceiling(n_plots_density_demog / n_cols_density_demog)

    combined_density_plots_demog <- wrap_plots(density_plot_list_demog, ncol = n_cols_density_demog, nrow = n_rows_density_demog) +
        plot_annotation(
            title = "Weighted Density Distributions of Numerical Demographic Variables",
            subtitle = "ATUS Data",
            caption = "Source: ATUS data with survey weights (WT06). X-axes may be capped near 99th percentile."
        ) & theme(plot.margin = margin(10, 10, 10, 10))

print(combined_density_plots_demog)

ggsave("gender_comparison_plots/combined_demographic_density.png", combined_density_plots_demog, width = n_cols_density_demog * 4, height = n_rows_density_demog * 3.5, dpi = 300)

} 



#corrolation matrix for demographic variables - numerical

existing_demog_vars <- intersect(numerical_demographic_vars, names(des$variables))
if (length(existing_demog_vars) < 2) {
    print("error")
} else {
    formula_corr_demog <- as.formula(paste("~", paste(existing_demog_vars, collapse = "+")))

    weighted_cov_demog <- svyvar(formula_corr_demog, design = des, na.rm = TRUE)
    if (!is.null(weighted_cov_demog) && all(dim(weighted_cov_demog) > 0)) {
        weighted_corr_demog <- cov2cor(as.matrix(weighted_cov_demog))
        clean_names_demog <- gsub("_", " ", rownames(weighted_corr_demog))
        clean_names_demog[clean_names_demog == "UHRSWORKT DAILY MIN"] <- "Usual Daily Work (min)"
        clean_names_demog <- tools::toTitleCase(tolower(clean_names_demog)) 
        rownames(weighted_corr_demog) <- clean_names_demog
        colnames(weighted_corr_demog) <- clean_names_demog

        corr_plot_demog <- ggcorrplot(
            weighted_corr_demog,
            method = "square",    
            type = "lower",       
            lab = TRUE,           
            lab_size = 2.5,       
            colors = c("#6D9EC1", "white", "#E46726"), 
            title = "Weighted Correlation Matrix of Numerical Demographic Variables",
            ggtheme = theme_minimal()
        ) +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8), 
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 8)
        ) +
        labs(caption = "Source: ATUS Data with survey weights (WT06). Pairwise deletion for NA.")
        print(corr_plot_demog)
        ggsave("gender_comparison_plots/demographic_correlation_heatmap_weighted.jpg", corr_plot_demog, width = 9, height = 8, dpi = 300)

    } 
}





######################################################################################
#PHASE 3: PCA and Clustering - UNSUPERVISED ANALYSIS
#######################################################################################


# firs we filter only for couples because it made more sense to do the PCA on partnered individuals

data_partnered_for_unsupervised <- data %>% 
    filter(SPOUSEPRES %in% c("Spouse present", "Unmarried partner present"))



# some initial checks on the data

print(dim(data_partnered_for_unsupervised))
missing_counts_partnered <- colSums(is.na(data_partnered_for_unsupervised))
print(missing_counts_partnered)



# since there is no missing value in the attributes that i want to use for pca, i deal with NAs later

variables_for_pca <- c("BLS_COMM", "BLS_EDUC", "BLS_FOOD", "BLS_LEIS", "BLS_SOCIAL", "BLS_WORK", "unpaid_labor", "BLS_PCARE")

# HERE WE DO THE DENSITY PLOTS FOR THE PCA VARIABLES BEFORE AND AFTER TRANSFORMING THEM TO CHECK IF IT IS DONE PROPERLY


density_plot_list_pca <- list()
for (variable in variables_for_pca) {
    if (!variable %in% names(data_partnered_for_unsupervised)) {
        warning(paste("Variable", variable, "error"))
        next
    }

    title_var_pca <- gsub("BLS_", "", variable)
    title_var_pca <- gsub("_", " ", title_var_pca)
    title_var_pca <- tools::toTitleCase(tolower(title_var_pca))

    plot_data_density_pca <- data_partnered_for_unsupervised %>%
        filter(!is.na(.data[[variable]]) & !is.na(WT06) & WT06 > 0)

    if (nrow(plot_data_density_pca) == 0) {
        warning(paste("No valid data for", variable, "erroer"))
        next
    }

    x_limit_pca <- quantile(plot_data_density_pca[[variable]], 0.99, na.rm = TRUE, weight = plot_data_density_pca$WT06)
    if(all(plot_data_density_pca[[variable]] >= 0, na.rm=TRUE)) {
        x_limit_pca <- max(0, x_limit_pca)
    }

    p_pca <- ggplot(plot_data_density_pca, aes(x = .data[[variable]], weight = WT06)) +
        geom_density(na.rm = TRUE, fill = "lightblue", alpha = 0.7) +
        { if (!is.na(x_limit_pca) && is.finite(x_limit_pca) && x_limit_pca > min(plot_data_density_pca[[variable]], na.rm=TRUE)) coord_cartesian(xlim = c(min(plot_data_density_pca[[variable]], na.rm=TRUE), x_limit_pca)) } +
        labs(
            title = title_var_pca,
            x = "Minutes per Day",
            y = "Weighted Density"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, hjust = 0.5))

    density_plot_list_pca[[variable]] <- p_pca
}

if (length(density_plot_list_pca) > 0) {
    n_plots_pca <- length(density_plot_list_pca)
    n_cols_pca <- ceiling(sqrt(n_plots_pca))
    n_rows_pca <- ceiling(n_plots_pca / n_cols_pca)

    combined_density_plots_pca <- wrap_plots(density_plot_list_pca, ncol = n_cols_pca, nrow = n_rows_pca) +
        plot_annotation(
            title = "Weighted Density Distributions of Variables for PCA (Partnered Individuals)",
            subtitle = "Minutes per Day (ATUS)",
            caption = "Source: ATUS data (Partnered Individuals) with survey weights (WT06). X-axes may be capped near 99th percentile."
        ) & theme(plot.margin = margin(10, 10, 10, 10))

    print(combined_density_plots_pca)
    ggsave("gender_comparison_plots/combined_pca_variable_density_partnered.png", combined_density_plots_pca, width = n_cols_pca * 4, height = n_rows_pca * 3.5, dpi = 300)

} 



#based on the density plots the variables that need log transforms are as follows:

val_to_transform <- c("BLS_COMM", "BLS_EDUC", "BLS_SOCIAL","unpaid_labor")



data_pca_input <- data_partnered_for_unsupervised %>%
    select(all_of(variables_for_pca))


#FIRST WE CAP (OUTLIER REMOVAL)

for (var in variables_for_pca) {
    if (var %in% names(data_pca_input) && is.numeric(data_pca_input[[var]])) {
        q99 <- quantile(data_pca_input[[var]], probs = 0.99, na.rm = TRUE)
        if (!is.na(q99)) {
            data_pca_input[[var]] <- ifelse(data_pca_input[[var]] > q99 & !is.na(data_pca_input[[var]]),
                                            q99,
                                            data_pca_input[[var]])
        } else {
             warning(paste("error"))
        }
    } else {
         warning(paste("error"))
    }
}

# LOG TRANSFORM THE VARIABLES THAT NEED IT (RIGHT SKEWED)

for (var in val_to_transform) {
    if (var %in% names(data_pca_input) && is.numeric(data_pca_input[[var]])) {
        if (any(data_pca_input[[var]] < 0, na.rm = TRUE)) {
            warning(paste("error"))
        }
        data_pca_input[[var]] <- log1p(data_pca_input[[var]]) =
    } else {
         warning(paste("error"))
    }
}

#STANDARDAZIATION

print("Standardizing variables (z-scoring)...")
vars_to_scale <- sapply(data_pca_input, function(x) is.numeric(x) && sd(x, na.rm = TRUE) > 0)
if (any(!vars_to_scale)) {
    warning(paste("Columns with zero variance detected:",
                  paste(names(vars_to_scale)[!vars_to_scale], collapse=", "),
                  ". These cannot be scaled and might cause issues in PCA."))

}

data_pca_ready <- data_pca_input
data_pca_ready[, vars_to_scale] <- scale(data_pca_input[, vars_to_scale])

# Check the result
print(head(data_pca_ready))
summary(data_pca_ready)



# POST TRANSFORMATION DENSITY PLOTS

if (nrow(data_pca_ready) == nrow(data_partnered_for_unsupervised)) {
    data_pca_ready_with_weights <- data_pca_ready
    data_pca_ready_with_weights$WT06 <- data_partnered_for_unsupervised$WT06
}


density_plot_list_transformed <- list()

for (variable in variables_for_pca) {
    if (!variable %in% names(data_pca_ready_with_weights)) {
        warning(paste("Variable", variable, "not found in scaled data with weights. Skipping density plot."))
        next
    }

    title_var_trans <- gsub("BLS_", "", variable)
    title_var_trans <- gsub("_", " ", title_var_trans)
    title_var_trans <- tools::toTitleCase(tolower(title_var_trans))
    title_var_trans <- paste(title_var_trans, "(Transformed)")
    plot_data_density_trans <- data_pca_ready_with_weights %>%
        filter(!is.na(WT06) & WT06 > 0)
    if (nrow(plot_data_density_trans) == 0) {
        warning(paste("No valid data for transformed", variable, "after filtering NAs/weights. Skipping density plot."))
        next
    }
    p_trans <- ggplot(plot_data_density_trans, aes(x = .data[[variable]], weight = WT06)) +
        geom_density(na.rm = TRUE, fill = "lightgreen", alpha = 0.7) + # Changed color
        labs(
            title = title_var_trans,
            x = "Standardized Value (Z-score)",
            y = "Weighted Density"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(size = 10, hjust = 0.5))

    density_plot_list_transformed[[variable]] <- p_trans
}

if (length(density_plot_list_transformed) > 0) {
    n_plots_trans <- length(density_plot_list_transformed)
    n_cols_trans <- ceiling(sqrt(n_plots_trans))
    n_rows_trans <- ceiling(n_plots_trans / n_cols_trans)

    combined_density_plots_trans <- wrap_plots(density_plot_list_transformed, ncol = n_cols_trans, nrow = n_rows_trans) +
        plot_annotation(
            title = "Weighted Density Distributions After Transformation (Capped, Logged, Scaled)",
            subtitle = "Variables Prepared for PCA (Partnered Individuals)",
            caption = "Source: ATUS data (Partnered Individuals) with survey weights (WT06)."
        ) & theme(plot.margin = margin(10, 10, 10, 10))

    print(combined_density_plots_trans)
    ggsave("gender_comparison_plots/combined_pca_variable_density_transformed.png", combined_density_plots_trans, width = n_cols_trans * 4, height = n_rows_trans * 3.5, dpi = 300)

}



# it looks like the transformation worked somewhat okay 

#doing pca and clustring now

# PCA


pca_result <- prcomp(data_pca_ready, center = TRUE, scale. = FALSE)
print(summary(pca_result))

var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_var_explained <- cumsum(var_explained)
jpeg("scree_plot.jpg", width = 800, height = 600) # Open JPG device with specified dimensions
plot(1:length(var_explained), var_explained, type = "b",
    xlab = "Principal Component", ylab = "Proportion of Variance Explained",
    main = "Scree Plot")
dev.off() 


loadings <- pca_result$rotation
print(loadings)

jpeg("pca_biplot.jpg", width = 800, height = 600) 
biplot(pca_result, scale = 0)                     
dev.off()                                         

contrib <- (pca_result$rotation^2) * 100 / rowSums(pca_result$rotation^2)
print(contrib)





contrib_df <- as.data.frame(contrib[, 1:4])  
contrib_df$variable <- rownames(contrib_df)
contrib_long <- tidyr::pivot_longer(contrib_df, cols = c("PC1", "PC2", "PC3", "PC4"), 
                                    names_to = "component", values_to = "contribution")

contrib_plot <- ggplot(contrib_long, aes(x = reorder(variable, contribution), y = contribution, fill = component)) +
geom_col() +
coord_flip() +
facet_wrap(~component, ncol = 2) +
labs(title = "Variable Contributions to Principal Components 1-4",
            subtitle = "Higher values indicate stronger influence on the component",
            x = NULL, y = "Contribution (%)") +
theme_minimal() +
theme(legend.position = "none",
            strip.background = element_rect(fill = "lightblue"),
            strip.text = element_text(face = "bold")) +
scale_fill_brewer(palette = "Accent")

ggsave("variable_contributions_pca.jpg", plot = contrib_plot, width = 10, height = 8, dpi = 300)





pcs_for_clustering <- pca_result$x[, 1:4]


# CLUSTERING

set.seed(123)
wss <- sapply(1:10, function(k){
  if(k == 1) return(sum(scale(pcs_for_clustering, scale = FALSE)^2))
  kmeans(pcs_for_clustering, centers = k, nstart = 25)$tot.withinss
})

elbow_plot <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, color = "blue") +
  labs(
    title = "Elbow Method for Optimal Number of Clusters",
    subtitle = "Work-Life Balance Profiles from PCA",
    x = "Number of Clusters (k)",
    y = "Within-cluster Sum of Squares"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(elbow_plot)
ggsave("elbow_plot_clusters.jpg", elbow_plot, width = 8, height = 6, dpi = 300)



silhouette_scores <- sapply(2:8, function(k) {
  km <- kmeans(pcs_for_clustering, centers = k, nstart = 25)
  ss <- silhouette(km$cluster, dist(pcs_for_clustering))
  mean(ss[, 3]) 
})


sil_plot <- ggplot(data.frame(k = 2:8, score = silhouette_scores), aes(x = k, y = score)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, color = "red") +
  labs(
    title = "Average Silhouette Scores by Number of Clusters",
    subtitle = "Higher values indicate better defined clusters",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(sil_plot)
ggsave("silhouette_scores_plot.jpg", sil_plot, width = 8, height = 6, dpi = 300)




set.seed(123)
final_k <- 3 
final_clusters <- kmeans(pcs_for_clustering, centers = final_k, nstart = 50)

data_with_clusters <- data_partnered_for_unsupervised %>%
  mutate(
    PC1 = pcs_for_clustering[,1],
    PC2 = pcs_for_clustering[,2],
    PC3 = pcs_for_clustering[,3],
    PC4 = pcs_for_clustering[,4],
    cluster = factor(final_clusters$cluster, 
                    levels = 1:final_k,
                    labels = paste("Profile", 1:final_k))
  )

cluster_sizes <- table(data_with_clusters$cluster)
print(cluster_sizes)







# final chosen k is 3

final_k <- 3
final_km <- kmeans(pcs_for_clustering, centers = final_k, nstart = 50)
final_sil <- silhouette(final_km$cluster, dist(pcs_for_clustering))

sil_values <- data.frame(
  cluster = factor(final_sil[,1]),
  neighbor = factor(final_sil[,2]),
  sil_width = final_sil[,3]
)

cluster_summary <- aggregate(sil_width ~ cluster, data = sil_values, 
                            FUN = function(x) c(mean = mean(x), 
                                               min = min(x),
                                               max = max(x),
                                               median = median(x),
                                               n = length(x)))
cluster_summary <- do.call(data.frame, cluster_summary)
colnames(cluster_summary) <- c("Cluster", "Mean_Sil", "Min_Sil", "Max_Sil", "Median_Sil", "Size")

print(cluster_summary)

sil_data <- data.frame(
  id = 1:length(final_sil[,3]),
  cluster = as.factor(final_sil[,1]),
  sil_width = final_sil[,3]
)

sil_data <- sil_data[order(sil_data$cluster, -sil_data$sil_width),]
sil_data$id <- 1:nrow(sil_data)  # Reindex after sorting

enhanced_sil_plot <- ggplot(sil_data, aes(x = id, y = sil_width, fill = cluster)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = mean(sil_data$sil_width), linetype = "dashed", color = "red") +
  geom_text(aes(x = Inf, y = mean(sil_data$sil_width), 
                label = paste0("Avg: ", round(mean(sil_data$sil_width), 3))),
            hjust = 1.1, vjust = -0.5, color = "red") +
  facet_grid(. ~ cluster, scales = "free_x", space = "free_x") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = paste0("Silhouette Widths for k = ", final_k, " Cluster Solution"),
    subtitle = "Higher values (closer to 1) indicate better cluster assignment",
    x = "Observations (Grouped by Cluster)",
    y = "Silhouette Width",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_rect(fill = "lightgrey"),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.spacing.x = unit(0.5, "lines")
  )

print(enhanced_sil_plot)
ggsave("enhanced_silhouette_plot.jpg", enhanced_sil_plot, width = 12, height = 8, dpi = 300)





# PCA Cluster Plot

pca_cluster_plot <- ggplot(data_with_clusters, aes(x = PC1, y = PC2, color = cluster)) +
  stat_density_2d(geom = "polygon", aes(alpha = after_stat(level), fill = cluster), bins = 6) +
  geom_point(size = 1.5, alpha = 0.6) +
  scale_alpha_continuous(range = c(0.05, 0.3), guide = "none") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = "none") +
  labs(
    title = "Work-Life Balance Profiles in PCA Space",
    subtitle = "Identified through K-means clustering",
    x = paste0("PC1 (", round(var_explained[1]*100, 1), "% variance explained)"),
    y = paste0("PC2 (", round(var_explained[2]*100, 1), "% variance explained)"),
    color = "Profile"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(pca_cluster_plot)
ggsave("pca_cluster_plot.jpg", pca_cluster_plot, width = 10, height = 8, dpi = 300)




# Radar Charts

cluster_means <- data_with_clusters %>%
    group_by(cluster) %>%
    summarize(across(all_of(variables_for_pca), mean, na.rm = TRUE))

radar_data_full <- as.data.frame(cluster_means %>% select(-cluster))
rownames(radar_data_full) <- paste("Profile", sort(unique(as.numeric(cluster_means$cluster))))

actual_min <- min(as.matrix(radar_data_full), na.rm = TRUE)
actual_max <- max(as.matrix(radar_data_full), na.rm = TRUE)
cat("Data range:", actual_min, "to", actual_max, "\n")


max_row <- rep(max(actual_max * 1.1, actual_max + 10), ncol(radar_data_full))
min_row <- rep(min(0, actual_min * 0.9), ncol(radar_data_full))

clean_var_names <- gsub("BLS_", "", colnames(radar_data_full))
clean_var_names <- gsub("_", " ", clean_var_names)
clean_var_names <- tools::toTitleCase(clean_var_names)
clean_var_names[clean_var_names == "Unpaid Labor"] <- "Unpaid Labor"
colnames(radar_data_full) <- clean_var_names

num_profiles <- nrow(radar_data_full)
profile_colors <- brewer.pal(n = min(num_profiles, 9), name = "Set1")

jpeg("work_life_balance_profiles_radar_3x1.png", width = 1200, height =500, quality = 100)
par(mfrow = c(1, 3), mar = c(1, 1, 2, 1)) 

for (i in 1:num_profiles) {
    profile_name <- rownames(radar_data_full)[i]
    
    radar_data_single <- rbind(
        max_row,
        min_row,
        radar_data_full[i, ]
    )
    colnames(radar_data_single) <- clean_var_names 
        radarchart(
        radar_data_single,
        axistype = 1,
        pcol = profile_colors[i],
        pfcol = scales::alpha(profile_colors[i], 0.3), 
        plwd = 3,
        plty = 1,
        cglcol = "gray",
        cglty = 1,
        axislabcol = "gray30",
        caxislabels = round(seq(min_row[1], max_row[1], length.out = 5), 1), 
        title = paste(profile_name, ": Time Use Pattern") 
    )
}

dev.off() 




# CLUSTER PROFILING - GENDER

gender_cluster_counts <- data_with_clusters %>%
    group_by(cluster, SEX) %>%
    summarise(count = n(), .groups = 'drop')

print(gender_cluster_counts)
print(table(data_with_clusters$cluster, data_with_clusters$SEX))

gender_cluster_plot <- ggplot(data_with_clusters, aes(x = cluster, fill = SEX)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = "Accent") +
    labs(
        title = "Distribution of Gender within Work-Life Balance Profiles",
        x = "Profile (Cluster)",
        y = "Number of Individuals",
        fill = "Gender"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

print(gender_cluster_plot)
ggsave("gender_distribution_by_cluster.jpg", gender_cluster_plot, width = 8, height = 6, dpi = 300)


gender_cluster_prop_plot <- ggplot(data_with_clusters, aes(x = cluster, fill = SEX)) +
    geom_bar(position = "fill") +
    scale_fill_brewer(palette = "Accent") +
    scale_y_continuous(labels = scales::percent_format()) + 
    labs(
        title = "Proportion of Gender within Work-Life Balance Profiles",
        x = "Profile (Cluster)",
        y = "Proportion",
        fill = "Gender"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

print(gender_cluster_prop_plot)
ggsave("gender_proportion_by_cluster.jpg", gender_cluster_prop_plot, width = 8, height = 6, dpi = 300)







cluster_gender_means_all_pca_vars <- data_with_clusters %>%
  filter(SEX %in% c("Male", "Female")) %>%
  group_by(cluster, SEX) %>%
  summarise(across(all_of(variables_for_pca), mean, na.rm = TRUE)) 
print("Mean Time Useby Cluster and Gender")
print(cluster_gender_means_all_pca_vars)



plot_data_gender_cluster_all_pca <- cluster_gender_means_all_pca_vars %>%
  pivot_longer(cols = all_of(variables_for_pca),
               names_to = "Activity",
               values_to = "MeanMinutes") %>%
  mutate(Activity_Label = case_when(
    Activity == "BLS_WORK" ~ "Paid Work",
    Activity == "unpaid_labor" ~ "Unpaid Labor",
    Activity == "BLS_LEIS" ~ "Leisure",
    Activity == "BLS_PCARE" ~ "Personal Care",
    Activity == "BLS_COMM" ~ "Commuting",
    Activity == "BLS_EDUC" ~ "Education",
    Activity == "BLS_FOOD" ~ "Food Prep/Eating",
    Activity == "BLS_SOCIAL" ~ "Socializing/Other",
    TRUE ~ Activity
  )) %>%
   mutate(SEX = factor(SEX, levels = c("Male", "Female")))

gender_division_plot_all_pca <- ggplot(plot_data_gender_cluster_all_pca,
                               aes(x = Activity_Label, y = MeanMinutes, fill = SEX)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width=0.7) +
  facet_wrap(~ cluster, ncol = 3) + 
  scale_fill_manual(values = c("Male" = "#2E86C1", "Female" = "#8E44AD"), name = "Gender") +
  labs(
    title = "Gendered Division of Labor Across Work-Life Profiles",
    subtitle = "Average Daily Minutes Spent on All Activities Used in PCA",
    x = "Activity",
    y = "Mean Minutes per Day",
    caption = "Source: ATUS Data (Partnered Individuals). Based on variables used in clustering."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 60, hjust = 1), 
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill="gray90")
  )

print(gender_division_plot_all_pca)
ggsave("gender_division_by_cluster_all_pca_vars.jpg", gender_division_plot_all_pca, width = 14, height = 7, dpi = 300) 





# CLUSTER PROFILING - CHILDREN

cluster_child_counts <- data_with_clusters %>%
    filter(!is.na(HH_CHILD)) %>%
    count(cluster, HH_CHILD) %>%
    spread(HH_CHILD, n, fill = 0) %>%
    rename(No_Children = No, Has_Children = Yes) %>%
    mutate(Total = No_Children + Has_Children,
                 Child_Percent = round(Has_Children / Total * 100, 1))

print(cluster_child_counts)

child_dist_plot <- ggplot(data_with_clusters %>% filter(!is.na(HH_CHILD)), 
                                                 aes(x = cluster, fill = HH_CHILD)) +
    geom_bar(position = "stack") +
    scale_fill_brewer(palette = "Set2", name = "Children Under 18") +
    labs(
        title = "Distribution of Households with Children Across Work-Life Balance Profiles",
        x = "Profile (Cluster)",
        y = "Count",
        caption = "Source: ATUS Data (Partnered Individuals)"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom"
    )

print(child_dist_plot)
ggsave("child_distribution_by_cluster.jpg", child_dist_plot, width = 8, height = 6, dpi = 300)


cluster_child_means <- data_with_clusters %>%
  filter(!is.na(HH_CHILD)) %>%
  group_by(cluster, HH_CHILD) %>%
  summarise(across(all_of(c("BLS_WORK", "unpaid_labor", "BLS_LEIS", "BLS_PCARE")), mean, na.rm = TRUE),
            .groups = "drop")

print(cluster_child_means)

plot_data_cluster_child <- cluster_child_means %>%
  pivot_longer(cols = c(BLS_WORK, unpaid_labor, BLS_LEIS, BLS_PCARE),
               names_to = "Activity",
               values_to = "MeanMinutes") %>%
  mutate(Activity_Label = case_when(
    Activity == "BLS_WORK" ~ "Paid Work",
    Activity == "unpaid_labor" ~ "Unpaid Labor",
    Activity == "BLS_LEIS" ~ "Leisure",
    Activity == "BLS_PCARE" ~ "Personal Care",
    TRUE ~ Activity
  )) %>%
   mutate(HH_CHILD = factor(HH_CHILD, levels = c("No", "Yes"), labels = c("No Children < 18", "Children < 18 Present")))


child_influence_plot <- ggplot(plot_data_cluster_child,
                               aes(x = Activity_Label, y = MeanMinutes, fill = HH_CHILD)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width=0.7) +
  facet_wrap(~ cluster, ncol = 3) +
  scale_fill_manual(values = c("No Children < 18" = "#5DADE2", "Children < 18 Present" = "#F5B041"), name = "Household Composition") +
  labs(
    title = "Influence of Children (<18) on Time Use Across Work-Life Profiles",
    subtitle = "Average Daily Minutes Spent on Key Activities",
    x = "Activity",
    y = "Mean Minutes per Day",
    caption = "Source: ATUS Data (Partnered Individuals). Based on variables used in clustering."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill="gray90")
  )

print(child_influence_plot)
ggsave("child_influence_by_cluster.jpg", child_influence_plot, width = 12, height = 6, dpi = 300)







# CLUSTER PROFILING - FAMILY INCOME

data_with_clusters <- data_with_clusters %>%
  mutate(
    Income_Group = factor(case_when(
      FAMINCOME %in% c("Less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                       "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $19,999",
                       "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999") ~ "< $35k",
      FAMINCOME %in% c("$35,000 to $39,999", "$40,000 to $49,999", "$50,000 to $59,999",
                       "$60,000 to $74,999") ~ "$35k - $74.9k",
      FAMINCOME %in% c("$75,000 to $99,999", "$100,000 to $149,999") ~ "$75k - $149.9k",
      FAMINCOME == "$150,000 and over" ~ "$150k+",
      TRUE ~ NA_character_
    ), levels = c("< $35k", "$35k - $74.9k", "$75k - $149.9k", "$150k+"))
  )

cluster_income_grouped_dist <- data_with_clusters %>%
  filter(!is.na(Income_Group)) %>% 
  count(cluster, Income_Group) %>%
  group_by(cluster) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup() %>%
  arrange(cluster, desc(Income_Group)) %>%
  group_by(cluster) %>%
  mutate(Label_Y = cumsum(Proportion) - 0.5 * Proportion)

print(cluster_income_grouped_dist %>% select(cluster, Income_Group, n, Proportion))

income_summary_grouped_table <- cluster_income_grouped_dist %>%
  select(cluster, Income_Group, Proportion) %>%
  pivot_wider(names_from = Income_Group, values_from = Proportion, values_fill = 0) %>%
  mutate(across(where(is.numeric), ~round(. * 100, 1)))

print(income_summary_grouped_table)






income_colors <- brewer.pal(n = length(levels(cluster_income_grouped_dist$Income_Group)), name = "YlGnBu") 

income_cluster_grouped_plot <- ggplot(cluster_income_grouped_dist,
                                      aes(x = cluster, y = Proportion, fill = Income_Group)) +
  geom_bar(stat = "identity", position = "fill", color = "white", linewidth = 0.2) + 
  geom_text(aes(y = Label_Y, label = paste0(round(Proportion * 100), "%")),
            color = "black", size = 3.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = income_colors, name = "Family Income Group") +
  labs(
    title = "Family Income Distribution Across Work-Life Profiles",
    subtitle = "Proportion of households within each profile",
    x = "Profile (Cluster)",
    y = "Proportion",
    caption = "Source: ATUS Data (Partnered Individuals)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(b=10)),
    axis.text.x = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", margin = margin(t=10)),
    axis.title.y = element_text(face = "bold", margin = margin(r=10)),
    legend.position = "bottom",
    legend.title = element_text(face="bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(income_cluster_grouped_plot)
ggsave("income_distribution_grouped_by_cluster.jpg", income_cluster_grouped_plot, width = 9, height = 7, dpi = 300)



##############################################################################
# PHASRE 4: SUPERVISED ANALYSIS 
##############################################################################


# BECAUSE THERE HAS BEEN CHANGES TO DATAM I IMPORT THE DATA AGAIN AND WORK ON THAT 
# THE CODE IS MEANT TO BE RUN IN SEQUENCE
# LINEAR MODELS 




originaldata <- read.csv("ATUS LABLED DATA.csv")
data <- read.csv("ATUS LABLED DATA.csv")



data <- data %>%
    mutate(unpaid_labor = BLS_CAREHH + BLS_CARENHH + BLS_HHACT + BLS_PURCH,
                 UHRSWORKT_DAILY_MIN = (UHRSWORKT * 60) / 7,
                 SPUSUALHRS_DAILY_MIN = (SPUSUALHRS * 60) / 7)


# remiving unnecessary variables (we used them to create new variables)

data <- data %>%
    select(-UHRSWORKT, -SPUSUALHRS)



# handling missing value 

# finidng real missing values - childern and spouse related variables

initial_missing <- colSums(is.na(data))
print("Initial count of missing values by variable:")
print(initial_missing[initial_missing > 0])

spouse_vars <- c("SPSEX", "sp_race4", "SPEDUC", "SPEMPSTAT", "SPEARNWEEK", 
                 "SPAGE", "SPUSUALHRS", "SPUSUALHRS_DAILY_MIN")


print(paste("Using", length(spouse_vars), "specific spouse-related variables"))

data <- data %>%
  mutate(across(
    .cols = all_of(intersect(spouse_vars, names(data))), 
    .fns = ~if_else(SPOUSEPRES == "No spouse/partner present", 
                    ifelse(is.numeric(.), NA_real_, "Not Applicable"), 
                    .)
  ))

child_vars <- c("HH_NUMKIDS", "AGEYCHILD") 
print(paste("Using", length(child_vars), "specific children-related variables"))

data <- data %>%
  mutate(across(
    .cols = all_of(intersect(child_vars, names(data))), 
    .fns = ~if_else(HH_CHILD == "No", 
                    ifelse(is.numeric(.), NA_real_, "Not Applicable"), 
                    .)
  ))

final_missing <- colSums(is.na(data))
print(final_missing[final_missing > 0])

missing_change <- final_missing - initial_missing
print(missing_change[missing_change != 0])



# dropping coloumns with >50% missing values

n_rows <- nrow(data)
missing_percentages <- (colSums(is.na(data)) / n_rows) * 100

high_missing_cols <- missing_percentages[missing_percentages > 50]
high_missing_cols <- sort(high_missing_cols, decreasing = TRUE)


print(high_missing_cols)
print(paste("total columns with >50% missing values:", length(high_missing_cols)))



# flag them
high_missing_vars <- names(high_missing_cols)
print(paste("Number of variables flagged for exclusion:", length(high_missing_vars)))
print("Variables to be excluded:")
print(high_missing_vars)


#double check the missing values

#View(data)
#view(originaldata)



cols_to_remove <- c("WHYABSNT", "HH_NUMKIDS", "AGEYCHILD", 
                    "SPEARNWEEK", "SPUSUALHRS_DAILY_MIN")


print(paste("Original number of columns:", ncol(data)))
data <- data %>% select(-all_of(cols_to_remove))


#final check of the data

print(paste("Number of columns after removal:", ncol(data)))
print(paste("Removed", length(cols_to_remove), "columns with high missingness"))


# Verify these columns are no longer present
remaining_high_missing <- missing_percentages[missing_percentages > 50]
remaining_high_missing <- remaining_high_missing[names(remaining_high_missing) %in% names(data)]

if(length(remaining_high_missing) > 0) {
  print("Warning")
  print(remaining_high_missing)
} else {
  print("All good")
}





# ruuning mice
# WARNING: THIS IS A VERY LONG TIME TO RUN




remaining_missing <- colSums(is.na(data))
print(remaining_missing[remaining_missing > 0])


variable_types <- sapply(data, class)
print(table(variable_types))





# Convert all character variables to factors before method assignment

data[] <- lapply(data, function(x) {
  if (is.character(x)) return(as.factor(x))
  return(x)
})

table(sapply(data, class))





methods_vector <- rep("", ncol(data))
names(methods_vector) <- names(data)

for(i in 1:length(methods_vector)) {
  var_name <- names(methods_vector)[i]
  
  if(sum(is.na(data[[var_name]])) == 0) {
    methods_vector[i] <- ""
    next
  }
  
  if(is.numeric(data[[var_name]])) {
    methods_vector[i] <- "pmm"  # Predictive mean matching for numeric
  } else if(is.factor(data[[var_name]]) || is.character(data[[var_name]])) {
    unique_values <- unique(na.omit(data[[var_name]]))
    if(length(unique_values) == 2) {
      methods_vector[i] <- "logreg"  # Logistic regression for binary
    } else if(length(unique_values) > 2) {
      methods_vector[i] <- "polyreg"  # Polytomous regression for multi-class
    }
  }
}

# i had to redifne these myself because the mice function was not working them


methods_vector["EARNWEEK"] <- "cart"  
methods_vector["SPAGE"] <- ""
methods_vector["UHRSWORKT_DAILY_MIN"] <- "cart"

# Display methods assigned
print(methods_vector[methods_vector != ""])




pred_matrix <- matrix(1, ncol = ncol(data), nrow = ncol(data))
colnames(pred_matrix) <- names(data)
rownames(pred_matrix) <- names(data)
diag(pred_matrix) <- 

if("unpaid_labor" %in% names(data)) {
  pred_matrix[, "unpaid_labor"] <- 0
}



set.seed(42) 
mice_obj <- mice(data, 
                m = 20,               
                method = methods_vector,
                predictorMatrix = pred_matrix,
                maxit = 5,            
                printFlag = TRUE)     




print(summary(mice_obj))

data_complete <- complete(mice_obj, action = "long")
print(dim(data_complete))



save(mice_obj, file = "mice_imputation.RData")

print("MICE imputation completed successfully.")


# some final checks on the imputed data subset 1 2 3

d1 <- complete(mice_obj, 1)
d2 <- complete(mice_obj, 2)
d3 <- complete(mice_obj, 3)

summary(d1$EARNWEEK)
summary(d2$EARNWEEK)

table(d1$EARNWEEK == d2$EARNWEEK)





# it seems like the imputation is working well, let's move on to the next step 





# now we need to make the models and also incoaprate the survey design - weights back into our data



imp_datasets <- lapply(1:20, function(i) complete(mice_obj, i))

explanatory_vars <- c(
  "SEX", "AGE", "RACE4", "EDUC",  # Demographics
  "EMPSTAT", "FULLPART", "UHRSWORKT_DAILY_MIN", "EARNWEEK",  # Employment
  "HH_SIZE", "SPOUSEPRES", "HH_CHILD"  # Household
)




available_vars <- intersect(explanatory_vars, names(imp_datasets[[1]]))
formula_str <- paste("unpaid_labor ~", paste(available_vars, collapse = " + "))
print(paste("Model formula:", formula_str))

survey_designs <- lapply(imp_datasets, function(data) {
  svydesign(ids = ~1, weights = ~WT06, data = data)
})

models <- lapply(survey_designs, function(design) {
  svyglm(as.formula(formula_str), design = design)
})

combined_results <- MIcombine(models)
summary_table <- summary(combined_results)

print(names(summary_table))

results_table <- data.frame(
  Variable = rownames(summary_table),
  Coefficient = summary_table$results,
  SE = summary_table$se,
  t_value = summary_table$results / summary_table$se,
  lower_CI = summary_table$results - 1.96 * summary_table$se,
  upper_CI = summary_table$results + 1.96 * summary_table$se
)

df_value <- 100 
results_table$p_value <- 2 * pt(-abs(results_table$t_value), df = df_value)



results_table$significance <- ifelse(results_table$p_value < 0.001, "***",
                            ifelse(results_table$p_value < 0.01, "**",
                            ifelse(results_table$p_value < 0.05, "*", "")))

results_table <- results_table[order(-abs(results_table$t_value)),]
print(results_table)




# there is no wat to calculate the R-squared for survey models directly, so we will use pseudo-R-squared

pseudo_r2 <- sapply(models, function(m) {
  null_formula <- as.formula("unpaid_labor ~ 1")
  null_model <- svyglm(null_formula, design = m$survey.design)
  
  1 - (m$deviance / null_model$deviance)
})

avg_pseudo_r2 <- mean(pseudo_r2)
print(paste("Average pseudo R-squared:", round(avg_pseudo_r2, 3)))



print("Pseudo-R-squared for each imputed dataset:")
for (i in 1:length(pseudo_r2)) {
  print(paste("Imputation", i, ":", round(pseudo_r2[i], 3)))
}




print(summary(pseudo_r2))

hist(pseudo_r2, 
     main = "Distribution of Pseudo-R-squared Across Imputations",
     xlab = "Pseudo-R-squared", 
     col = "lightblue", 
     breaks = 10)






results_table$lower_CI <- results_table$Coefficient - 1.96 * results_table$SE
results_table$upper_CI <- results_table$Coefficient + 1.96 * results_table$SE

top_effects <- subset(results_table, Variable != "(Intercept)")[1:10,]

ggplot(top_effects, aes(x = reorder(Variable, t_value), y = Coefficient)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Significant Predictors of Unpaid Labor",
    x = "",
    y = "Effect on Unpaid Labor (minutes)"
  )

ggsave("top_effects_plot.png", width = 8, height = 6, dpi = 300)




# AFTER BASE MODEL WE MOVE TO THE ENGINEERED FEATURES
# THESE WERE MADE LIKE THIS BC FUNCTION "INTERSECRT" WAS CAUSING MULTIICOLLINEARITY

# creating features

imp_datasets_with_features <- lapply(1:20, function(i) {
  df <- complete(mice_obj, i)
  
  df$AGE_c <- scale(df$AGE, scale = FALSE)  
  df$AGE_SQUARED <- df$AGE_c^2  
  

  races <- levels(df$RACE4)
  for (race in races[-1]) { 
    var_name <- paste0("Male_", make.names(race))
    df[[var_name]] <- ifelse(df$SEX == "Male" & df$RACE4 == race, 1, 0)
  }
  
  edu_levels <- levels(df$EDUC)
  for (edu in edu_levels[-1]) { 
    var_name <- paste0("Male_", make.names(edu))
    df[[var_name]] <- ifelse(df$SEX == "Male" & df$EDUC == edu, 1, 0)
  }
  
  df$Male_WithChild <- ifelse(df$SEX == "Male" & df$HH_CHILD == "Yes", 1, 0)
  df$Female_WithChild <- ifelse(df$SEX == "Female" & df$HH_CHILD == "Yes", 1, 0)
  
  df$Male_WithSpouse <- ifelse(df$SEX == "Male" & df$SPOUSEPRES == "Spouse present", 1, 0)
  df$Female_WithSpouse <- ifelse(df$SEX == "Female" & df$SPOUSEPRES == "Spouse present", 1, 0)
  
  emp_statuses <- levels(df$EMPSTAT)
  for (status in emp_statuses[-1]) {  
    var_name <- paste0("Male_", make.names(status))
    df[[var_name]] <- ifelse(df$SEX == "Male" & df$EMPSTAT == status, 1, 0)
  }
  
  for (status in emp_statuses[-1]) {  
    var_name <- paste0(make.names(status), "_WithChild")
    df[[var_name]] <- ifelse(df$EMPSTAT == status & df$HH_CHILD == "Yes", 1, 0)
  }
  
  df$AGE_GROUP <- cut(df$AGE, breaks=c(15, 25, 35, 45, 55, 65, 100), 
                      labels=c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  
  df$WORK_HH_RATIO <- ifelse(df$HH_SIZE > 0, df$UHRSWORKT_DAILY_MIN/df$HH_SIZE, 0)
  df$LOG_EARN <- ifelse(df$EARNWEEK > 0, log(df$EARNWEEK), 0)
  
  df$EARN_CATEGORY <- cut(df$EARNWEEK, 
                          breaks=c(-1, 0, 500, 1000, 2000, 5000, 1000000), 
                          labels=c("No earnings", "Low", "Medium-low", "Medium", "Medium-high", "High"))
  
  df$HOUSEHOLD_TYPE <- case_when(
    df$HH_CHILD == "Yes" & df$SPOUSEPRES == "Spouse present" ~ "Married with children",
    df$HH_CHILD == "Yes" & df$SPOUSEPRES == "No spouse/partner present" ~ "Single parent",
    df$HH_CHILD == "No" & df$SPOUSEPRES == "Spouse present" ~ "Married without children",
    df$HH_CHILD == "No" & df$SPOUSEPRES == "No spouse/partner present" ~ "Single without children",
    TRUE ~ "Other"
  )
  
  df$FREE_TIME_PROXY <- 24*60 - df$UHRSWORKT_DAILY_MIN
  
  return(df)
})







# List new features
new_features <- setdiff(names(imp_datasets_with_features[[1]]), names(imp_datasets[[1]]))
print(new_features)


# HERE WE STANDARDIZE THE NUMERICAL PREDICTORS

# Features need standardization
numeric_predictors <- c("WORK_HH_RATIO", "FREE_TIME_PROXY", "HH_SIZE", "UHRSWORKT_DAILY_MIN", "LOG_EARN")

imp_datasets_with_features <- lapply(imp_datasets_with_features, function(df) {
  for (var in numeric_predictors) {
    if (var %in% names(df)) {
      df[[var]] <- scale(df[[var]])
      
      if (identical(df, imp_datasets_with_features[[1]])) {
        cat(paste("Standardized", var, "-> mean:", 
                 mean(df[[var]], na.rm=TRUE), 
                 "sd:", sd(df[[var]], na.rm=TRUE), "\n"))
      }
    }
  }
  return(df)
})



# WE MAKE SUERVEY DESIGN AGAIN

survey_designs_featured <- lapply(imp_datasets_with_features, function(data) {
  svydesign(ids = ~1, weights = ~WT06, data = data)
})


base_vars <- c("SEX", "AGE_c", "EMPSTAT", "FULLPART", "HH_SIZE")

interaction_vars <- c(
  "Male_WithChild", "Female_WithChild",
  "Male_WithSpouse", "Female_WithSpouse",
  "AGE_SQUARED",
  "Male_Black.African.American", "Male_Hispanic.Latino",
  "Male_Other...Mixed", "Male_White",
  "Male_HS.diploma..no.college", "Male_Less.than.HS.diploma",
  "Male_Some.college",
  "Male_Employed...at.work", "Male_Not.in.labor.force",
  "Male_Unemployed...looking", "Male_Unemployed...on.layoff",
  "Employed...at.work_WithChild", "Not.in.labor.force_WithChild",
  "Unemployed...looking_WithChild", "Unemployed...on.layoff_WithChild"
)

categorical_vars <- c("HOUSEHOLD_TYPE", "AGE_GROUP", "EARN_CATEGORY")
continuous_vars <- c("WORK_HH_RATIO", "LOG_EARN", "FREE_TIME_PROXY")

enhanced_vars <- c(base_vars, interaction_vars, categorical_vars, continuous_vars)
available_enhanced_vars <- intersect(enhanced_vars, names(imp_datasets_with_features[[1]]))

enhanced_formula <- paste("unpaid_labor ~", paste(available_enhanced_vars, collapse = " + "))
print(paste("Enhanced model formula:", enhanced_formula))






# NOW FITTING ENHANCED MODEL


enhanced_models <- lapply(survey_designs_featured, function(design) {
  svyglm(as.formula(enhanced_formula), design = design)
})

enhanced_combined <- MIcombine(enhanced_models)
enhanced_summary <- summary(enhanced_combined)


enhanced_r2 <- sapply(enhanced_models, function(m) {
  null_formula <- as.formula("unpaid_labor ~ 1")
  null_model <- svyglm(null_formula, design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

avg_enhanced_r2 <- mean(enhanced_r2)
print(paste("Enhanced model average pseudo R-squared:", round(avg_enhanced_r2, 3)))


# Display individual R-squared values

for (i in 1:length(enhanced_r2)) {
  print(paste("Imputation", i, ":", round(enhanced_r2[i], 3)))
}

print("=== Model Performance Comparison ===")
print(paste("Original model average pseudo-R²:", round(avg_pseudo_r2, 3)))
print(paste("Enhanced model average pseudo-R²:", round(avg_enhanced_r2, 3)))
print(paste("Improvement:", round((avg_enhanced_r2 - avg_pseudo_r2) * 100, 3), "percentage points"))


enhanced_results <- data.frame(
  Variable = rownames(enhanced_summary),
  Coefficient = enhanced_summary$results,
  SE = enhanced_summary$se,
  t_value = enhanced_summary$results / enhanced_summary$se,
  lower_CI = enhanced_summary$results - 1.96 * enhanced_summary$se,
  upper_CI = enhanced_summary$results + 1.96 * enhanced_summary$se
)

enhanced_results$p_value <- 2 * pt(-abs(enhanced_results$t_value), df = 100)
enhanced_results$significance <- ifelse(enhanced_results$p_value < 0.001, "***",
                                ifelse(enhanced_results$p_value < 0.01, "**",
                                ifelse(enhanced_results$p_value < 0.05, "*", "")))
enhanced_results <- enhanced_results[order(-abs(enhanced_results$t_value)),]

print(enhanced_results)



# Creating visualization of top effects \

top_enhanced_effects <- subset(enhanced_results, Variable != "(Intercept)")[1:10,]

ggplot(top_enhanced_effects, aes(x = reorder(Variable, t_value), y = Coefficient)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower_CI, ymax = upper_CI), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Most Significant Predictors of Unpaid Labor (Enhanced Model)",
    subtitle = paste("Pseudo-R²:", round(avg_enhanced_r2, 3), 
                    "(Improvement:", round((avg_enhanced_r2 - avg_pseudo_r2) * 100, 1), "percentage points)"),
    x = "",
    y = "Effect on Unpaid Labor (minutes)"
  )

ggsave("top_enhanced_effects_plot.png", width = 8, height = 6, dpi = 300)




# WE DONT HAVE NORMAL RESIDUALS, SO WE TRY ANOTHER MODEL


# WE TRANSFORM THE DEPENDENT VARIABLE TO SEE IF WE CAN GET BETTER RESULTS


imp_datasets_transformed <- lapply(imp_datasets_with_features, function(data) {
  data$log_unpaid_labor <- log(data$unpaid_labor + 1)
  
  data$sqrt_unpaid_labor <- sqrt(data$unpaid_labor)
  
  return(data)
})

survey_designs_transformed <- lapply(imp_datasets_transformed, function(data) {
  svydesign(ids = ~1, weights = ~WT06, data = data)
})

log_models <- lapply(survey_designs_transformed, function(design) {
  svyglm(as.formula(paste("log_unpaid_labor ~", paste(available_enhanced_vars, collapse = " + "))), 
         design = design)
})

log_combined <- MIcombine(log_models)
log_summary <- summary(log_combined)

log_r2 <- sapply(log_models, function(m) {
  null_formula <- as.formula("log_unpaid_labor ~ 1")
  null_model <- svyglm(null_formula, design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

print(paste("Log-transformed model R²:", round(mean(log_r2), 3)))


sqrt_models <- lapply(survey_designs_transformed, function(design) {
  svyglm(as.formula(paste("sqrt_unpaid_labor ~", paste(available_enhanced_vars, collapse = " + "))), 
         design = design)
})

sqrt_combined <- MIcombine(sqrt_models)
sqrt_summary <- summary(sqrt_combined)

sqrt_r2 <- sapply(sqrt_models, function(m) {
  null_formula <- as.formula("sqrt_unpaid_labor ~ 1")
  null_model <- svyglm(null_formula, design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

print(paste("Square root-transformed model R²:", round(mean(sqrt_r2), 3)))


print("\n=== Model Comparison: All Transformations ===")
model_comparison <- data.frame(
    Transformation = c("Original (untransformed)", "Enhanced features", 
                                        "Log transformation", "Square root transformation"),
    R_squared = c(avg_pseudo_r2, avg_enhanced_r2, 
                             mean(log_r2), mean(sqrt_r2)),
    Improvement = c(0, 
                                 (avg_enhanced_r2 - avg_pseudo_r2) * 100,
                                 (mean(log_r2) - avg_pseudo_r2) * 100,
                                 (mean(sqrt_r2) - avg_pseudo_r2) * 100)
)

model_comparison$R_squared <- round(model_comparison$R_squared, 3)
model_comparison$Improvement <- round(model_comparison$Improvement, 1)
model_comparison$Improvement <- paste(model_comparison$Improvement, "percentage points")

print(model_comparison)

barplot(model_comparison$R_squared, 
                names.arg = model_comparison$Transformation,
                main = "Comparison of Model R² Values by Transformation",
                xlab = "Transformation Method", 
                ylab = "Pseudo-R² Value",
                col = rainbow(nrow(model_comparison)),
                las = 2,  
                cex.names = 0.8)  


abline(h = avg_pseudo_r2, lty = 2, col = "black")
text(1, avg_pseudo_r2 + 0.01, "Original model baseline", pos = 3, cex = 0.7)

best_model_index <- which.max(model_comparison$R_squared)
print(paste("Best model: ", model_comparison$Transformation[best_model_index], 
                        "with R² =", model_comparison$R_squared[best_model_index]))






# compare residual normality for the best model


if(best_model_index == 3) {  
    best_models <- log_models
    outcome_name <- "log_unpaid_labor"
} else if(best_model_index == 4) { 
    best_models <- sqrt_models
    outcome_name <- "sqrt_unpaid_labor"
} else {  
    best_models <- enhanced_models
    outcome_name <- "unpaid_labor"
}

best_model <- best_models[[1]]  
resids <- residuals(best_model)
qqnorm(resids, main=paste("Q-Q Plot of Residuals for", 
                                                 model_comparison$Transformation[best_model_index]))
qqline(resids, col="red")

png("best_model_qqplot.png", width=800, height=600, res=100)
qqnorm(resids, main=paste("Q-Q Plot of Residuals for", 
             model_comparison$Transformation[best_model_index]))
qqline(resids, col="red")
dev.off()


hist(resids, breaks=30, 
     main=paste("Histogram of Residuals for", 
               model_comparison$Transformation[best_model_index]),
     xlab="Residuals")

png("best_model_residuals_histogram.png", width=800, height=600, res=100)
hist(resids, breaks=30, 
   main=paste("Histogram of Residuals for", 
        model_comparison$Transformation[best_model_index]),
   xlab="Residuals")
dev.off()



############################################################################
# two part model for unpaid labor - failed
zero_prop <- mean(imp_datasets_with_features[[1]]$unpaid_labor == 0)
print(paste("Proportion of zeros in unpaid labor:", round(zero_prop, 3)))

imp_datasets_twopart <- lapply(imp_datasets_with_features, function(data) {
  data$any_unpaid <- ifelse(data$unpaid_labor > 0, 1, 0)
  return(data)
})

survey_designs_twopart <- lapply(imp_datasets_twopart, function(data) {
  svydesign(ids = ~1, weights = ~WT06, data = data)
})

part1_models <- lapply(survey_designs_twopart, function(design) {
  svyglm(as.formula(paste("any_unpaid ~", paste(available_enhanced_vars, collapse = " + "))),
         family = quasibinomial(),
         design = design)
})

part2_models <- lapply(1:length(imp_datasets_twopart), function(i) {
  data_subset <- subset(imp_datasets_twopart[[i]], unpaid_labor > 0)
  design_subset <- svydesign(ids = ~1, weights = ~WT06, data = data_subset)
  
  svyglm(as.formula(paste("unpaid_labor ~", paste(available_enhanced_vars, collapse = " + "))),
         family = Gamma(link = "log"),
         design = design_subset)
})

part1_r2 <- sapply(part1_models, function(m) {
  null_formula <- as.formula("any_unpaid ~ 1")
  null_model <- svyglm(null_formula, family = quasibinomial(), design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

part2_r2 <- sapply(part2_models, function(m) {
  null_formula <- as.formula("unpaid_labor ~ 1")
  null_model <- svyglm(null_formula, family = Gamma(link = "log"), design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

print(paste("Part 1 (Participation) R²:", round(mean(part1_r2), 3)))
print(paste("Part 2 (Amount) R²:", round(mean(part2_r2), 3)))

############################################################################


# DEALING WITH OUTLIERS


summary(imp_datasets_with_features[[1]]$unpaid_labor)

png("unpaid_labor_boxplot.png", width=800, height=600, res=100)
boxplot(imp_datasets_with_features[[1]]$unpaid_labor, 
  main="Unpaid Labor Distribution", 
  ylab="Minutes per day",
  outline=TRUE)
dev.off()



q1 <- quantile(imp_datasets_with_features[[1]]$unpaid_labor, 0.25)
q3 <- quantile(imp_datasets_with_features[[1]]$unpaid_labor, 0.75)
iqr <- q3 - q1
upper_bound <- q3 + 1.5 * iqr

outlier_count <- sum(imp_datasets_with_features[[1]]$unpaid_labor > upper_bound)
outlier_percent <- (outlier_count / length(imp_datasets_with_features[[1]]$unpaid_labor)) * 100

print(paste("Number of outliers:", outlier_count))
print(paste("Percentage of data:", round(outlier_percent, 2), "%"))
print(paste("Maximum value:", max(imp_datasets_with_features[[1]]$unpaid_labor), "minutes"))





imp_datasets_winsorized <- lapply(1:20, function(i) {
  data_i <- imp_datasets_with_features[[i]]
  
  p95 <- quantile(data_i$unpaid_labor, 0.95)
  p99 <- quantile(data_i$unpaid_labor, 0.99)
  
  data_i$unpaid_labor_w95 <- pmin(data_i$unpaid_labor, p95)
  data_i$unpaid_labor_w99 <- pmin(data_i$unpaid_labor, p99)
  
  data_i$sqrt_unpaid_w95 <- sqrt(data_i$unpaid_labor_w95)
  data_i$sqrt_unpaid_w99 <- sqrt(data_i$unpaid_labor_w99)
  
  if(i == 1) {
    print(paste("95th percentile cap:", round(p95), "minutes"))
    print(paste("99th percentile cap:", round(p99), "minutes"))
    print(paste("Original max:", round(max(data_i$unpaid_labor)), "minutes"))
  }
  
  return(data_i)
})
survey_designs_winsorized <- lapply(imp_datasets_winsorized, function(data) {
  svydesign(ids = ~1, weights = ~WT06, data = data)
})


# 95th percentile winsorized model
w95_sqrt_models <- lapply(survey_designs_winsorized, function(design) {
  svyglm(as.formula(paste("sqrt_unpaid_w95 ~", paste(available_enhanced_vars, collapse = " + "))), 
         design = design)
})

# 99th percentile winsorized model
w99_sqrt_models <- lapply(survey_designs_winsorized, function(design) {
  svyglm(as.formula(paste("sqrt_unpaid_w99 ~", paste(available_enhanced_vars, collapse = " + "))), 
         design = design)
})


w95_sqrt_r2 <- sapply(w95_sqrt_models, function(m) {
  null_formula <- as.formula("sqrt_unpaid_w95 ~ 1")
  null_model <- svyglm(null_formula, design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

w99_sqrt_r2 <- sapply(w99_sqrt_models, function(m) {
  null_formula <- as.formula("sqrt_unpaid_w99 ~ 1")
  null_model <- svyglm(null_formula, design = m$survey.design)
  1 - (m$deviance / null_model$deviance)
})

print(paste("95% Winsorized Sqrt model R²:", round(mean(w95_sqrt_r2), 3)))
print(paste("99% Winsorized Sqrt model R²:", round(mean(w99_sqrt_r2), 3)))
print(paste("Original Sqrt model R² (for comparison):", round(mean(sqrt_r2), 3)))



model_comparison_updated <- rbind(
  model_comparison,
  data.frame(
    Transformation = c("95% Winsorized + Sqrt", "99% Winsorized + Sqrt"),
    R_squared = c(round(mean(w95_sqrt_r2), 3), round(mean(w99_sqrt_r2), 3)),
    Improvement = paste(
      round(c((mean(w95_sqrt_r2) - avg_pseudo_r2) * 100, 
              (mean(w99_sqrt_r2) - avg_pseudo_r2) * 100), 1), 
      "percentage points")
  )
)

print(model_comparison_updated)


if(mean(w95_sqrt_r2) > mean(w99_sqrt_r2)) {
  best_win_models <- w95_sqrt_models
  best_win_var <- "sqrt_unpaid_w95"
  best_win_name <- "95% Winsorized + Sqrt"
} else {
  best_win_models <- w99_sqrt_models
  best_win_var <- "sqrt_unpaid_w99"
  best_win_name <- "99% Winsorized + Sqrt"
}

# Compare residuals
best_win_model <- best_win_models[[1]]
win_resids <- residuals(best_win_model)

par(mfrow=c(1,2))
qqnorm(resids, main=paste("Original Sqrt Model\nQ-Q Plot of Residuals"))
qqline(resids, col="red")

qqnorm(win_resids, main=paste(best_win_name, "\nQ-Q Plot of Residuals"))
qqline(win_resids, col="red")

par(mfrow=c(1,2))
hist(resids, breaks=30, main="Original Sqrt Model\nResidual Distribution", xlab="Residuals")
hist(win_resids, breaks=30, main=paste(best_win_name, "\nResidual Distribution"), xlab="Residuals")


# MAY THE BEST MODEL WIN

all_r2 <- c(
  "Original" = avg_pseudo_r2,
  "Enhanced" = avg_enhanced_r2,
  "Log" = mean(log_r2),
  "Sqrt" = mean(sqrt_r2),
  "W95_Sqrt" = mean(w95_sqrt_r2),
  "W99_Sqrt" = mean(w99_sqrt_r2)
)

best_overall <- names(all_r2)[which.max(all_r2)]
print(paste("BEST MODEL OVERALL:", best_overall, "with R² =", round(max(all_r2), 3)))

best_win_combined <- MIcombine(best_win_models)
best_win_summary <- summary(best_win_combined)

best_win_results <- data.frame(
  Variable = rownames(best_win_summary),
  Coefficient = best_win_summary$results,
  SE = best_win_summary$se,
  t_value = best_win_summary$results / best_win_summary$se,
  lower_CI = best_win_summary$results - 1.96 * best_win_summary$se,
  upper_CI = best_win_summary$results + 1.96 * best_win_summary$se
)

best_win_results$p_value <- 2 * pt(-abs(best_win_results$t_value), df = 100)
best_win_results$significance <- ifelse(best_win_results$p_value < 0.001, "***",
                                ifelse(best_win_results$p_value < 0.01, "**",
                                ifelse(best_win_results$p_value < 0.05, "*", "")))

best_win_results <- best_win_results[order(-abs(best_win_results$t_value)),]

print(paste("Effects of variables on", best_win_name, "(sorted by significance):"))
print(head(best_win_results, 10)) 





##########################################################
# PAHSE 4.2 LAST 2 MODELS
#########################################################




# Reclassify unpaid labor into 3 classes
mice_classified <- mice_obj  

classify_unpaid <- function(x) {
  case_when(
    x >= 240 ~ "overworked",
    x <= 30 ~ "underworked",
    TRUE ~ "normal"
  )
}

classification_datasets <- lapply(1:20, function(i) {
  df <- complete(mice_classified, i)
  df$class_label <- classify_unpaid(df$unpaid_labor)
  df <- df %>% filter(!is.na(class_label))
  df$class_label <- as.factor(df$class_label)
  df <- df %>% select(-unpaid_labor, -BLS_COMM, -BLS_EDUC, -BLS_FOOD, -BLS_LEIS, -BLS_PCARE, -BLS_SOCIAL, -BLS_WORK, -BLS_HHACT, -BLS_PURCH, -BLS_CAREHH, -BLS_CARENHH)  # drop original continuous outcome
  return(df)
})



split_datasets <- lapply(classification_datasets, function(df) {
  set.seed(42)
  train_index <- createDataPartition(df$class_label, p = 0.8, list = FALSE)
  list(
    train = df[train_index, ],
    test = df[-train_index, ]
  )
})






rf_models <- list()
rf_results <- list()
rf_tuning_summary <- list()

for (i in 1:20) {
  cat("Training Random Forest model for imputation", i, "...
")
  split_data <- split_datasets[[i]]
  train_data <- split_data$train
  test_data <- split_data$test
  
  ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
  rf_tuned <- train(
    class_label ~ ., data = train_data %>% select(-WT06) %>% drop_na(),
    method = "rf",
    trControl = ctrl,
    tuneLength = 5
  )
  
  pred_input <- test_data %>% select(-WT06, -class_label) %>% drop_na()
  actual_labels <- test_data$class_label[complete.cases(test_data %>% select(-WT06, -class_label))]
  
  preds <- predict(rf_tuned, newdata = pred_input)
  conf_mat <- confusionMatrix(preds, actual_labels)
  
  rf_models[[i]] <- rf_tuned$finalModel
  rf_results[[i]] <- list(
    accuracy = conf_mat$overall['Accuracy'],
    confusion = conf_mat$table,
    importance = varImp(rf_tuned)$importance,
    pred = preds,
    truth = actual_labels
  )
  
  rf_tuning_summary[[i]] <- rf_tuned$results[order(-rf_tuned$results$Accuracy), ]
}





rf_accuracies <- sapply(rf_results, function(r) r$accuracy)
cat("\n--- Summary of Classification Results ---\n")
cat("Random Forest Accuracy: ", round(mean(rf_accuracies), 3), "±", round(sd(rf_accuracies), 3), "\n")

conf_example <- rf_results[[1]]$confusion
print(conf_example)

xgb_results <- list()
xgb_tuning_summary <- list()

for (i in 1:20) {
  split_data <- split_datasets[[i]]
  train_data <- split_data$train
  test_data <- split_data$test
  
  recipe_xgb <- recipe(class_label ~ ., data = train_data %>% select(-WT06)) %>%
    step_dummy(all_nominal_predictors()) %>%
    prep()
  
  x_train <- bake(recipe_xgb, train_data) %>% select(-class_label)
  y_train <- train_data$class_label
  x_test <- bake(recipe_xgb, test_data) %>% select(-class_label)
  y_test <- test_data$class_label
  
  ctrl <- trainControl(method = "cv", number = 5)
  tune_grid <- expand.grid(
    nrounds = c(50, 100),
    eta = c(0.05, 0.1, 0.3),
    max_depth = c(3, 6),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  cat("Training XGBoost model for imputation", i, "...
")
  xgb_tuned <- train(
    x = x_train,
    y = y_train,
    method = "xgbTree",
    trControl = ctrl,
    tuneGrid = tune_grid,
    verbose = TRUE
  )
  
  preds <- predict(xgb_tuned, newdata = x_test)
  accuracy <- mean(preds == y_test)
  
  xgb_results[[i]] <- list(
    accuracy = accuracy,
    truth = y_test,
    pred = preds
  )
  
  xgb_tuning_summary[[i]] <- xgb_tuned$results[order(-xgb_tuned$results$Accuracy), ]
} 

xgb_accuracies <- sapply(xgb_results, function(r) r$accuracy)
cat("XGBoost Accuracy: ", round(mean(xgb_accuracies), 3), "±", round(sd(xgb_accuracies), 3), "\n")

# Optional: Plot pooled confusion matrix for one of the models
conf_example <- xgb_results[[1]]$confusion
print(conf_example)





# Per-class precision, recall, F1-score and heatmap (RF model example)

rf_preds_df <- data.frame(
  pred = rf_results[[1]]$pred,
  truth = rf_results[[1]]$truth
)

metrics_tbl <- rf_preds_df %>%
  metrics(truth = truth, estimate = pred)
print(metrics_tbl)

class_metrics <- rf_preds_df %>%
  group_by(truth) %>%
  summarise(
    precision = precision_vec(truth, pred, estimator = "macro"),
    recall = recall_vec(truth, pred, estimator = "macro"),
    f1 = f_meas_vec(truth, pred, estimator = "macro")
  )
print(class_metrics)

# Confusion matrix heatmap


cm_matrix <- as.data.frame(confusionMatrix(rf_results[[1]]$pred, rf_results[[1]]$truth)$table)
colnames(cm_matrix) <- c("Prediction", "Reference", "Freq")

ggplot(cm_matrix, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  theme_minimal() +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted")


xgb_preds_df <- data.frame(
  pred = xgb_results[[1]]$pred,
  truth = xgb_results[[1]]$truth
)

metrics_tbl <- xgb_preds_df %>%
  metrics(truth = truth, estimate = pred)
print(metrics_tbl)

class_metrics <- xgb_preds_df %>%
  group_by(truth) %>%
  summarise(
    precision = precision_vec(truth, pred, estimator = "macro"),
    recall = recall_vec(truth, pred, estimator = "macro"),
    f1 = f_meas_vec(truth, pred, estimator = "macro")
  )
print(class_metrics)

# Confusion matrix heatmap

cm_matrix <- as.data.frame(confusionMatrix(xgb_results[[1]]$pred, xgb_results[[1]]$truth)$table)
colnames(cm_matrix) <- c("Prediction", "Reference", "Freq")

ggplot(cm_matrix, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 4) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  theme_minimal() +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted")


# Stop parallel cluster after modeling


stopCluster(cl)
registerDoSEQ()
rf_accuracies <- sapply(rf_results, function(r) r$accuracy)
xgb_accuracies <- sapply(xgb_results, function(r) r$accuracy)

cat("\n--- Summary of Classification Results ---\n")
cat("Random Forest Accuracy: ", round(mean(rf_accuracies), 3), "±", round(sd(rf_accuracies), 3), "\n")
cat("XGBoost Accuracy: ", round(mean(xgb_accuracies), 3), "±", round(sd(xgb_accuracies), 3), "\n")


# Variable importance example
varImpPlot(rf_models[[1]], main = "Random Forest - Variable Importance (First Imputation)")





# Step 6A: Final Training on Combined Actual Train Set Using Best Hyperparameters
final_train_data <- do.call(rbind, lapply(split_datasets, function(x) x$train))
final_test_data  <- do.call(rbind, lapply(split_datasets, function(x) x$test))


best_rf_mtry <- as.integer(names(sort(table(sapply(rf_tuning_summary, function(df) df$mtry[1])), decreasing = TRUE))[1])
final_rf <- randomForest(
  class_label ~ .,
  data = final_train_data %>% select(-WT06)  %>% drop_na(),
  mtry = best_rf_mtry,
  importance = TRUE
)
rf_pred <- predict(final_rf, newdata = final_test_data %>% select(-WT06, -class_label))
rf_conf <- confusionMatrix(rf_pred, final_test_data$class_label)

xgb_top <- do.call(rbind, lapply(xgb_tuning_summary, function(df) df[1, ]))
tunegrid_best = data.frame(
  nrounds = 100,
  eta = 0.1,
  max_depth = 3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1)


recipe_final_xgb <- recipe(class_label ~ ., data = final_train_data %>% select(-WT06)) %>% step_dummy(all_nominal_predictors()) %>% prep()
x_train_final <- bake(recipe_final_xgb, final_train_data) %>% select(-class_label)
y_train_final <- final_train_data$class_label
x_test_final  <- bake(recipe_final_xgb, final_test_data) %>% select(-class_label)
y_test_final  <- final_test_data$class_label

final_xgb <- train(
  x = x_train_final,
  y = y_train_final,
  method = "xgbTree",
  trControl = trainControl(method = "none"),
  verbose = TRUE,
  tuneGrid = tunegrid_best,
)
xgb_pred <- predict(final_xgb, x_test_final)
xgb_conf <- confusionMatrix(xgb_pred, y_test_final)


# Final Model Comparison 


cat("
--- Final Model Comparison on Actual Test Set ---
")
cat("Final RF Accuracy:", round(rf_conf$overall['Accuracy'], 3), "
")
cat("Final XGB Accuracy:", round(xgb_conf$overall['Accuracy'], 3), "
")































































