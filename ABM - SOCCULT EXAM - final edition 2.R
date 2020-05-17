# SOCIAL AND CULTURAL DYNAMICS IN COGNITION EXAM SPRING 2020 #
# Diagnostic disparity in women with Asperger's Syndrome #
# Bella Terragni #


############## SECTION 0 - set-up ############

# load packages
pacman::p_load(dplyr, tidyr, MASS, tidyverse, psych, viridis, wesanderson)

# set theme for visualizations
theme_set(theme_minimal())



############## SECTION 1 - functions for simulation ############

# In this section, I define the general functions utilized when running the simulation

# Function that assigns individuals with ASD if they have n symptoms or more above a set value (severity_level)
# takes four parameters: the dataset with agents and their assigned symptom severities, number of possible symptoms,
# a value for the severity level needed for a symptom to be clinically present, and a cut-off value for number of
# symptoms needed before an individual clinically speaking has ASD.
# I set default values, since I use the same values throughout the simulation, but want to retain the flexibility
# to run simulations with other values
# NB: this is the function that determines the "true" dagnosis of the individuals that the diagnostic_model() function later tries to guess
assign_ASD <- function(D_all, n_symptoms = 9, severity_level = 0.4, n_symptom_cutoff = 6){
  D_all$symptoms <- "N/A"
  D_all$ASD <- "N/A"
  for (i in 1:(nrow(D_all))){
    symp_count <- 0
    for (j in 1:n_symptoms){
      if (D_all[[i, 2+j]] > severity_level){
        symp_count = symp_count + 1
      }}
    D_all[i,"symptoms"] <- symp_count
    if (D_all$symptoms[[i]] >= n_symptom_cutoff){
      D_all[i,"ASD"] <- 1
    }
    else {
      D_all[i,"ASD"] <- 0
    }
  }
  # converting factors to numerics
  D_all$symptoms <- as.numeric(as.character(D_all$symptoms))
  D_all$ASD <- as.numeric(as.character(D_all$ASD))
  return(D_all)
}


# function that determines which symptoms are the most prominent (based on mean) in a specific population
# the function takes 5 parameters: full dataset, sample size of group for diagnostic updating, number of diagnostic criteria,
# percentage of males in diagnostic updating sample (decimals), and a subset criterium. If no subset is needed, use default.
# function returns vector of n symptoms used for diagnostic criteria
diagnostic_criteria_model <- function(full_dataset, sample_size, n_diagnostic_criteria, male_percent, subset_criterium1 = T) {
  # subset specific group with ASD diagnosis
  diagnostic_group_m <- full_dataset %>% filter(subset_criterium1) %>% filter(gender == "M")
  diagnostic_group_f <- full_dataset %>% filter(subset_criterium1) %>% filter(gender == "F")
  # sample randomly from groups
  # diagnostic_sample <- diagnostic_group_m[sample(nrow(diagnostic_group_m), sample_size), ]
  diagnostic_sample_m <- diagnostic_group_m[sample(nrow(diagnostic_group_m), sample_size*male_percent), ]
  diagnostic_sample_f <- diagnostic_group_f[sample(nrow(diagnostic_group_f), sample_size*(1-male_percent)), ]
  diagnostic_sample <- rbind(diagnostic_sample_m, diagnostic_sample_f)
  # calculate mean symptom severity
  symptom_severity <- data.frame(S1 = mean(diagnostic_sample$S1), S2 = mean(diagnostic_sample$S2), S3 = mean(diagnostic_sample$S3), S4 = mean(diagnostic_sample$S4), S5 = mean(diagnostic_sample$S5), S6 = mean(diagnostic_sample$S6), S7 = mean(diagnostic_sample$S7), S8 = mean(diagnostic_sample$S8), S9 = mean(diagnostic_sample$S9))
  # sort symptoms
  symptom_severity <- gather(symptom_severity)
  symptom_severity <- symptom_severity[order(-symptom_severity$value), ]
  # subset the highest ranking symptoms (uses parameter value)
  diagnostic_criteria <- symptom_severity[1:n_diagnostic_criteria, ]
  # make vector with diagnostic criteria symptoms
  diagnostic_criteria <- c(diagnostic_criteria[,1])
  return(diagnostic_criteria)
}


# function that diagnoses people with ASD if they have n_symptom_cutoff or more of the diagnostic criteria
# takes full_dataset, diagnostic_criteria vector from diagnostic_criteria_model() and a set number for n_symptom_cutoff 
diagnostic_model <- function(full_dataset, diagnostic_criteria, n_symptom_cutoff){
  round <- ((ncol(full_dataset)) - 12)
  round <- as.character(round)
  for (i in 1:nrow(full_dataset)){
    diagnostic_symp_count <- 0
    for (j in 1:length(diagnostic_criteria)){
      symptom <- diagnostic_criteria[j]
      if (full_dataset[i,symptom] > 0.5){
        diagnostic_symp_count = diagnostic_symp_count + 1
      }}
    if (diagnostic_symp_count >= n_symptom_cutoff){
      full_dataset[i,round] <- 1
    } else {
      full_dataset[i,round] <- 0
    } }
  names(full_dataset)[names(full_dataset) == round] <- paste("ASD", round, sep = "")
  return(full_dataset)
}


# function that calculates the amount of false negatives and positives, as well as true negatives and postitives in set
# takes the full dataset from diagnostic_model() as a parameter
evaluate_diagnosis <- function(full_dataset) {
  full_dataset <- data.frame(full_dataset, stringsAsFactors = FALSE)
  evaluation_data <- data.frame()
  for (j in c(1:(ncol(full_dataset)-13))){
    round <- paste("ASD", j, sep = "")
    eval <- paste("Eval", j, sep = "")
    for (i in 1:nrow(full_dataset)){
      if (full_dataset[i,"ASD"] == 1 & full_dataset[i,round] == 1){
        evaluation_data[i, j] <- "TP"
      }
      if (full_dataset[i, "ASD"] == 1 & full_dataset[i,round] == 0){
        evaluation_data[i, j] <- "FN"
      }
      if (full_dataset[i, "ASD"] == 0 & full_dataset[i, round] == 0){
        evaluation_data[i, j] <- "TN"
      }
      if (full_dataset[i,"ASD"] == 0 & full_dataset[i, round] == 1){
        evaluation_data[i, j] <- "FP"
      }}
  }
  return(evaluation_data)
}


# Function that combines and runs the 3 functions above (diagnostic_criteria_model(), diagnostic_model(), and evaluate_diagnosis()) for n iterations
# Takes the parameters: full dataset, number of iterations, size diagnostic updating sample, number of diagnostic symptoms, number of symptoms
# needed to get diagnsed (n_symptom_cutoff), as well as the percentage of males in the diagnostic analysis group (decimal).
# Function returns list of data frames. The first data frame contains the raw simulation data, the second contains the diagnostic vectors used for
# each iteration, and the third dataframe contains the evaluation of the diagnosis
run_simulation <- function(full_dataset, iterations, n_sample, n_diagnostic_symptoms, n_symptom_cutoff, male_percent){
  # run first round, possible individual subset criterium
  test_vec1 <- diagnostic_criteria_model(full_dataset,  n_sample, n_diagnostic_symptoms, 1, full_dataset$ASD == 1)
  test_data1 <- diagnostic_model(full_dataset, test_vec1, n_symptom_cutoff)
  # create data frame for diagnostic vectors
  diagnostic_vector_data <- data.frame("round" = c(test_vec1))
  # run x iterations
  for (i in (1:iterations)){
    test_vec2 <- diagnostic_criteria_model(test_data1, n_sample, n_diagnostic_symptoms, male_percent, (test_data1[,i+13]) == 1)
    test_data1 <- diagnostic_model(test_data1, test_vec2, n_symptom_cutoff)
    # add new symptoms to diagnostic vector dataframe
    diagnostic_vector_data <- data.frame(diagnostic_vector_data, round = test_vec2)
  }
  eval <- evaluate_diagnosis(test_data1)
  gender <- test_data1[,"gender"]
  #ASD <- test_data1[,"ASD"] # REMOVE
  eval <- cbind(gender, eval)
  # bind results in list of dataframes
  results <- list(test_data1, diagnostic_vector_data, eval)
  return(results)
}



############# SECTION 2 - functions for evaluating outcome ##################

# in this section I define functions for restructuring and evaluating the data from a simulation of n iterations

# Function for determining amount of FP's, FN's, TP's, and TN's between the genders for every iteration
# takes one parameter: the evaluation dataframe from run_simulation()
# Returns list, where first element is the long format dataframe evaluating number of TP's, FP's, TN's, and FN's
# in each iteration of the algorithm. List element 2 is exlusively male individuals, and element 3 is females
evaluation_long <- function(evaluation_df){
  # males
  m_data <- evaluation_df %>% filter(gender == "M")
  evaluation_m <- m_data[,2:(ncol(m_data))]
  count_data_m <- data.frame()
  for (column in 1:ncol(evaluation_m)){
    iteration <- evaluation_m[,column]
    count_table <- iteration %>% unlist() %>% table()
    if (any("FP" == as.data.frame(count_table))){
      temp_data <- data.frame(FN = count_table[["FN"]], FP = count_table[["FP"]], TN = count_table[["TN"]], TP = count_table[["TP"]])
    }
    else {
    temp_data <- data.frame(FN = count_table[["FN"]], FP = 0, TN = count_table[["TN"]], TP = count_table[["TP"]])
    }
    count_data_m <- rbind(count_data_m, temp_data)
  }
  count_data_m <- data.frame(count_data_m, gender = c("M"))
  # females
  f_data <- evaluation_df %>% filter(gender == "F")
  evaluation_f <- f_data[,2:(ncol(f_data))]
  count_data_f <- data.frame()
  for (column in 1:ncol(evaluation_f)){
    iteration <- evaluation_f[,column]
    count_table <- iteration %>% unlist() %>% table()
    if (any("FP" == as.data.frame(count_table))){
      temp_data <- data.frame(FN = count_table[["FN"]], FP = count_table[["FP"]], TN = count_table[["TN"]], TP = count_table[["TP"]])
    }
    else {
    temp_data <- data.frame(FN = count_table[["FN"]], FP = 0, TN = count_table[["TN"]], TP = count_table[["TP"]])
    }
    count_data_f <- rbind(count_data_f, temp_data)
  }
  count_data_f <- data.frame(count_data_f, gender = c("F"))
  # restructure
  all_count_data_long <- rbind(count_data_f, count_data_m)
  id_int <- nrow(all_count_data_long)/2
  id_vec <- c(1:id_int,1:id_int)
  all_count_data_long <- cbind(all_count_data_long, id_vec)
  # create list of data frames
  count_list <- list(all_count_data_long, count_data_m, count_data_f)
  
  return(count_list)
}


# Function virtually identical to count_evaluation_long() except it returns the evaluation in (a type of) wide format used for plotting
evaluation_wide <- function(evaluation_df){
  # males
  m_data <- evaluation_df %>% filter(gender == "M")
  evaluation_m <- m_data[,2:(ncol(m_data))]
  count_data_m <- data.frame()
  for (column in 1:ncol(evaluation_m)){
    iteration <- evaluation_m[,column]
    count_table <- iteration %>% unlist() %>% table()
    if (any("FP" == as.data.frame(count_table))){
      temp_data <- data.frame(FN_m = count_table[["FN"]], FP_m = count_table[["FP"]], TN_m = count_table[["TN"]], TP_m = count_table[["TP"]])
    }
    else {
      temp_data <- data.frame(FN_m = count_table[["FN"]], FP_m = 0, TN_m = count_table[["TN"]], TP_m = count_table[["TP"]])
    }
    count_data_m <- rbind(count_data_m, temp_data)
  }
  count_data_m <- data.frame(count_data_m, gender = c("M"))
  # females
  f_data <- evaluation_df %>% filter(gender == "F")
  evaluation_f <- f_data[,2:(ncol(f_data))]
  count_data_f <- data.frame()
  for (column in 1:ncol(evaluation_f)){
    iteration <- evaluation_f[,column]
    count_table <- iteration %>% unlist() %>% table()
    if (any("FP" == as.data.frame(count_table))){
      temp_data <- data.frame(FN_f = count_table[["FN"]], FP_f = count_table[["FP"]], TN_f = count_table[["TN"]], TP_f = count_table[["TP"]])
    }
    else {
      temp_data <- data.frame(FN_f = count_table[["FN"]], FP_f = 0, TN_f = count_table[["TN"]], TP_f = count_table[["TP"]])
      }
    count_data_f <- rbind(count_data_f, temp_data)
  }
  count_data_f <- data.frame(count_data_f, gender = c("F"))
  # restructure
  all_count_data_wide <- cbind(count_data_f, count_data_m)
  all_count_data_wide <- cbind(all_count_data_wide[,1:4], all_count_data_wide[,6:9])
  # create list of data frames
  count_list <- list(all_count_data_wide, count_data_m, count_data_f)
  
  return(count_list)
  }


# Function that calculates the percentages of groups (TP, FN, TN, FP) for every iteration
# Takes the evaluation data frame from the run_simulation() function as the parameter
percentage_converter_all <- function(evaluation_df){
  eval <- evaluation_wide(evaluation_df)
  # males
  eval_m <- eval[[2]]
  eval_m <- eval_m[,1:4]
  m_percentage <- data.frame()
  for (i in 1:ncol(eval_m)){
    for (j in 1:nrow(eval_m)){
      cell <- eval_m[j,i]
      row <- sum(eval_m[j,])
      percent <- cell/(row/100)
      m_percentage[j,i] <- percent
    }
  }
  # females
  eval_f <- eval[[3]]
  eval_f <- eval_f[,1:4]
  f_percentage <- data.frame()
  for (i in 1:ncol(eval_f)){
    for (j in 1:nrow(eval_f)){
      cell <- eval_f[j,i]
      row <- sum(eval_f[j,])
      percent <- cell/(row/100)
      f_percentage[j,i] <- percent
    }
  }
  # make dataframe
  all_percentage <- data.frame(FN_f = f_percentage[,1], FP_f = f_percentage[,2], TN_f = f_percentage[,3], TP_f = f_percentage[,4],FN_m = m_percentage[,1], FP_m = m_percentage[,2], TN_m = m_percentage[,3], TP_m = m_percentage[,4])

  return(all_percentage)
}


# Functions that calculates how many percent of individuals with ASD have (TP) and have not (FN) gotten their diagnoses
# for every iteration. Takes the evaluation data frame from the run_simulation() function as the parameter
percentage_converter_ASD <- function(evaluation_df){
  eval <- evaluation_wide(evaluation_df)
  # males
  eval_m <- eval[[2]]
  eval_m <- eval_m[,c("FN_m", "TP_m")]
  m_percentage <- data.frame()
  for (i in 1:ncol(eval_m)){
    for (j in 1:nrow(eval_m)){
      cell <- eval_m[j,i]
      row <- sum(eval_m[j,])
      percent <- cell/(row/100)
      m_percentage[j,i] <- percent
    }
  }
  # females
  eval_f <- eval[[3]]
  eval_f <- eval_f[,c("FN_f", "TP_f")]
  f_percentage <- data.frame()
  for (i in 1:ncol(eval_f)){
    for (j in 1:nrow(eval_f)){
      cell <- eval_f[j,i]
      row <- sum(eval_f[j,])
      percent <- cell/(row/100)
      f_percentage[j,i] <- percent
    }
  }
  # make dataframe
  ASD_percentage <- data.frame(FN_f = f_percentage[, 1], TP_f = f_percentage[, 2], FN_m = m_percentage[, 1], TP_m = m_percentage[,2])
  
  return(ASD_percentage)
}


# Function determining which symptoms have been part of the diagnostic criteria for each iteration
# It creates a matrix used for plotting and takes a dataframe of the diagnostic_vectors from the run_simulation() function as the parameter
find_diagnostic_symptoms <- function(diagnostic_vectors){
  null_vec <- rep.int(0, ncol(diagnostic_vectors))
  all_diagnostic_vectors <- data.frame(S1 = null_vec, S2 = null_vec, S3 = null_vec, S4 = null_vec, S5 = null_vec, S6 = null_vec, S7 = null_vec, S8 = null_vec, S9 = null_vec)
  for (i in 1:ncol(diagnostic_vectors)){
    for (j in 1:nrow(diagnostic_vectors)){
      if (diagnostic_vectors[j,i] == "S1"){
        all_diagnostic_vectors[i,"S1"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S2"){
        all_diagnostic_vectors[i,"S2"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S3"){
        all_diagnostic_vectors[i,"S3"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S4"){
        all_diagnostic_vectors[i,"S4"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S5"){
        all_diagnostic_vectors[i,"S5"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S6"){
        all_diagnostic_vectors[i,"S6"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S7"){
        all_diagnostic_vectors[i,"S7"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S8"){
        all_diagnostic_vectors[i,"S8"] <- 1
      }
      if (diagnostic_vectors[j,i] == "S9"){
        all_diagnostic_vectors[i,"S9"] <- 1
      }
    }    
  }
  matrix_vectors <- data.matrix(all_diagnostic_vectors, rownames.force = NA)

  return(matrix_vectors)
}



############# ANALYSIS 1.a ################

### Generate data 1.a ###

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.7
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.7
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.7
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.3
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.3
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.3
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.7
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.3
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.7
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.3
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.7
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.3
Symptom9_Female_SD = 0.4

# Set correlations between symptoms (in this round of the simulation, correlations are homogenous and independent of gender)
SymptomCorr12 = 0.05
SymptomCorr13 = 0.05
SymptomCorr14 = 0.05
SymptomCorr15 = 0.05
SymptomCorr16 = 0.05
SymptomCorr17 = 0.05
SymptomCorr18 = 0.05
SymptomCorr19 = 0.05
SymptomCorr23 = 0.05
SymptomCorr24 = 0.05
SymptomCorr25 = 0.05
SymptomCorr26 = 0.05
SymptomCorr27 = 0.05
SymptomCorr28 = 0.05
SymptomCorr29 = 0.05
SymptomCorr34 = 0.05
SymptomCorr35 = 0.05
SymptomCorr36 = 0.05
SymptomCorr37 = 0.05
SymptomCorr38 = 0.05
SymptomCorr39 = 0.05
SymptomCorr45 = 0.05
SymptomCorr46 = 0.05
SymptomCorr47 = 0.05
SymptomCorr48 = 0.05
SymptomCorr49 = 0.05
SymptomCorr56 = 0.05
SymptomCorr57 = 0.05
SymptomCorr58 = 0.05
SymptomCorr59 = 0.05
SymptomCorr67 = 0.05
SymptomCorr68 = 0.05
SymptomCorr69 = 0.05
SymptomCorr78 = 0.05
SymptomCorr79 = 0.05
SymptomCorr89 = 0.05

# Geneate male agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaMale <- matrix(data=c(
  Symptom1_Male_SD^2, SymptomCorr12, SymptomCorr13, SymptomCorr14, SymptomCorr15, SymptomCorr16, SymptomCorr17, SymptomCorr18, SymptomCorr19,
  SymptomCorr12, Symptom2_Male_SD^2, SymptomCorr23,SymptomCorr24, SymptomCorr25, SymptomCorr26, SymptomCorr27, SymptomCorr28, SymptomCorr29,
  SymptomCorr13,SymptomCorr23, Symptom3_Male_SD^2, SymptomCorr34, SymptomCorr35, SymptomCorr36, SymptomCorr37, SymptomCorr38, SymptomCorr39,
  SymptomCorr14, SymptomCorr24, SymptomCorr34, Symptom4_Male_SD^2, SymptomCorr45, SymptomCorr46, SymptomCorr47, SymptomCorr48, SymptomCorr49,
  SymptomCorr15, SymptomCorr25, SymptomCorr35, SymptomCorr45, Symptom5_Male_SD^2, SymptomCorr56, SymptomCorr57, SymptomCorr58, SymptomCorr59,
  SymptomCorr16, SymptomCorr26, SymptomCorr36, SymptomCorr46, SymptomCorr56, Symptom6_Male_SD^2, SymptomCorr67, SymptomCorr68,SymptomCorr69,
  SymptomCorr17, SymptomCorr27, SymptomCorr37, SymptomCorr47, SymptomCorr57, SymptomCorr67, Symptom7_Male_SD^2, SymptomCorr78, SymptomCorr79,
  SymptomCorr18, SymptomCorr28, SymptomCorr38, SymptomCorr48, SymptomCorr58, SymptomCorr68, SymptomCorr78, Symptom8_Male_SD^2, SymptomCorr89,
  SymptomCorr19, SymptomCorr29, SymptomCorr39, SymptomCorr49, SymptomCorr59, SymptomCorr69, SymptomCorr79, SymptomCorr89, Symptom9_Male_SD^2),     
  nrow=9,ncol=9)

# Test for positive definitive
det(SigmaMale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Male <- mvrnorm(n = 200, # number of male participant
                  mu = c(Symptom1_Male_M, Symptom2_Male_M, Symptom3_Male_M, Symptom4_Male_M, Symptom5_Male_M, Symptom6_Male_M, Symptom7_Male_M, Symptom8_Male_M, Symptom9_Male_M), # mean of each variable
                  SigmaMale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Male <- data.frame(
  agentID = seq(nrow(D_Male)),
  gender = "M",
  S1 = D_Male [,1],
  S2 =  D_Male [,2],
  S3 =  D_Male [,3],
  S4 = D_Male [,4],
  S5 = D_Male [,5],
  S6 =  D_Male [,6],
  S7 =  D_Male [,7],
  S8 = D_Male [,8],
  S9 = D_Male [,9]
)

# Generate female agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaFemale <- matrix(data=c(
  Symptom1_Female_SD^2, SymptomCorr12, SymptomCorr13, SymptomCorr14, SymptomCorr15, SymptomCorr16, SymptomCorr17, SymptomCorr18, SymptomCorr19,
  SymptomCorr12, Symptom2_Female_SD^2, SymptomCorr23,SymptomCorr24, SymptomCorr25, SymptomCorr26, SymptomCorr27, SymptomCorr28, SymptomCorr29,
  SymptomCorr13,SymptomCorr23, Symptom3_Female_SD^2, SymptomCorr34, SymptomCorr35, SymptomCorr36, SymptomCorr37, SymptomCorr38, SymptomCorr39,
  SymptomCorr14, SymptomCorr24, SymptomCorr34, Symptom4_Female_SD^2, SymptomCorr45, SymptomCorr46, SymptomCorr47, SymptomCorr48, SymptomCorr49,
  SymptomCorr15, SymptomCorr25, SymptomCorr35, SymptomCorr45, Symptom5_Female_SD^2, SymptomCorr56, SymptomCorr57, SymptomCorr58, SymptomCorr59,
  SymptomCorr16, SymptomCorr26, SymptomCorr36, SymptomCorr46, SymptomCorr56, Symptom6_Female_SD^2, SymptomCorr67, SymptomCorr68,SymptomCorr69,
  SymptomCorr17, SymptomCorr27, SymptomCorr37, SymptomCorr47, SymptomCorr57, SymptomCorr67, Symptom7_Female_SD^2, SymptomCorr78, SymptomCorr79,
  SymptomCorr18, SymptomCorr28, SymptomCorr38, SymptomCorr48, SymptomCorr58, SymptomCorr68, SymptomCorr78, Symptom8_Female_SD^2, SymptomCorr89,
  SymptomCorr19, SymptomCorr29, SymptomCorr39, SymptomCorr49, SymptomCorr59, SymptomCorr69, SymptomCorr79, SymptomCorr89, Symptom9_Female_SD^2),     
  nrow=9,ncol=9)

# check for positive defnitive
det(SigmaFemale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Female <- mvrnorm(n = 200, # number of female participant
                    mu = c(Symptom1_Female_M, Symptom2_Female_M, Symptom3_Female_M, Symptom4_Female_M, Symptom5_Female_M, Symptom6_Female_M, Symptom7_Female_M, Symptom8_Female_M, Symptom9_Female_M), # mean of each variable
                    SigmaFemale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Female <- data.frame(
  agentID = nrow(D_Male) + seq(nrow(D_Female)),
  gender = "F",
  S1 = D_Female [,1],
  S2 =  D_Female [,2],
  S3 =  D_Female [,3],
  S4 = D_Female [,4],
  S5 = D_Female [,5],
  S6 =  D_Female [,6],
  S7 =  D_Female [,7],
  S8 = D_Female [,8],
  S9 = D_Female [,9]
)

# Merge the male and female dataframes into final dataframe with all agents
D_1a <- rbind(D_Male, D_Female)

# check symptom correlation visually
heatmap(cor(D_1a[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))



### Run simulation 1.a ###

# assign ASD to affected agents
D_1a <- assign_ASD(D_1a)

# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
#males
D_1a %>% filter(gender == "M") %>% summary()
# ASD males
D_1a %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_1a %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_1a %>% filter(gender == "F") %>% summary()
# ASD females
D_1a %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_1a %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()


# run simulation with 50 % males in diagnostic updating sample
results_1a <- run_simulation(full_dataset = D_1a, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.5)

# assign outcome dataframes to variables 
sim_data_1a <- results_1a[[1]]
vec_data_1a <- results_1a[[2]]
eval_data_1a <- results_1a[[3]]



### Restructure outcome of simulation 1.a ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_1a <- evaluation_long(eval_data_1a)
long_1a <- eval_long_1a[[1]]
eval_wide_1a <- evaluation_wide(eval_data_1a)
wide_1a <- eval_wide_1a[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_1a <- percentage_converter_all(eval_data_1a)

# run function to calculate ratio of FN's to TP's
perc_ASD_1a <- percentage_converter_ASD(eval_data_1a)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_1a <- find_diagnostic_symptoms(vec_data_1a)



### Visualizations via plots and graphs 1.a ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, simulation 1.a")

# single out the progression of FN's (total count values)
ggplot(wide_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, simulation 1.a")

# what is the trend in FN's among women
  ggplot(wide_1a, aes(x = 1:nrow(wide_1a), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, simulation 1.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
  ggplot(wide_1a, aes(x = 1:nrow(wide_1a), y = FN_m)) + 
    geom_point(color = "steelblue") +
    ylab ('n individuals') + xlab('iterations') +
    ggtitle("Total False Negatives in Males, simulation 1.a") +
    geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, simulation 1.a")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, simulation 1.a")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1a, aes(x = 1:nrow(wide_1a), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of Female FN's, 1.a, third run") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1a, aes(x = 1:nrow(wide_1a), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, simulation 1.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1a, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, 1.a, third run") 



############# ANALYSIS 1.b ################

### Generate data 1.b ###

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.7
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.7
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.7
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.3
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.3
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.3
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.7
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.3
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.7
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.3
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.7
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.3
Symptom9_Female_SD = 0.4

# Set correlations between symptoms (in this round of the simulation, correlations are homogenous and independent of gender)
SymptomCorr12 = 0.05
SymptomCorr13 = 0.05
SymptomCorr14 = 0.05
SymptomCorr15 = 0.05
SymptomCorr16 = 0.05
SymptomCorr17 = 0.05
SymptomCorr18 = 0.05
SymptomCorr19 = 0.05
SymptomCorr23 = 0.05
SymptomCorr24 = 0.05
SymptomCorr25 = 0.05
SymptomCorr26 = 0.05
SymptomCorr27 = 0.05
SymptomCorr28 = 0.05
SymptomCorr29 = 0.05
SymptomCorr34 = 0.05
SymptomCorr35 = 0.05
SymptomCorr36 = 0.05
SymptomCorr37 = 0.05
SymptomCorr38 = 0.05
SymptomCorr39 = 0.05
SymptomCorr45 = 0.05
SymptomCorr46 = 0.05
SymptomCorr47 = 0.05
SymptomCorr48 = 0.05
SymptomCorr49 = 0.05
SymptomCorr56 = 0.05
SymptomCorr57 = 0.05
SymptomCorr58 = 0.05
SymptomCorr59 = 0.05
SymptomCorr67 = 0.05
SymptomCorr68 = 0.05
SymptomCorr69 = 0.05
SymptomCorr78 = 0.05
SymptomCorr79 = 0.05
SymptomCorr89 = 0.05

# Geneate male agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaMale <- matrix(data=c(
  Symptom1_Male_SD^2, SymptomCorr12, SymptomCorr13, SymptomCorr14, SymptomCorr15, SymptomCorr16, SymptomCorr17, SymptomCorr18, SymptomCorr19,
  SymptomCorr12, Symptom2_Male_SD^2, SymptomCorr23,SymptomCorr24, SymptomCorr25, SymptomCorr26, SymptomCorr27, SymptomCorr28, SymptomCorr29,
  SymptomCorr13,SymptomCorr23, Symptom3_Male_SD^2, SymptomCorr34, SymptomCorr35, SymptomCorr36, SymptomCorr37, SymptomCorr38, SymptomCorr39,
  SymptomCorr14, SymptomCorr24, SymptomCorr34, Symptom4_Male_SD^2, SymptomCorr45, SymptomCorr46, SymptomCorr47, SymptomCorr48, SymptomCorr49,
  SymptomCorr15, SymptomCorr25, SymptomCorr35, SymptomCorr45, Symptom5_Male_SD^2, SymptomCorr56, SymptomCorr57, SymptomCorr58, SymptomCorr59,
  SymptomCorr16, SymptomCorr26, SymptomCorr36, SymptomCorr46, SymptomCorr56, Symptom6_Male_SD^2, SymptomCorr67, SymptomCorr68,SymptomCorr69,
  SymptomCorr17, SymptomCorr27, SymptomCorr37, SymptomCorr47, SymptomCorr57, SymptomCorr67, Symptom7_Male_SD^2, SymptomCorr78, SymptomCorr79,
  SymptomCorr18, SymptomCorr28, SymptomCorr38, SymptomCorr48, SymptomCorr58, SymptomCorr68, SymptomCorr78, Symptom8_Male_SD^2, SymptomCorr89,
  SymptomCorr19, SymptomCorr29, SymptomCorr39, SymptomCorr49, SymptomCorr59, SymptomCorr69, SymptomCorr79, SymptomCorr89, Symptom9_Male_SD^2),     
  nrow=9,ncol=9)

# Test for positive definitive
det(SigmaMale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Male <- mvrnorm(n = 200, # number of male participant
                  mu = c(Symptom1_Male_M, Symptom2_Male_M, Symptom3_Male_M, Symptom4_Male_M, Symptom5_Male_M, Symptom6_Male_M, Symptom7_Male_M, Symptom8_Male_M, Symptom9_Male_M), # mean of each variable
                  SigmaMale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Male <- data.frame(
  agentID = seq(nrow(D_Male)),
  gender = "M",
  S1 = D_Male [,1],
  S2 =  D_Male [,2],
  S3 =  D_Male [,3],
  S4 = D_Male [,4],
  S5 = D_Male [,5],
  S6 =  D_Male [,6],
  S7 =  D_Male [,7],
  S8 = D_Male [,8],
  S9 = D_Male [,9]
)

# Generate female agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaFemale <- matrix(data=c(
  Symptom1_Female_SD^2, SymptomCorr12, SymptomCorr13, SymptomCorr14, SymptomCorr15, SymptomCorr16, SymptomCorr17, SymptomCorr18, SymptomCorr19,
  SymptomCorr12, Symptom2_Female_SD^2, SymptomCorr23,SymptomCorr24, SymptomCorr25, SymptomCorr26, SymptomCorr27, SymptomCorr28, SymptomCorr29,
  SymptomCorr13,SymptomCorr23, Symptom3_Female_SD^2, SymptomCorr34, SymptomCorr35, SymptomCorr36, SymptomCorr37, SymptomCorr38, SymptomCorr39,
  SymptomCorr14, SymptomCorr24, SymptomCorr34, Symptom4_Female_SD^2, SymptomCorr45, SymptomCorr46, SymptomCorr47, SymptomCorr48, SymptomCorr49,
  SymptomCorr15, SymptomCorr25, SymptomCorr35, SymptomCorr45, Symptom5_Female_SD^2, SymptomCorr56, SymptomCorr57, SymptomCorr58, SymptomCorr59,
  SymptomCorr16, SymptomCorr26, SymptomCorr36, SymptomCorr46, SymptomCorr56, Symptom6_Female_SD^2, SymptomCorr67, SymptomCorr68,SymptomCorr69,
  SymptomCorr17, SymptomCorr27, SymptomCorr37, SymptomCorr47, SymptomCorr57, SymptomCorr67, Symptom7_Female_SD^2, SymptomCorr78, SymptomCorr79,
  SymptomCorr18, SymptomCorr28, SymptomCorr38, SymptomCorr48, SymptomCorr58, SymptomCorr68, SymptomCorr78, Symptom8_Female_SD^2, SymptomCorr89,
  SymptomCorr19, SymptomCorr29, SymptomCorr39, SymptomCorr49, SymptomCorr59, SymptomCorr69, SymptomCorr79, SymptomCorr89, Symptom9_Female_SD^2),     
  nrow=9,ncol=9)

# check for positive defnitive
det(SigmaFemale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Female <- mvrnorm(n = 200, # number of female participant
                    mu = c(Symptom1_Female_M, Symptom2_Female_M, Symptom3_Female_M, Symptom4_Female_M, Symptom5_Female_M, Symptom6_Female_M, Symptom7_Female_M, Symptom8_Female_M, Symptom9_Female_M), # mean of each variable
                    SigmaFemale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Female <- data.frame(
  agentID = nrow(D_Male) + seq(nrow(D_Female)),
  gender = "F",
  S1 = D_Female [,1],
  S2 =  D_Female [,2],
  S3 =  D_Female [,3],
  S4 = D_Female [,4],
  S5 = D_Female [,5],
  S6 =  D_Female [,6],
  S7 =  D_Female [,7],
  S8 = D_Female [,8],
  S9 = D_Female [,9]
)

# Merge the male and female dataframes into final dataframe with all agents
D_1b <- rbind(D_Male, D_Female)


# check symptom correlation visually
heatmap(cor(D_1b[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))



### Run simulation 1.b ###

# assign ASD to affected agents
D_1b <- assign_ASD(D_1b)


# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
#males
D_1b %>% filter(gender == "M") %>% summary()
# ASD males
D_1b %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_1b %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_1b %>% filter(gender == "F") %>% summary()
# ASD females
D_1b %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_1b %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()


# run simulation with 80 % males in diagnostic updating sample
results_1b <- run_simulation(full_dataset = D_1b, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.8)

# assign outcome dataframes to variables 
sim_data_1b <- results_1b[[1]]
vec_data_1b <- results_1b[[2]]
eval_data_1b <- results_1b[[3]]



### Restructure outcome of simulation 1.b ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_1b <- evaluation_long(eval_data_1b)
long_1b <- eval_long_1b[[1]]
eval_wide_1b <- evaluation_wide(eval_data_1b)
wide_1b <- eval_wide_1b[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_1b <- percentage_converter_all(eval_data_1b)

# run function to calculate ratio of FN's to TP's
perc_ASD_1b <- percentage_converter_ASD(eval_data_1b)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_1b <- find_diagnostic_symptoms(vec_data_1b)



### Visualizations via plots and graphs 1.b ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, simulation 1.b")

# single out the progression of FN's (total count values)
ggplot(wide_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, simulation 1.b")

# what is the trend in FN's among women
ggplot(wide_1b, aes(x = 1:nrow(wide_1b), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, simulation 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1b, aes(x = 1:nrow(wide_1b), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, simulation 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, simulation 1.b")

# single out the progression of FN's (percentage, only ASD)
ggplot(perc_ASD_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, simulation 1.b")

# what is the trend in FN's among women (percentage, only ASD)
ggplot(perc_ASD_1b, aes(x = 1:nrow(wide_1b), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, simulation 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, only ASD)
ggplot(perc_ASD_1b, aes(x = 1:nrow(wide_1b), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, simulation 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1b, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, simulation 1.b") 



############# ANALYSIS 1.c ################

### Generate data 1.c ###

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.7
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.7
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.7
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.3
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.3
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.3
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.7
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.3
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.7
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.3
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.7
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.3
Symptom9_Female_SD = 0.4

# Set correlations between symptoms (in this round of the simulation, correlations are homogenous and independent of gender)
SymptomCorr12 = 0.05
SymptomCorr13 = 0.05
SymptomCorr14 = 0.05
SymptomCorr15 = 0.05
SymptomCorr16 = 0.05
SymptomCorr17 = 0.05
SymptomCorr18 = 0.05
SymptomCorr19 = 0.05
SymptomCorr23 = 0.05
SymptomCorr24 = 0.05
SymptomCorr25 = 0.05
SymptomCorr26 = 0.05
SymptomCorr27 = 0.05
SymptomCorr28 = 0.05
SymptomCorr29 = 0.05
SymptomCorr34 = 0.05
SymptomCorr35 = 0.05
SymptomCorr36 = 0.05
SymptomCorr37 = 0.05
SymptomCorr38 = 0.05
SymptomCorr39 = 0.05
SymptomCorr45 = 0.05
SymptomCorr46 = 0.05
SymptomCorr47 = 0.05
SymptomCorr48 = 0.05
SymptomCorr49 = 0.05
SymptomCorr56 = 0.05
SymptomCorr57 = 0.05
SymptomCorr58 = 0.05
SymptomCorr59 = 0.05
SymptomCorr67 = 0.05
SymptomCorr68 = 0.05
SymptomCorr69 = 0.05
SymptomCorr78 = 0.05
SymptomCorr79 = 0.05
SymptomCorr89 = 0.05

# Geneate male agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaMale <- matrix(data=c(
  Symptom1_Male_SD^2, SymptomCorr12, SymptomCorr13, SymptomCorr14, SymptomCorr15, SymptomCorr16, SymptomCorr17, SymptomCorr18, SymptomCorr19,
  SymptomCorr12, Symptom2_Male_SD^2, SymptomCorr23,SymptomCorr24, SymptomCorr25, SymptomCorr26, SymptomCorr27, SymptomCorr28, SymptomCorr29,
  SymptomCorr13,SymptomCorr23, Symptom3_Male_SD^2, SymptomCorr34, SymptomCorr35, SymptomCorr36, SymptomCorr37, SymptomCorr38, SymptomCorr39,
  SymptomCorr14, SymptomCorr24, SymptomCorr34, Symptom4_Male_SD^2, SymptomCorr45, SymptomCorr46, SymptomCorr47, SymptomCorr48, SymptomCorr49,
  SymptomCorr15, SymptomCorr25, SymptomCorr35, SymptomCorr45, Symptom5_Male_SD^2, SymptomCorr56, SymptomCorr57, SymptomCorr58, SymptomCorr59,
  SymptomCorr16, SymptomCorr26, SymptomCorr36, SymptomCorr46, SymptomCorr56, Symptom6_Male_SD^2, SymptomCorr67, SymptomCorr68,SymptomCorr69,
  SymptomCorr17, SymptomCorr27, SymptomCorr37, SymptomCorr47, SymptomCorr57, SymptomCorr67, Symptom7_Male_SD^2, SymptomCorr78, SymptomCorr79,
  SymptomCorr18, SymptomCorr28, SymptomCorr38, SymptomCorr48, SymptomCorr58, SymptomCorr68, SymptomCorr78, Symptom8_Male_SD^2, SymptomCorr89,
  SymptomCorr19, SymptomCorr29, SymptomCorr39, SymptomCorr49, SymptomCorr59, SymptomCorr69, SymptomCorr79, SymptomCorr89, Symptom9_Male_SD^2),     
  nrow=9,ncol=9)

# Test for positive definitive
det(SigmaMale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Male <- mvrnorm(n = 200, # number of male participant
                  mu = c(Symptom1_Male_M, Symptom2_Male_M, Symptom3_Male_M, Symptom4_Male_M, Symptom5_Male_M, Symptom6_Male_M, Symptom7_Male_M, Symptom8_Male_M, Symptom9_Male_M), # mean of each variable
                  SigmaMale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Male <- data.frame(
  agentID = seq(nrow(D_Male)),
  gender = "M",
  S1 = D_Male [,1],
  S2 =  D_Male [,2],
  S3 =  D_Male [,3],
  S4 = D_Male [,4],
  S5 = D_Male [,5],
  S6 =  D_Male [,6],
  S7 =  D_Male [,7],
  S8 = D_Male [,8],
  S9 = D_Male [,9]
)

# Generate female agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaFemale <- matrix(data=c(
  Symptom1_Female_SD^2, SymptomCorr12, SymptomCorr13, SymptomCorr14, SymptomCorr15, SymptomCorr16, SymptomCorr17, SymptomCorr18, SymptomCorr19,
  SymptomCorr12, Symptom2_Female_SD^2, SymptomCorr23,SymptomCorr24, SymptomCorr25, SymptomCorr26, SymptomCorr27, SymptomCorr28, SymptomCorr29,
  SymptomCorr13,SymptomCorr23, Symptom3_Female_SD^2, SymptomCorr34, SymptomCorr35, SymptomCorr36, SymptomCorr37, SymptomCorr38, SymptomCorr39,
  SymptomCorr14, SymptomCorr24, SymptomCorr34, Symptom4_Female_SD^2, SymptomCorr45, SymptomCorr46, SymptomCorr47, SymptomCorr48, SymptomCorr49,
  SymptomCorr15, SymptomCorr25, SymptomCorr35, SymptomCorr45, Symptom5_Female_SD^2, SymptomCorr56, SymptomCorr57, SymptomCorr58, SymptomCorr59,
  SymptomCorr16, SymptomCorr26, SymptomCorr36, SymptomCorr46, SymptomCorr56, Symptom6_Female_SD^2, SymptomCorr67, SymptomCorr68,SymptomCorr69,
  SymptomCorr17, SymptomCorr27, SymptomCorr37, SymptomCorr47, SymptomCorr57, SymptomCorr67, Symptom7_Female_SD^2, SymptomCorr78, SymptomCorr79,
  SymptomCorr18, SymptomCorr28, SymptomCorr38, SymptomCorr48, SymptomCorr58, SymptomCorr68, SymptomCorr78, Symptom8_Female_SD^2, SymptomCorr89,
  SymptomCorr19, SymptomCorr29, SymptomCorr39, SymptomCorr49, SymptomCorr59, SymptomCorr69, SymptomCorr79, SymptomCorr89, Symptom9_Female_SD^2),     
  nrow=9,ncol=9)

# check for positive defnitive
det(SigmaFemale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Female <- mvrnorm(n = 200, # number of female participant
                    mu = c(Symptom1_Female_M, Symptom2_Female_M, Symptom3_Female_M, Symptom4_Female_M, Symptom5_Female_M, Symptom6_Female_M, Symptom7_Female_M, Symptom8_Female_M, Symptom9_Female_M), # mean of each variable
                    SigmaFemale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Female <- data.frame(
  agentID = nrow(D_Male) + seq(nrow(D_Female)),
  gender = "F",
  S1 = D_Female [,1],
  S2 =  D_Female [,2],
  S3 =  D_Female [,3],
  S4 = D_Female [,4],
  S5 = D_Female [,5],
  S6 =  D_Female [,6],
  S7 =  D_Female [,7],
  S8 = D_Female [,8],
  S9 = D_Female [,9]
)

# Merge the male and female dataframes into final dataframe with all agents
D_1c <- rbind(D_Male, D_Female)

# check symptom correlation visually
heatmap(cor(D_1c[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))



### Run simulation 1.c ###

# assign ASD to affected agents
D_1c <- assign_ASD(D_1c)

# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
#males
D_1c %>% filter(gender == "M") %>% summary()
# ASD males
D_1c %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_1c %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_1c %>% filter(gender == "F") %>% summary()
# ASD females
D_1c %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_1c %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()


# run simulation with 100 % males in diagnostic updating sample
results_1c <- run_simulation(full_dataset = D_1c, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 1)


# assign outcome dataframes to variables 
sim_data_1c <- results_1c[[1]]
vec_data_1c <- results_1c[[2]]
eval_data_1c <- results_1c[[3]]



### Restructure outcome of simulation 1.c ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_1c <- evaluation_long(eval_data_1c)
long_1c <- eval_long_1c[[1]]
eval_wide_1c <- evaluation_wide(eval_data_1c)
wide_1c <- eval_wide_1c[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_1c <- percentage_converter_all(eval_data_1c)

# run function to calculate ratio of FN's to TP's
perc_ASD_1c <- percentage_converter_ASD(eval_data_1c)

# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_1c <- find_diagnostic_symptoms(vec_data_1c)



### Visualizations via plots and graphs 1.c ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = TP_f, color = "TP, females")) + 
  geom_line(aes(y = FN_f, color = "FN, females")) +
  geom_line(aes(y = FP_f, color = "FP, females")) +
  geom_line(aes(y = TN_f, color = "TN, females")) +
  geom_line(aes(y = TP_m, color = "TP, males")) + 
  geom_line(aes(y = FN_m, color = "FN, males")) +
  geom_line(aes(y = FP_m, color = "FP, males")) +
  geom_line(aes(y = TN_m, color = "TN, males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, simulation 1.c")

# single out the progression of FN's (total count values)
ggplot(wide_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, simulation 1.c")

# what is the trend in FN's among women
ggplot(wide_1c, aes(x = 1:nrow(wide_1c), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, simulation 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1c, aes(x = 1:nrow(wide_1c), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, simulation 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, simulation 1.c")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, simulation 1.c")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1c, aes(x = 1:nrow(wide_1c), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, simulation 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1c, aes(x = 1:nrow(wide_1c), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, simulation 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1c, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, simulation 1.c") 



############# ANALYSIS 2.a ################

### Generate data 2.a ###

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.7
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.7
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.7
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.3
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.3
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.3
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.7
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.3
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.7
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.3
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.7
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.3
Symptom9_Female_SD = 0.4

# Set correlations between symptoms
#(in this round of the simulation, correlations are different between the sexes
# there are stronger between symptoms which occur together at high frequencies
# in the different sexes, thus simulating more discrete phenotypes)
#males
SymptomCorr12_m = 0.07
SymptomCorr13_m = 0.07
SymptomCorr14_m = 0.05
SymptomCorr15_m = 0.05
SymptomCorr16_m = 0.05
SymptomCorr17_m = 0.03
SymptomCorr18_m = 0.03
SymptomCorr19_m = 0.03
SymptomCorr23_m = 0.07
SymptomCorr24_m = 0.05
SymptomCorr25_m = 0.05
SymptomCorr26_m = 0.05
SymptomCorr27_m = 0.03
SymptomCorr28_m = 0.03
SymptomCorr29_m = 0.03
SymptomCorr34_m = 0.05
SymptomCorr35_m = 0.05
SymptomCorr36_m = 0.05
SymptomCorr37_m = 0.03
SymptomCorr38_m = 0.03
SymptomCorr39_m = 0.03
SymptomCorr45_m = 0.05
SymptomCorr46_m = 0.05
SymptomCorr47_m = 0.05
SymptomCorr48_m = 0.05
SymptomCorr49_m = 0.05
SymptomCorr56_m = 0.04
SymptomCorr57_m = 0.05
SymptomCorr58_m = 0.05
SymptomCorr59_m = 0.05
SymptomCorr67_m = 0.05
SymptomCorr68_m = 0.05
SymptomCorr69_m = 0.05
SymptomCorr78_m = 0.07
SymptomCorr79_m = 0.07
SymptomCorr89_m = 0.07
# females
SymptomCorr12_f = 0.05
SymptomCorr13_f = 0.03
SymptomCorr14_f = 0.07
SymptomCorr15_f = 0.05
SymptomCorr16_f = 0.03
SymptomCorr17_f = 0.07
SymptomCorr18_f = 0.05
SymptomCorr19_f = 0.03
SymptomCorr23_f = 0.05
SymptomCorr24_f = 0.05
SymptomCorr25_f = 0.05
SymptomCorr26_f = 0.05
SymptomCorr27_f = 0.05
SymptomCorr28_f = 0.05
SymptomCorr29_f = 0.05
SymptomCorr34_f = 0.03
SymptomCorr35_f = 0.05
SymptomCorr36_f = 0.07
SymptomCorr37_f = 0.03
SymptomCorr38_f = 0.05
SymptomCorr39_f = 0.07
SymptomCorr45_f = 0.05
SymptomCorr46_f = 0.03
SymptomCorr47_f = 0.07
SymptomCorr48_f = 0.05
SymptomCorr49_f = 0.03
SymptomCorr56_f = 0.05
SymptomCorr57_f = 0.05
SymptomCorr58_f = 0.05
SymptomCorr59_f = 0.05
SymptomCorr67_f = 0.03
SymptomCorr68_f = 0.05
SymptomCorr69_f = 0.07
SymptomCorr78_f = 0.05
SymptomCorr79_f = 0.03
SymptomCorr89_f = 0.05


# Geneate male agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaMale <- matrix(data=c(
  Symptom1_Male_SD^2, SymptomCorr12_m, SymptomCorr13_m, SymptomCorr14_m, SymptomCorr15_m, SymptomCorr16_m, SymptomCorr17_m, SymptomCorr18_m, SymptomCorr19_m,
  SymptomCorr12_m, Symptom2_Male_SD^2, SymptomCorr23_m, SymptomCorr24_m, SymptomCorr25_m, SymptomCorr26_m, SymptomCorr27_m, SymptomCorr28_m, SymptomCorr29_m,
  SymptomCorr13_m, SymptomCorr23_m, Symptom3_Male_SD^2, SymptomCorr34_m, SymptomCorr35_m, SymptomCorr36_m, SymptomCorr37_m, SymptomCorr38_m, SymptomCorr39_m,
  SymptomCorr14_m, SymptomCorr24_m, SymptomCorr34_m, Symptom4_Male_SD^2, SymptomCorr45_m, SymptomCorr46_m, SymptomCorr47_m, SymptomCorr48_m, SymptomCorr49_m,
  SymptomCorr15_m, SymptomCorr25_m, SymptomCorr35_m, SymptomCorr45_m, Symptom5_Male_SD^2, SymptomCorr56_m, SymptomCorr57_m, SymptomCorr58_m, SymptomCorr59_m,
  SymptomCorr16_m, SymptomCorr26_m, SymptomCorr36_m, SymptomCorr46_m, SymptomCorr56_m, Symptom6_Male_SD^2, SymptomCorr67_m, SymptomCorr68_m, SymptomCorr69_m,
  SymptomCorr17_m, SymptomCorr27_m, SymptomCorr37_m, SymptomCorr47_m, SymptomCorr57_m, SymptomCorr67_m, Symptom7_Male_SD^2, SymptomCorr78_m, SymptomCorr79_m,
  SymptomCorr18_m, SymptomCorr28_m, SymptomCorr38_m, SymptomCorr48_m, SymptomCorr58_m, SymptomCorr68_m, SymptomCorr78_m, Symptom8_Male_SD^2, SymptomCorr89_m,
  SymptomCorr19_m, SymptomCorr29_m, SymptomCorr39_m, SymptomCorr49_m, SymptomCorr59_m, SymptomCorr69_m, SymptomCorr79_m, SymptomCorr89_m, Symptom9_Male_SD^2),     
  nrow=9,ncol=9)

# Test for positive definitive
det(SigmaMale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Male <- mvrnorm(n = 200, # number of male participant
                  mu = c(Symptom1_Male_M, Symptom2_Male_M, Symptom3_Male_M, Symptom4_Male_M, Symptom5_Male_M, Symptom6_Male_M, Symptom7_Male_M, Symptom8_Male_M, Symptom9_Male_M), # mean of each variable
                  SigmaMale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Male <- data.frame(
  agentID = seq(nrow(D_Male)),
  gender = "M",
  S1 = D_Male [,1],
  S2 =  D_Male [,2],
  S3 =  D_Male [,3],
  S4 = D_Male [,4],
  S5 = D_Male [,5],
  S6 =  D_Male [,6],
  S7 =  D_Male [,7],
  S8 = D_Male [,8],
  S9 = D_Male [,9]
)


# Generate female agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaFemale <- matrix(data=c(
  Symptom1_Female_SD^2, SymptomCorr12_f, SymptomCorr13_f, SymptomCorr14_f, SymptomCorr15_f, SymptomCorr16_f, SymptomCorr17_f, SymptomCorr18_f, SymptomCorr19_f,
  SymptomCorr12_f, Symptom2_Female_SD^2, SymptomCorr23_f, SymptomCorr24_f, SymptomCorr25_f, SymptomCorr26_f, SymptomCorr27_f, SymptomCorr28_f, SymptomCorr29_f,
  SymptomCorr13_f, SymptomCorr23_f, Symptom3_Female_SD^2, SymptomCorr34_f, SymptomCorr35_f, SymptomCorr36_f, SymptomCorr37_f, SymptomCorr38_f, SymptomCorr39_f,
  SymptomCorr14_f, SymptomCorr24_f, SymptomCorr34_f, Symptom4_Female_SD^2, SymptomCorr45_f, SymptomCorr46_f, SymptomCorr47_f, SymptomCorr48_f, SymptomCorr49_f,
  SymptomCorr15_f, SymptomCorr25_f, SymptomCorr35_f, SymptomCorr45_f, Symptom5_Female_SD^2, SymptomCorr56_f, SymptomCorr57_f, SymptomCorr58_f, SymptomCorr59_f,
  SymptomCorr16_f, SymptomCorr26_f, SymptomCorr36_f, SymptomCorr46_f, SymptomCorr56_f, Symptom6_Female_SD^2, SymptomCorr67_f, SymptomCorr68_f, SymptomCorr69_f,
  SymptomCorr17_f, SymptomCorr27_f, SymptomCorr37_f, SymptomCorr47_f, SymptomCorr57_f, SymptomCorr67_f, Symptom7_Female_SD^2, SymptomCorr78_f, SymptomCorr79_f,
  SymptomCorr18_f, SymptomCorr28_f, SymptomCorr38_f, SymptomCorr48_f, SymptomCorr58_f, SymptomCorr68_f, SymptomCorr78_f, Symptom8_Female_SD^2, SymptomCorr89_f,
  SymptomCorr19_f, SymptomCorr29_f, SymptomCorr39_f, SymptomCorr49_f, SymptomCorr59_f, SymptomCorr69_f, SymptomCorr79_f, SymptomCorr89_f, Symptom9_Female_SD^2),     
  nrow=9,ncol=9)

# check for positive defnitive
det(SigmaFemale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Female <- mvrnorm(n = 200, # number of female participant
                    mu = c(Symptom1_Female_M, Symptom2_Female_M, Symptom3_Female_M, Symptom4_Female_M, Symptom5_Female_M, Symptom6_Female_M, Symptom7_Female_M, Symptom8_Female_M, Symptom9_Female_M), # mean of each variable
                    SigmaFemale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Female <- data.frame(
  agentID = nrow(D_Male) + seq(nrow(D_Female)),
  gender = "F",
  S1 = D_Female [,1],
  S2 =  D_Female [,2],
  S3 =  D_Female [,3],
  S4 = D_Female [,4],
  S5 = D_Female [,5],
  S6 =  D_Female [,6],
  S7 =  D_Female [,7],
  S8 = D_Female [,8],
  S9 = D_Female [,9]
)


# Merge the male and female dataframes into final dataframe with all agents
D_2a <- rbind(D_Male, D_Female)

# check symptom correlation visually
heatmap(cor(D_2a[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))
heatmap(cor(D_Male[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))
heatmap(cor(D_Female[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))


### Run simulation 2.a ###

# assign ASD to affected agents
D_2a <- assign_ASD(D_2a)

# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
#males
D_2a %>% filter(gender == "M") %>% summary()
# ASD males
D_2a %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_2a %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_2a %>% filter(gender == "F") %>% summary()
# ASD females
D_2a %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_2a %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()


# run simulation with 50 % males in diagnostic updating sample
results_2a <- run_simulation(full_dataset = D_2a, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.5)


# assign outcome dataframes to variables 
sim_data_2a <- results_2a[[1]]
vec_data_2a <- results_2a[[2]]
eval_data_2a <- results_2a[[3]]



### Restructure outcome of simulation 2.a ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_2a <- evaluation_long(eval_data_2a)
long_2a <- eval_long_2a[[1]]
eval_wide_2a <- evaluation_wide(eval_data_2a)
wide_2a <- eval_wide_2a[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_2a <- percentage_converter_all(eval_data_2a)

# run function to calculate ratio of FN's to TP's
perc_ASD_2a <- percentage_converter_ASD(eval_data_2a)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_2a <- find_diagnostic_symptoms(vec_data_2a)



### Visualizations via plots and graphs 2.a ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_2a, aes(x=1:nrow(wide_2a))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, simulation 2.a")

# single out the progression of FN's (total count values)
ggplot(wide_2a, aes(x=1:nrow(wide_2a))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, simulation 2.a")

# what is the trend in FN's among women
ggplot(wide_2a, aes(x = 1:nrow(wide_2a), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, simulation 2.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_2a, aes(x = 1:nrow(wide_2a), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, simulation 2.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_2a, aes(x=1:nrow(wide_2a))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, simulation 2.a")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_2a, aes(x=1:nrow(wide_2a))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, simulation 2.a")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_2a, aes(x = 1:nrow(wide_2a), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, simulation 2.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_2a, aes(x = 1:nrow(wide_2a), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, simulation 2.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_2a, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, simulation 2.a") 



############# ANALYSIS 2.b ################

### Generate data 2.b ###

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.7
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.7
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.7
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.3
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.3
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.3
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.7
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.3
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.7
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.3
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.7
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.3
Symptom9_Female_SD = 0.4

# Set correlations between symptoms
#(in this round of the simulation, correlations are different between the sexes
# there are stronger between symptoms which occur together at high frequencies
# in the different sexes, thus simulating more discrete phenotypes)
#males
SymptomCorr12_m = 0.07
SymptomCorr13_m = 0.07
SymptomCorr14_m = 0.05
SymptomCorr15_m = 0.05
SymptomCorr16_m = 0.05
SymptomCorr17_m = 0.03
SymptomCorr18_m = 0.03
SymptomCorr19_m = 0.03
SymptomCorr23_m = 0.07
SymptomCorr24_m = 0.05
SymptomCorr25_m = 0.05
SymptomCorr26_m = 0.05
SymptomCorr27_m = 0.03
SymptomCorr28_m = 0.03
SymptomCorr29_m = 0.03
SymptomCorr34_m = 0.05
SymptomCorr35_m = 0.05
SymptomCorr36_m = 0.05
SymptomCorr37_m = 0.03
SymptomCorr38_m = 0.03
SymptomCorr39_m = 0.03
SymptomCorr45_m = 0.05
SymptomCorr46_m = 0.05
SymptomCorr47_m = 0.05
SymptomCorr48_m = 0.05
SymptomCorr49_m = 0.05
SymptomCorr56_m = 0.04
SymptomCorr57_m = 0.05
SymptomCorr58_m = 0.05
SymptomCorr59_m = 0.05
SymptomCorr67_m = 0.05
SymptomCorr68_m = 0.05
SymptomCorr69_m = 0.05
SymptomCorr78_m = 0.07
SymptomCorr79_m = 0.07
SymptomCorr89_m = 0.07
# females
SymptomCorr12_f = 0.05
SymptomCorr13_f = 0.03
SymptomCorr14_f = 0.07
SymptomCorr15_f = 0.05
SymptomCorr16_f = 0.03
SymptomCorr17_f = 0.07
SymptomCorr18_f = 0.05
SymptomCorr19_f = 0.03
SymptomCorr23_f = 0.05
SymptomCorr24_f = 0.05
SymptomCorr25_f = 0.05
SymptomCorr26_f = 0.05
SymptomCorr27_f = 0.05
SymptomCorr28_f = 0.05
SymptomCorr29_f = 0.05
SymptomCorr34_f = 0.03
SymptomCorr35_f = 0.05
SymptomCorr36_f = 0.07
SymptomCorr37_f = 0.03
SymptomCorr38_f = 0.05
SymptomCorr39_f = 0.07
SymptomCorr45_f = 0.05
SymptomCorr46_f = 0.03
SymptomCorr47_f = 0.07
SymptomCorr48_f = 0.05
SymptomCorr49_f = 0.03
SymptomCorr56_f = 0.05
SymptomCorr57_f = 0.05
SymptomCorr58_f = 0.05
SymptomCorr59_f = 0.05
SymptomCorr67_f = 0.03
SymptomCorr68_f = 0.05
SymptomCorr69_f = 0.07
SymptomCorr78_f = 0.05
SymptomCorr79_f = 0.03
SymptomCorr89_f = 0.05


# Geneate male agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaMale <- matrix(data=c(
  Symptom1_Male_SD^2, SymptomCorr12_m, SymptomCorr13_m, SymptomCorr14_m, SymptomCorr15_m, SymptomCorr16_m, SymptomCorr17_m, SymptomCorr18_m, SymptomCorr19_m,
  SymptomCorr12_m, Symptom2_Male_SD^2, SymptomCorr23_m, SymptomCorr24_m, SymptomCorr25_m, SymptomCorr26_m, SymptomCorr27_m, SymptomCorr28_m, SymptomCorr29_m,
  SymptomCorr13_m, SymptomCorr23_m, Symptom3_Male_SD^2, SymptomCorr34_m, SymptomCorr35_m, SymptomCorr36_m, SymptomCorr37_m, SymptomCorr38_m, SymptomCorr39_m,
  SymptomCorr14_m, SymptomCorr24_m, SymptomCorr34_m, Symptom4_Male_SD^2, SymptomCorr45_m, SymptomCorr46_m, SymptomCorr47_m, SymptomCorr48_m, SymptomCorr49_m,
  SymptomCorr15_m, SymptomCorr25_m, SymptomCorr35_m, SymptomCorr45_m, Symptom5_Male_SD^2, SymptomCorr56_m, SymptomCorr57_m, SymptomCorr58_m, SymptomCorr59_m,
  SymptomCorr16_m, SymptomCorr26_m, SymptomCorr36_m, SymptomCorr46_m, SymptomCorr56_m, Symptom6_Male_SD^2, SymptomCorr67_m, SymptomCorr68_m, SymptomCorr69_m,
  SymptomCorr17_m, SymptomCorr27_m, SymptomCorr37_m, SymptomCorr47_m, SymptomCorr57_m, SymptomCorr67_m, Symptom7_Male_SD^2, SymptomCorr78_m, SymptomCorr79_m,
  SymptomCorr18_m, SymptomCorr28_m, SymptomCorr38_m, SymptomCorr48_m, SymptomCorr58_m, SymptomCorr68_m, SymptomCorr78_m, Symptom8_Male_SD^2, SymptomCorr89_m,
  SymptomCorr19_m, SymptomCorr29_m, SymptomCorr39_m, SymptomCorr49_m, SymptomCorr59_m, SymptomCorr69_m, SymptomCorr79_m, SymptomCorr89_m, Symptom9_Male_SD^2),     
  nrow=9,ncol=9)

# Test for positive definitive
det(SigmaMale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Male <- mvrnorm(n = 200, # number of male participant
                  mu = c(Symptom1_Male_M, Symptom2_Male_M, Symptom3_Male_M, Symptom4_Male_M, Symptom5_Male_M, Symptom6_Male_M, Symptom7_Male_M, Symptom8_Male_M, Symptom9_Male_M), # mean of each variable
                  SigmaMale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Male <- data.frame(
  agentID = seq(nrow(D_Male)),
  gender = "M",
  S1 = D_Male [,1],
  S2 =  D_Male [,2],
  S3 =  D_Male [,3],
  S4 = D_Male [,4],
  S5 = D_Male [,5],
  S6 =  D_Male [,6],
  S7 =  D_Male [,7],
  S8 = D_Male [,8],
  S9 = D_Male [,9]
)


# Generate female agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaFemale <- matrix(data=c(
  Symptom1_Female_SD^2, SymptomCorr12_f, SymptomCorr13_f, SymptomCorr14_f, SymptomCorr15_f, SymptomCorr16_f, SymptomCorr17_f, SymptomCorr18_f, SymptomCorr19_f,
  SymptomCorr12_f, Symptom2_Female_SD^2, SymptomCorr23_f, SymptomCorr24_f, SymptomCorr25_f, SymptomCorr26_f, SymptomCorr27_f, SymptomCorr28_f, SymptomCorr29_f,
  SymptomCorr13_f, SymptomCorr23_f, Symptom3_Female_SD^2, SymptomCorr34_f, SymptomCorr35_f, SymptomCorr36_f, SymptomCorr37_f, SymptomCorr38_f, SymptomCorr39_f,
  SymptomCorr14_f, SymptomCorr24_f, SymptomCorr34_f, Symptom4_Female_SD^2, SymptomCorr45_f, SymptomCorr46_f, SymptomCorr47_f, SymptomCorr48_f, SymptomCorr49_f,
  SymptomCorr15_f, SymptomCorr25_f, SymptomCorr35_f, SymptomCorr45_f, Symptom5_Female_SD^2, SymptomCorr56_f, SymptomCorr57_f, SymptomCorr58_f, SymptomCorr59_f,
  SymptomCorr16_f, SymptomCorr26_f, SymptomCorr36_f, SymptomCorr46_f, SymptomCorr56_f, Symptom6_Female_SD^2, SymptomCorr67_f, SymptomCorr68_f, SymptomCorr69_f,
  SymptomCorr17_f, SymptomCorr27_f, SymptomCorr37_f, SymptomCorr47_f, SymptomCorr57_f, SymptomCorr67_f, Symptom7_Female_SD^2, SymptomCorr78_f, SymptomCorr79_f,
  SymptomCorr18_f, SymptomCorr28_f, SymptomCorr38_f, SymptomCorr48_f, SymptomCorr58_f, SymptomCorr68_f, SymptomCorr78_f, Symptom8_Female_SD^2, SymptomCorr89_f,
  SymptomCorr19_f, SymptomCorr29_f, SymptomCorr39_f, SymptomCorr49_f, SymptomCorr59_f, SymptomCorr69_f, SymptomCorr79_f, SymptomCorr89_f, Symptom9_Female_SD^2),     
  nrow=9,ncol=9)

# check for positive defnitive
det(SigmaFemale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Female <- mvrnorm(n = 200, # number of female participant
                    mu = c(Symptom1_Female_M, Symptom2_Female_M, Symptom3_Female_M, Symptom4_Female_M, Symptom5_Female_M, Symptom6_Female_M, Symptom7_Female_M, Symptom8_Female_M, Symptom9_Female_M), # mean of each variable
                    SigmaFemale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Female <- data.frame(
  agentID = nrow(D_Male) + seq(nrow(D_Female)),
  gender = "F",
  S1 = D_Female [,1],
  S2 =  D_Female [,2],
  S3 =  D_Female [,3],
  S4 = D_Female [,4],
  S5 = D_Female [,5],
  S6 =  D_Female [,6],
  S7 =  D_Female [,7],
  S8 = D_Female [,8],
  S9 = D_Female [,9]
)


# Merge the male and female dataframes into final dataframe with all agents
D_2b <- rbind(D_Male, D_Female)

# check symptom correlation visually
heatmap(cor(D_2b[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))
heatmap(cor(D_Male[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))
heatmap(cor(D_Female[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))


### Run simulation 2.b ###

# assign ASD to affected agents
D_2b <- assign_ASD(D_2b)

# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
#males
D_2b %>% filter(gender == "M") %>% summary()
# ASD males
D_2b %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_2b %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_2b %>% filter(gender == "F") %>% summary()
# ASD females
D_2b %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_2b %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()


# run simulation with 80 % males in diagnostic updating sample
results_2b <- run_simulation(full_dataset = D_2b, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.8)


# assign outcome dataframes to variables 
sim_data_2b <- results_2b[[1]]
vec_data_2b <- results_2b[[2]]
eval_data_2b <- results_2b[[3]]



### Restructure outcome of simulation 2.b ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_2b <- evaluation_long(eval_data_2b)
long_2b <- eval_long_2b[[1]]
eval_wide_2b <- evaluation_wide(eval_data_2b)
wide_2b <- eval_wide_2b[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_2b <- percentage_converter_all(eval_data_2b)

# run function to calculate ratio of FN's to TP's
perc_ASD_2b <- percentage_converter_ASD(eval_data_2b)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_2b <- find_diagnostic_symptoms(vec_data_2b)



### Visualizations via plots and graphs 2.b ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_2b, aes(x=1:nrow(wide_2b))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, simulation 2.b")

# single out the progression of FN's (total count values)
ggplot(wide_2b, aes(x=1:nrow(wide_2b))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, simulation 2.b")

# what is the trend in FN's among women
ggplot(wide_2b, aes(x = 1:nrow(wide_2b), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, simulation 2.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_2b, aes(x = 1:nrow(wide_2b), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, simulation 2.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_2b, aes(x=1:nrow(wide_2b))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, simulation 2.b")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_2b, aes(x=1:nrow(wide_2b))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, simulation 2.b")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_2b, aes(x = 1:nrow(wide_2b), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, simulation 2.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_2b, aes(x = 1:nrow(wide_2b), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, simulation 2.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_2b, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, simulation 2.b") 



############# ANALYSIS 2.c ################

### Generate data 2.c ###

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.7
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.7
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.7
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.3
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.3
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.3
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.7
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.3
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.7
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.3
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.7
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.3
Symptom9_Female_SD = 0.4

# Set correlations between symptoms
#(in this round of the simulation, correlations are different between the sexes
# there are stronger between symptoms which occur together at high frequencies
# in the different sexes, thus simulating more discrete phenotypes)
#males
SymptomCorr12_m = 0.07
SymptomCorr13_m = 0.07
SymptomCorr14_m = 0.05
SymptomCorr15_m = 0.05
SymptomCorr16_m = 0.05
SymptomCorr17_m = 0.03
SymptomCorr18_m = 0.03
SymptomCorr19_m = 0.03
SymptomCorr23_m = 0.07
SymptomCorr24_m = 0.05
SymptomCorr25_m = 0.05
SymptomCorr26_m = 0.05
SymptomCorr27_m = 0.03
SymptomCorr28_m = 0.03
SymptomCorr29_m = 0.03
SymptomCorr34_m = 0.05
SymptomCorr35_m = 0.05
SymptomCorr36_m = 0.05
SymptomCorr37_m = 0.03
SymptomCorr38_m = 0.03
SymptomCorr39_m = 0.03
SymptomCorr45_m = 0.05
SymptomCorr46_m = 0.05
SymptomCorr47_m = 0.05
SymptomCorr48_m = 0.05
SymptomCorr49_m = 0.05
SymptomCorr56_m = 0.04
SymptomCorr57_m = 0.05
SymptomCorr58_m = 0.05
SymptomCorr59_m = 0.05
SymptomCorr67_m = 0.05
SymptomCorr68_m = 0.05
SymptomCorr69_m = 0.05
SymptomCorr78_m = 0.07
SymptomCorr79_m = 0.07
SymptomCorr89_m = 0.07
# females
SymptomCorr12_f = 0.05
SymptomCorr13_f = 0.03
SymptomCorr14_f = 0.07
SymptomCorr15_f = 0.05
SymptomCorr16_f = 0.03
SymptomCorr17_f = 0.07
SymptomCorr18_f = 0.05
SymptomCorr19_f = 0.03
SymptomCorr23_f = 0.05
SymptomCorr24_f = 0.05
SymptomCorr25_f = 0.05
SymptomCorr26_f = 0.05
SymptomCorr27_f = 0.05
SymptomCorr28_f = 0.05
SymptomCorr29_f = 0.05
SymptomCorr34_f = 0.03
SymptomCorr35_f = 0.05
SymptomCorr36_f = 0.07
SymptomCorr37_f = 0.03
SymptomCorr38_f = 0.05
SymptomCorr39_f = 0.07
SymptomCorr45_f = 0.05
SymptomCorr46_f = 0.03
SymptomCorr47_f = 0.07
SymptomCorr48_f = 0.05
SymptomCorr49_f = 0.03
SymptomCorr56_f = 0.05
SymptomCorr57_f = 0.05
SymptomCorr58_f = 0.05
SymptomCorr59_f = 0.05
SymptomCorr67_f = 0.03
SymptomCorr68_f = 0.05
SymptomCorr69_f = 0.07
SymptomCorr78_f = 0.05
SymptomCorr79_f = 0.03
SymptomCorr89_f = 0.05


# Geneate male agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaMale <- matrix(data=c(
  Symptom1_Male_SD^2, SymptomCorr12_m, SymptomCorr13_m, SymptomCorr14_m, SymptomCorr15_m, SymptomCorr16_m, SymptomCorr17_m, SymptomCorr18_m, SymptomCorr19_m,
  SymptomCorr12_m, Symptom2_Male_SD^2, SymptomCorr23_m, SymptomCorr24_m, SymptomCorr25_m, SymptomCorr26_m, SymptomCorr27_m, SymptomCorr28_m, SymptomCorr29_m,
  SymptomCorr13_m, SymptomCorr23_m, Symptom3_Male_SD^2, SymptomCorr34_m, SymptomCorr35_m, SymptomCorr36_m, SymptomCorr37_m, SymptomCorr38_m, SymptomCorr39_m,
  SymptomCorr14_m, SymptomCorr24_m, SymptomCorr34_m, Symptom4_Male_SD^2, SymptomCorr45_m, SymptomCorr46_m, SymptomCorr47_m, SymptomCorr48_m, SymptomCorr49_m,
  SymptomCorr15_m, SymptomCorr25_m, SymptomCorr35_m, SymptomCorr45_m, Symptom5_Male_SD^2, SymptomCorr56_m, SymptomCorr57_m, SymptomCorr58_m, SymptomCorr59_m,
  SymptomCorr16_m, SymptomCorr26_m, SymptomCorr36_m, SymptomCorr46_m, SymptomCorr56_m, Symptom6_Male_SD^2, SymptomCorr67_m, SymptomCorr68_m, SymptomCorr69_m,
  SymptomCorr17_m, SymptomCorr27_m, SymptomCorr37_m, SymptomCorr47_m, SymptomCorr57_m, SymptomCorr67_m, Symptom7_Male_SD^2, SymptomCorr78_m, SymptomCorr79_m,
  SymptomCorr18_m, SymptomCorr28_m, SymptomCorr38_m, SymptomCorr48_m, SymptomCorr58_m, SymptomCorr68_m, SymptomCorr78_m, Symptom8_Male_SD^2, SymptomCorr89_m,
  SymptomCorr19_m, SymptomCorr29_m, SymptomCorr39_m, SymptomCorr49_m, SymptomCorr59_m, SymptomCorr69_m, SymptomCorr79_m, SymptomCorr89_m, Symptom9_Male_SD^2),     
  nrow=9,ncol=9)

# Test for positive definitive
det(SigmaMale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Male <- mvrnorm(n = 200, # number of male participant
                  mu = c(Symptom1_Male_M, Symptom2_Male_M, Symptom3_Male_M, Symptom4_Male_M, Symptom5_Male_M, Symptom6_Male_M, Symptom7_Male_M, Symptom8_Male_M, Symptom9_Male_M), # mean of each variable
                  SigmaMale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Male <- data.frame(
  agentID = seq(nrow(D_Male)),
  gender = "M",
  S1 = D_Male [,1],
  S2 =  D_Male [,2],
  S3 =  D_Male [,3],
  S4 = D_Male [,4],
  S5 = D_Male [,5],
  S6 =  D_Male [,6],
  S7 =  D_Male [,7],
  S8 = D_Male [,8],
  S9 = D_Male [,9]
)


# Generate female agents
# Create a variance-covariance matrix for the symptom severities we want to generate (variance is sd^2)
SigmaFemale <- matrix(data=c(
  Symptom1_Female_SD^2, SymptomCorr12_f, SymptomCorr13_f, SymptomCorr14_f, SymptomCorr15_f, SymptomCorr16_f, SymptomCorr17_f, SymptomCorr18_f, SymptomCorr19_f,
  SymptomCorr12_f, Symptom2_Female_SD^2, SymptomCorr23_f, SymptomCorr24_f, SymptomCorr25_f, SymptomCorr26_f, SymptomCorr27_f, SymptomCorr28_f, SymptomCorr29_f,
  SymptomCorr13_f, SymptomCorr23_f, Symptom3_Female_SD^2, SymptomCorr34_f, SymptomCorr35_f, SymptomCorr36_f, SymptomCorr37_f, SymptomCorr38_f, SymptomCorr39_f,
  SymptomCorr14_f, SymptomCorr24_f, SymptomCorr34_f, Symptom4_Female_SD^2, SymptomCorr45_f, SymptomCorr46_f, SymptomCorr47_f, SymptomCorr48_f, SymptomCorr49_f,
  SymptomCorr15_f, SymptomCorr25_f, SymptomCorr35_f, SymptomCorr45_f, Symptom5_Female_SD^2, SymptomCorr56_f, SymptomCorr57_f, SymptomCorr58_f, SymptomCorr59_f,
  SymptomCorr16_f, SymptomCorr26_f, SymptomCorr36_f, SymptomCorr46_f, SymptomCorr56_f, Symptom6_Female_SD^2, SymptomCorr67_f, SymptomCorr68_f, SymptomCorr69_f,
  SymptomCorr17_f, SymptomCorr27_f, SymptomCorr37_f, SymptomCorr47_f, SymptomCorr57_f, SymptomCorr67_f, Symptom7_Female_SD^2, SymptomCorr78_f, SymptomCorr79_f,
  SymptomCorr18_f, SymptomCorr28_f, SymptomCorr38_f, SymptomCorr48_f, SymptomCorr58_f, SymptomCorr68_f, SymptomCorr78_f, Symptom8_Female_SD^2, SymptomCorr89_f,
  SymptomCorr19_f, SymptomCorr29_f, SymptomCorr39_f, SymptomCorr49_f, SymptomCorr59_f, SymptomCorr69_f, SymptomCorr79_f, SymptomCorr89_f, Symptom9_Female_SD^2),     
  nrow=9,ncol=9)

# check for positive defnitive
det(SigmaFemale)

# Generate individual symptom severities for200 agents from a multivariate normal distribution
D_Female <- mvrnorm(n = 200, # number of female participant
                    mu = c(Symptom1_Female_M, Symptom2_Female_M, Symptom3_Female_M, Symptom4_Female_M, Symptom5_Female_M, Symptom6_Female_M, Symptom7_Female_M, Symptom8_Female_M, Symptom9_Female_M), # mean of each variable
                    SigmaFemale) # variance co-variance matrix

# Assign new, meaningful names to variables and add ID to data frame
D_Female <- data.frame(
  agentID = nrow(D_Male) + seq(nrow(D_Female)),
  gender = "F",
  S1 = D_Female [,1],
  S2 =  D_Female [,2],
  S3 =  D_Female [,3],
  S4 = D_Female [,4],
  S5 = D_Female [,5],
  S6 =  D_Female [,6],
  S7 =  D_Female [,7],
  S8 = D_Female [,8],
  S9 = D_Female [,9]
)


# Merge the male and female dataframes into final dataframe with all agents
D_2c <- rbind(D_Male, D_Female)

# check symptom correlation visually
heatmap(cor(D_2c[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))
heatmap(cor(D_Male[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))
heatmap(cor(D_Female[, 3:11]), Rowv = NA, Colv = NA, col = viridis(20))


### Run simulation 2.c ###

# assign ASD to affected agents
D_2c <- assign_ASD(D_2c)

# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
#males
D_2c %>% filter(gender == "M") %>% summary()
# ASD males
D_2c %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_2c %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_2c %>% filter(gender == "F") %>% summary()
# ASD females
D_2c %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_2c %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()


# run simulation with 100 % males in diagnostic updating sample
results_2c <- run_simulation(full_dataset = D_2c, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 1)


# assign outcome dataframes to variables 
sim_data_2c <- results_2c[[1]]
vec_data_2c <- results_2c[[2]]
eval_data_2c <- results_2c[[3]]



### Restructure outcome of simulation 2.c ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_2c <- evaluation_long(eval_data_2c)
long_2c <- eval_long_2c[[1]]
eval_wide_2c <- evaluation_wide(eval_data_2c)
wide_2c <- eval_wide_2c[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_2c <- percentage_converter_all(eval_data_2c)

# run function to calculate ratio of FN's to TP's
perc_ASD_2c <- percentage_converter_ASD(eval_data_2c)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_2c <- find_diagnostic_symptoms(vec_data_2c)



### Visualizations via plots and graphs 2.c ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_2c, aes(x=1:nrow(wide_2c))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, simulation 2.c")

# single out the progression of FN's (total count values)
ggplot(wide_2c, aes(x=1:nrow(wide_2c))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, simulation 2.c")

# what is the trend in FN's among women
ggplot(wide_2c, aes(x = 1:nrow(wide_2c), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, simulation 2.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_2c, aes(x = 1:nrow(wide_2c), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, simulation 2.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_2c, aes(x=1:nrow(wide_2c))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, simulation 2.c")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_2c, aes(x=1:nrow(wide_2c))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, simulation 2.c")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_2c, aes(x = 1:nrow(wide_2c), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, simulation 2.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_2c, aes(x = 1:nrow(wide_2c), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, simulation 2.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_2c, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, simulation 2.c") 
