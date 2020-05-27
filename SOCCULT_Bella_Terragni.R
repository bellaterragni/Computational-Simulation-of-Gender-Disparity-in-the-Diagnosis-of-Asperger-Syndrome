# SOCIAL AND CULTURAL DYNAMICS IN COGNITION EXAM SPRING 2020 #
# Diagnostic disparity in women with Asperger's Syndrome #
# Bella Terragni #


############## SECTION 0 - set-up ############

# load packages
pacman::p_load(dplyr, tidyr, MASS, tidyverse, psych, viridis, rethinking, pracma)

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
# the cutoff value for a present symptom is hardcoded to five
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
# Takes the parameters: full dataset, number of iterations, size of diagnostic updating sample, number of diagnostic symptoms, number of symptoms
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



######## SECTION 3 - Generate data for all analysis 1.a, 1.b, 1.c, 1.d, 1.e, and 1.g ######

# set a mean and sd for the severity of 9 symptoms in males and females
# males
Symptom1_Male_M = 0.8
Symptom1_Male_SD = 0.4
Symptom2_Male_M = 0.8
Symptom2_Male_SD = 0.4
Symptom3_Male_M = 0.8
Symptom3_Male_SD = 0.4
Symptom4_Male_M = 0.5
Symptom4_Male_SD = 0.4
Symptom5_Male_M = 0.5
Symptom5_Male_SD = 0.4
Symptom6_Male_M = 0.5
Symptom6_Male_SD = 0.4
Symptom7_Male_M = 0.2
Symptom7_Male_SD = 0.4
Symptom8_Male_M = 0.2
Symptom8_Male_SD = 0.4
Symptom9_Male_M = 0.2
Symptom9_Male_SD = 0.4
# females
Symptom1_Female_M = 0.8
Symptom1_Female_SD = 0.4
Symptom2_Female_M = 0.5
Symptom2_Female_SD = 0.4
Symptom3_Female_M = 0.2
Symptom3_Female_SD = 0.4
Symptom4_Female_M = 0.8
Symptom4_Female_SD = 0.4
Symptom5_Female_M = 0.5
Symptom5_Female_SD = 0.4
Symptom6_Female_M = 0.2
Symptom6_Female_SD = 0.4
Symptom7_Female_M = 0.8
Symptom7_Female_SD = 0.4
Symptom8_Female_M = 0.5
Symptom8_Female_SD = 0.4
Symptom9_Female_M = 0.2
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

# Assign new names to variables and add ID to data frame
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

# Assign new names to variables and add ID to data frame
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
D_MF <- rbind(D_Male, D_Female)



######### assign ASD to affected agents #########
D_MF <- assign_ASD(D_MF, n_symptoms = 9, severity_level = 0.4, n_symptom_cutoff = 6)



####### check the generated data #######

# check symptom correlation visually
heatmap(cor(D_MF[ ,3:11]), Rowv = NA, Colv = NA, col = viridis(20))

# check the distribution of ASD positive diagnoses as well as symptom severities for both genders
D_MF %>% filter(ASD == 1) %>% summary() # my round of simulation M: 85, F: 86 -> 42.75 % ASD
#males
D_MF %>% filter(gender == "M") %>% summary()
# ASD males
D_MF %>% filter(gender == "M") %>% filter(ASD == 1) %>% summary()
# male controls
D_MF %>% filter(gender == "M") %>% filter(ASD == 0) %>% summary()

# femlaes
D_MF %>% filter(gender == "F") %>% summary()
# ASD females
D_MF %>% filter(gender == "F") %>% filter(ASD == 1) %>% summary()
# female controls
D_MF %>% filter(gender == "F") %>% filter(ASD == 0) %>% summary()

# check whether sum of severities are similar
D_MFm <- D_MF %>% filter(gender == "M") %>% filter(ASD == 1) 
D_MFf <- D_MF %>% filter(gender == "F") %>% filter(ASD == 1) 
D_allm <- D_MF %>% filter(gender == "M")
D_allf <- D_MF %>% filter(gender == "F") 
sum(D_MFm[,3:11]/nrow(D_MFm))
sum(D_MFf[,3:11]/nrow(D_MFf))


# visualize symptom severity distributions of AS individuals via density plots
# males
dens(D_MFm[,3:11])
# females
dens(D_MFf[,3:11])
# Visualize symptom severity distributions of entire population via density plots
# males
dens(D_allm[,3:11])
# females
dens(D_allf[,3:11])


# comapre distribution of symptom severity betweeen genders
plot(density(D_MFm$S1), main = "Symptom 1", col = "steelblue3", xlab='Severity', ylab='Density') +
lines(density(D_MFf$S1), col = "indianred2")

plot(density(D_MFf$S2), main = "Symptom 2", col = "indianred2", xlab='Severity', ylab='Density') +
lines(density(D_MFm$S2), col = "steelblue3")

plot(density(D_MFf$S3), main = "Symptom 3", col = "indianred2", xlab='Severity', ylab='Density') +
lines(density(D_MFm$S3), col = "steelblue3") 

plot(density(D_MFm$S4), main = "Symptom 4", col = "steelblue3", xlab='Severity', ylab='Density') +
lines(density(D_MFf$S4), col = "indianred2") 

plot(density(D_MFf$S5), main = "Symptom 5", col = "indianred2", xlab='Severity', ylab='Density') +
lines(density(D_MFm$S5), col = "steelblue3")

plot(density(D_MFm$S6), main = "Symptom 6", col = "steelblue3", xlab='Severity', ylab='Density') +
lines(density(D_MFf$S6), col = "indianred2") 

plot(density(D_MFm$S7), main = "Symptom 7", col = "steelblue3", xlab='Severity', ylab='Density') +
lines(density(D_MFf$S7), col = "indianred2") 

plot(density(D_MFf$S8), main = "Symptom 8", col = "indianred2", xlab='Severity', ylab='Density') +
lines(density(D_MFm$S8), col = "steelblue3") 

plot(density(D_MFm$S9), main = "Symptom 9", col = "steelblue3", xlab='Severity', ylab='Density') +
lines(density(D_MFf$S9), col = "indianred2")



################# SECTION 4 - Running the simulation for analysis 1.a, 1.b, 1.c, 1.d, 1.e, 1.f #################

######### Analysis 1.a #########

### assign data ###
D_1a <- D_MF


### Run simulation 1.a. with set parameters ###

# run simulation with 50 % males in diagnostic updating sample (gender ratio 1:1)
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
  ggtitle("Total FP, FN, TP, TN, 1.a")

# single out the progression of FN's (total count values)
ggplot(wide_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, 1.a")

# what is the trend in FN's among women
  ggplot(wide_1a, aes(x = 1:nrow(wide_1a), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, analysis 1.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
  ggplot(wide_1a, aes(x = 1:nrow(wide_1a), y = FN_m)) + 
    geom_point(color = "steelblue") +
    ylab ('n individuals') + xlab('iterations') +
    ggtitle("Total False Negatives in Males, analysis 1.a") +
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
  ggtitle("Percentage of FP, FN, TP, TN, 1.a")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, 1.a")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1a, aes(x = 1:nrow(wide_1a), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of Female FN's, 1.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1a, aes(x = 1:nrow(wide_1a), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, 1.a") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# display plot of FN's, TN's, TP's, and FP's in percentage with simple moving average (windows of 9)
f_FN_1a <- movavg(perc_all_1a$FN_f, 5, type=c("s"))
m_FN_1a <- movavg(perc_all_1a$FN_m, 5, type=c("s"))
f_TN_1a <- movavg(perc_all_1a$TN_f, 5, type=c("s"))
m_TN_1a <- movavg(perc_all_1a$TN_m, 5, type=c("s"))
f_FP_1a <- movavg(perc_all_1a$FP_f, 5, type=c("s"))
m_FP_1a <- movavg(perc_all_1a$FP_m, 5, type=c("s"))
f_TP_1a <- movavg(perc_all_1a$TP_f, 5, type=c("s"))
m_TP_1a <- movavg(perc_all_1a$TP_m, 5, type=c("s"))

ggplot(perc_all_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = f_TP_1a, color = "females, TP")) + 
  geom_line(aes(y = f_FN_1a, color = "females, FN")) +
  geom_line(aes(y = f_FP_1a, color = "females, FP")) +
  geom_line(aes(y = f_TN_1a, color = "females, TN")) +
  geom_line(aes(y = m_TP_1a, color = "males, TP")) + 
  geom_line(aes(y = m_FN_1a, color = "males, FN")) +
  geom_line(aes(y = m_FP_1a, color = "males, FP")) +
  geom_line(aes(y = m_TN_1a, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP's, FN's, TP's, TN's, analysis 1.a")


# display plot of FN's as percentage of ASD individuals only using simple moving average (windows of 9) to smooth line
f_FN_ASD_1a <- movavg(perc_ASD_1a$FN_f, 9, type=c("s"))
m_FN_ASD_1a <- movavg(perc_ASD_1a$FN_m, 9, type=c("s"))
ggplot(perc_ASD_1a, aes(x=1:nrow(wide_1a))) + 
  geom_line(aes(y = f_FN_ASD_1a, color = "females")) +
  geom_line(aes(y = m_FN_ASD_1a, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.a")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1a, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, analysis 1.a") 



############# ANALYSIS 1.b ################

### assign data ###
D_1b <- D_MF


#### Run simulation 1.b #####

# run simulation with 60 % males in diagnostic updating sample (gender ratio 3:2)
results_1b <- run_simulation(full_dataset = D_1b, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.6)

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
  ggtitle("Total FP, FN, TP, TN, analysis 1.b")

# single out the progression of FN's (total count values)
ggplot(wide_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, analysis 1.b")

# what is the trend in FN's among women
ggplot(wide_1b, aes(x = 1:nrow(wide_1b), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, analysis 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1b, aes(x = 1:nrow(wide_1b), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, analysis 1.b") +
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
  ggtitle("Percentage of FP, FN, TP, TN, analysis 1.b")

# single out the progression of FN's (percentage, only ASD)
ggplot(perc_ASD_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.b")

# what is the trend in FN's among women (percentage, only ASD)
ggplot(perc_ASD_1b, aes(x = 1:nrow(wide_1b), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, analysis 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, only ASD)
ggplot(perc_ASD_1b, aes(x = 1:nrow(wide_1b), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, analysis 1.b") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# display plot of FN's, TN's, TP's, and FP's in percentage with simple moving average (windows of 5)
f_FN_1b <- movavg(perc_all_1b$FN_f, 5, type=c("s"))
m_FN_1b <- movavg(perc_all_1b$FN_m, 5, type=c("s"))
f_TN_1b <- movavg(perc_all_1b$TN_f, 5, type=c("s"))
m_TN_1b <- movavg(perc_all_1b$TN_m, 5, type=c("s"))
f_FP_1b <- movavg(perc_all_1b$FP_f, 5, type=c("s"))
m_FP_1b <- movavg(perc_all_1b$FP_m, 5, type=c("s"))
f_TP_1b <- movavg(perc_all_1b$TP_f, 5, type=c("s"))
m_TP_1b <- movavg(perc_all_1b$TP_m, 5, type=c("s"))

ggplot(perc_all_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = f_TP_1b, color = "females, TP")) + 
  geom_line(aes(y = f_FN_1b, color = "females, FN")) +
  geom_line(aes(y = f_FP_1b, color = "females, FP")) +
  geom_line(aes(y = f_TN_1b, color = "females, TN")) +
  geom_line(aes(y = m_TP_1b, color = "males, TP")) + 
  geom_line(aes(y = m_FN_1b, color = "males, FN")) +
  geom_line(aes(y = m_FP_1b, color = "males, FP")) +
  geom_line(aes(y = m_TN_1b, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP's, FN's, TP's, TN's, analysis 1.b")


# display plot of FN's as percentage of ASD individuals only using simple moving average (windows of 9) to smooth line
f_FN_ASD_1b <- movavg(perc_ASD_1b$FN_f, 9, type=c("s"))
m_FN_ASD_1b <- movavg(perc_ASD_1b$FN_m, 9, type=c("s"))
ggplot(perc_ASD_1b, aes(x=1:nrow(wide_1b))) + 
  geom_line(aes(y = f_FN_ASD_1b, color = "females")) +
  geom_line(aes(y = m_FN_ASD_1b, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.b")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1b, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, analysis 1.b") 



############# ANALYSIS 1.c ################

### Assign data ###

D_1c <- D_MF


### Run simulation 1.c ###

# run simulation with 70 % males in diagnostic updating sample
results_1c <- run_simulation(full_dataset = D_1c, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.7)


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
  ggtitle("Total FP, FN, TP, TN, analysis 1.c")

# single out the progression of FN's (total count values)
ggplot(wide_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, analysis 1.c")

# what is the trend in FN's among women
ggplot(wide_1c, aes(x = 1:nrow(wide_1c), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, analysis 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1c, aes(x = 1:nrow(wide_1c), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, analysis 1.c") +
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
  ggtitle("Percentage of FP, FN, TP, TN, analysis 1.c")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.c")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1c, aes(x = 1:nrow(wide_1c), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, analysis 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1c, aes(x = 1:nrow(wide_1c), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, analysis 1.c") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# display plot of FN's, TN's, TP's, and FP's in percentage with simple moving average (windows of 5)
f_FN_1c <- movavg(perc_all_1c$FN_f, 5, type=c("s"))
m_FN_1c <- movavg(perc_all_1c$FN_m, 5, type=c("s"))
f_TN_1c <- movavg(perc_all_1c$TN_f, 5, type=c("s"))
m_TN_1c <- movavg(perc_all_1c$TN_m, 5, type=c("s"))
f_FP_1c <- movavg(perc_all_1c$FP_f, 5, type=c("s"))
m_FP_1c <- movavg(perc_all_1c$FP_m, 5, type=c("s"))
f_TP_1c <- movavg(perc_all_1c$TP_f, 5, type=c("s"))
m_TP_1c <- movavg(perc_all_1c$TP_m, 5, type=c("s"))

ggplot(perc_all_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = f_TP_1c, color = "females, TP")) + 
  geom_line(aes(y = f_FN_1c, color = "females, FN")) +
  geom_line(aes(y = f_FP_1c, color = "females, FP")) +
  geom_line(aes(y = f_TN_1c, color = "females, TN")) +
  geom_line(aes(y = m_TP_1c, color = "males, TP")) + 
  geom_line(aes(y = m_FN_1c, color = "males, FN")) +
  geom_line(aes(y = m_FP_1c, color = "males, FP")) +
  geom_line(aes(y = m_TN_1c, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP's, FN's, TP's, TN's, analysis 1.c")


# display plot of FN's as percentage of ASD individuals only using simple moving average (windows of 9) to smooth line
f_FN_ASD_1c <- movavg(perc_ASD_1c$FN_f, 9, type=c("s"))
m_FN_ASD_1c <- movavg(perc_ASD_1c$FN_m, 9, type=c("s"))
ggplot(perc_ASD_1c, aes(x=1:nrow(wide_1c))) + 
  geom_line(aes(y = f_FN_ASD_1c, color = "females")) +
  geom_line(aes(y = m_FN_ASD_1c, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.c")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1c, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, analysis 1.c") 



############# ANALYSIS 1.d ################

### assign data 1.d ###
D_1d <- D_MF


### Run simulation 1.d ###

# run simulation with 80 % males in diagnostic updating sample
results_1d <- run_simulation(full_dataset = D_1d, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.8)


# assign outcome dataframes to variables 
sim_data_1d <- results_1d[[1]]
vec_data_1d <- results_1d[[2]]
eval_data_1d <- results_1d[[3]]



### Restructure outcome of simulation 1.d ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_1d <- evaluation_long(eval_data_1d)
long_1d <- eval_long_1d[[1]]
eval_wide_1d <- evaluation_wide(eval_data_1d)
wide_1d <- eval_wide_1d[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_1d <- percentage_converter_all(eval_data_1d)

# run function to calculate ratio of FN's to TP's
perc_ASD_1d <- percentage_converter_ASD(eval_data_1d)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_1d <- find_diagnostic_symptoms(vec_data_1d)



### Visualizations via plots and graphs 1.d ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_1d, aes(x=1:nrow(wide_1d))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, analysis 1.d")

# single out the progression of FN's (total count values)
ggplot(wide_1d, aes(x=1:nrow(wide_1d))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, analysis 1.d")

# what is the trend in FN's among women
ggplot(wide_1d, aes(x = 1:nrow(wide_1d), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, analysis 1.d") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1d, aes(x = 1:nrow(wide_1d), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, analysis 1.d") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_1d, aes(x=1:nrow(wide_1d))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, analysis 1.d")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1d, aes(x=1:nrow(wide_1d))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.d")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1d, aes(x = 1:nrow(wide_1d), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, analysis 1.d") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1d, aes(x = 1:nrow(wide_1d), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, analysis 1.d") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# display plot of FN's, TN's, TP's, and FP's in percentage with simple moving average (windows of 5)
f_FN_1d <- movavg(perc_all_1d$FN_f, 5, type=c("s"))
m_FN_1d <- movavg(perc_all_1d$FN_m, 5, type=c("s"))
f_TN_1d <- movavg(perc_all_1d$TN_f, 5, type=c("s"))
m_TN_1d <- movavg(perc_all_1d$TN_m, 5, type=c("s"))
f_FP_1d <- movavg(perc_all_1d$FP_f, 5, type=c("s"))
m_FP_1d <- movavg(perc_all_1d$FP_m, 5, type=c("s"))
f_TP_1d <- movavg(perc_all_1d$TP_f, 5, type=c("s"))
m_TP_1d <- movavg(perc_all_1d$TP_m, 5, type=c("s"))

ggplot(perc_all_1d, aes(x=1:nrow(wide_1d))) + 
  geom_line(aes(y = f_TP_1d, color = "females, TP")) + 
  geom_line(aes(y = f_FN_1d, color = "females, FN")) +
  geom_line(aes(y = f_FP_1d, color = "females, FP")) +
  geom_line(aes(y = f_TN_1d, color = "females, TN")) +
  geom_line(aes(y = m_TP_1d, color = "males, TP")) + 
  geom_line(aes(y = m_FN_1d, color = "males, FN")) +
  geom_line(aes(y = m_FP_1d, color = "males, FP")) +
  geom_line(aes(y = m_TN_1d, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP's, FN's, TP's, TN's, analysis 1.d")


# display plot of FN's as percentage of ASD individuals only using simple moving average (windows of 9) to smooth line
f_FN_ASD_1d <- movavg(perc_ASD_1d$FN_f, 9, type=c("s"))
m_FN_ASD_1d <- movavg(perc_ASD_1d$FN_m, 9, type=c("s"))
ggplot(perc_ASD_1d, aes(x=1:nrow(wide_1d))) + 
  geom_line(aes(y = f_FN_ASD_1d, color = "females")) +
  geom_line(aes(y = m_FN_ASD_1d, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.d")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1d, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, analysis 1.d") 



############# ANALYSIS 1.e ################

### assign data 1.e ###
D_1e <- D_MF


### run simulation 1.e ###

# run simulation with 100 % males in diagnostic updating sample
results_1e <- run_simulation(full_dataset = D_1e, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 0.9)


# assign outcome dataframes to variables 
sim_data_1e <- results_1e[[1]]
vec_data_1e <- results_1e[[2]]
eval_data_1e <- results_1e[[3]]



### Restructure outcome of simulation 1.e ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_1e <- evaluation_long(eval_data_1e)
long_1e <- eval_long_1e[[1]]
eval_wide_1e <- evaluation_wide(eval_data_1e)
wide_1e <- eval_wide_1e[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_1e <- percentage_converter_all(eval_data_1e)

# run function to calculate ratio of FN's to TP's
perc_ASD_1e <- percentage_converter_ASD(eval_data_1e)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_1e <- find_diagnostic_symptoms(vec_data_1e)



### Visualizations via plots and graphs 1.e ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_1e, aes(x=1:nrow(wide_1e))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, analysis 1.e")

# single out the progression of FN's (total count values)
ggplot(wide_1e, aes(x=1:nrow(wide_1e))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, analysis 1.e")

# what is the trend in FN's among women
ggplot(wide_1e, aes(x = 1:nrow(wide_1e), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, analysis 1.e") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1e, aes(x = 1:nrow(wide_1e), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, analysis 1.e") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_1e, aes(x=1:nrow(wide_1e))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, analysis 1.e")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1e, aes(x=1:nrow(wide_1e))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.e")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1e, aes(x = 1:nrow(wide_1e), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, analysis 1.e") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1e, aes(x = 1:nrow(wide_1e), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, analysis 1.e") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# display plot of FN's, TN's, TP's, and FP's in percentage with simple moving average (windows of 5)
f_FN_1e <- movavg(perc_all_1e$FN_f, 5, type=c("s"))
m_FN_1e <- movavg(perc_all_1e$FN_m, 5, type=c("s"))
f_TN_1e <- movavg(perc_all_1e$TN_f, 5, type=c("s"))
m_TN_1e <- movavg(perc_all_1e$TN_m, 5, type=c("s"))
f_FP_1e <- movavg(perc_all_1e$FP_f, 5, type=c("s"))
m_FP_1e <- movavg(perc_all_1e$FP_m, 5, type=c("s"))
f_TP_1e <- movavg(perc_all_1e$TP_f, 5, type=c("s"))
m_TP_1e <- movavg(perc_all_1e$TP_m, 5, type=c("s"))

ggplot(perc_all_1e, aes(x=1:nrow(wide_1e))) + 
  geom_line(aes(y = f_TP_1e, color = "females, TP")) + 
  geom_line(aes(y = f_FN_1e, color = "females, FN")) +
  geom_line(aes(y = f_FP_1e, color = "females, FP")) +
  geom_line(aes(y = f_TN_1e, color = "females, TN")) +
  geom_line(aes(y = m_TP_1e, color = "males, TP")) + 
  geom_line(aes(y = m_FN_1e, color = "males, FN")) +
  geom_line(aes(y = m_FP_1e, color = "males, FP")) +
  geom_line(aes(y = m_TN_1e, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP's, FN's, TP's, TN's, analysis 1.e")


# display plot of FN's as percentage of ASD individuals only using simple moving average (windows of 9) to smooth line
f_FN_ASD_1e <- movavg(perc_ASD_1e$FN_f, 9, type=c("s"))
m_FN_ASD_1e <- movavg(perc_ASD_1e$FN_m, 9, type=c("s"))
ggplot(perc_ASD_1e, aes(x=1:nrow(wide_1e))) + 
  geom_line(aes(y = f_FN_ASD_1e, color = "females")) +
  geom_line(aes(y = m_FN_ASD_1e, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.e")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1e, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, analysis 1.e") 



############# ANALYSIS 1.f ################

### assign data 1.f ###
D_1f <- D_MF


### run simulation 1.f ###

# run simulation with 100 % males in diagnostic updating sample
results_1f <- run_simulation(full_dataset = D_1f, iterations = 100, n_sample = 20, n_diagnostic_symptoms = 5, n_symptom_cutoff = 4, male_percent = 1)


# assign outcome dataframes to variables 
sim_data_1f <- results_1f[[1]]
vec_data_1f <- results_1f[[2]]
eval_data_1f <- results_1f[[3]]



### Restructure outcome of simulation 1.f ###

# run functions to determine FP's, FN's, TP's, and TN's for each iteration
eval_long_1f <- evaluation_long(eval_data_1f)
long_1f <- eval_long_1f[[1]]
eval_wide_1f <- evaluation_wide(eval_data_1f)
wide_1f <- eval_wide_1f[[1]]


# run function to calculate percentage of FP's, FN's, TP's, and TN's for each iteration
perc_all_1f <- percentage_converter_all(eval_data_1f)

# run function to calculate ratio of FN's to TP's
perc_ASD_1f <- percentage_converter_ASD(eval_data_1f)


# run function that makes a dataframe containing information of the pervalence of the symptoms
# being present in the diagnostic criteria
diag_symp_1f <- find_diagnostic_symptoms(vec_data_1f)



### Visualizations via plots and graphs 1.f ###

# create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (total count values)
# uses first list element (the wide format data frame) of the output from evaluation_wide())
ggplot(wide_1f, aes(x=1:nrow(wide_1f))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total FP, FN, TP, TN, analysis 1.f")

# single out the progression of FN's (total count values)
ggplot(wide_1f, aes(x=1:nrow(wide_1f))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives, analysis 1.f")

# what is the trend in FN's among women
ggplot(wide_1f, aes(x = 1:nrow(wide_1f), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Females, analysis 1.f") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men
ggplot(wide_1f, aes(x = 1:nrow(wide_1f), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('n individuals') + xlab('iterations') +
  ggtitle("Total False Negatives in Males, analysis 1.f") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


#  create line plot of the progression of the amount of FP's, FN's, TP's, and TN's for each gender over time (percentage)
ggplot(perc_all_1f, aes(x=1:nrow(wide_1f))) + 
  geom_line(aes(y = TP_f, color = "females, TP")) + 
  geom_line(aes(y = FN_f, color = "females, FN")) +
  geom_line(aes(y = FP_f, color = "females, FP")) +
  geom_line(aes(y = TN_f, color = "females, TN")) +
  geom_line(aes(y = TP_m, color = "males, TP")) + 
  geom_line(aes(y = FN_m, color = "males, FN")) +
  geom_line(aes(y = FP_m, color = "males, FP")) +
  geom_line(aes(y = TN_m, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP, FN, TP, TN, analysis 1.f")

# single out the progression of FN's (percentage, ASD only)
ggplot(perc_ASD_1f, aes(x=1:nrow(wide_1f))) + 
  geom_line(aes(y = FN_f, color = "females")) +
  geom_line(aes(y = FN_m, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.f")

# what is the trend in FN's among women (percentage, ASD only)
ggplot(perc_ASD_1f, aes(x = 1:nrow(wide_1f), y = FN_f)) + 
  geom_point(color = "lightcoral") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Females, analysis 1.f") +
  geom_smooth(method = "lm", se = F, color = "seagreen")

# what is the trend in FN's among men (percentage, ASD only)
ggplot(perc_ASD_1f, aes(x = 1:nrow(wide_1f), y = FN_m)) + 
  geom_point(color = "steelblue") +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives in Males, analysis 1.f") +
  geom_smooth(method = "lm", se = F, color = "seagreen")


# display plot of FN's, TN's, TP's, and FP's in percentage with simple moving average (windows of 5)
f_FN_1f <- movavg(perc_all_1f$FN_f, 5, type=c("s"))
m_FN_1f <- movavg(perc_all_1f$FN_m, 5, type=c("s"))
f_TN_1f <- movavg(perc_all_1f$TN_f, 5, type=c("s"))
m_TN_1f <- movavg(perc_all_1f$TN_m, 5, type=c("s"))
f_FP_1f <- movavg(perc_all_1f$FP_f, 5, type=c("s"))
m_FP_1f <- movavg(perc_all_1f$FP_m, 5, type=c("s"))
f_TP_1f <- movavg(perc_all_1f$TP_f, 5, type=c("s"))
m_TP_1f <- movavg(perc_all_1f$TP_m, 5, type=c("s"))

ggplot(perc_all_1f, aes(x=1:nrow(wide_1f))) + 
  geom_line(aes(y = f_TP_1f, color = "females, TP")) + 
  geom_line(aes(y = f_FN_1f, color = "females, FN")) +
  geom_line(aes(y = f_FP_1f, color = "females, FP")) +
  geom_line(aes(y = f_TN_1f, color = "females, TN")) +
  geom_line(aes(y = m_TP_1f, color = "males, TP")) + 
  geom_line(aes(y = m_FN_1f, color = "males, FN")) +
  geom_line(aes(y = m_FP_1f, color = "males, FP")) +
  geom_line(aes(y = m_TN_1f, color = "males, TN")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of FP's, FN's, TP's, TN's, analysis 1.f")


# display plot of FN's as percentage of ASD individuals only using simple moving average (windows of 9) to smooth line
f_FN_ASD_1f <- movavg(perc_ASD_1f$FN_f, 9, type=c("s"))
m_FN_ASD_1f <- movavg(perc_ASD_1f$FN_m, 9, type=c("s"))
ggplot(perc_ASD_1f, aes(x=1:nrow(wide_1f))) + 
  geom_line(aes(y = f_FN_ASD_1f, color = "females")) +
  geom_line(aes(y = m_FN_ASD_1f, color = "males")) +
  ylab ('% of individuals') + xlab('iterations') +
  ggtitle("Percentage of False Negatives, analysis 1.f")


# "binary heatmap" depicts the progression of the inclusion of symptoms in the diagnostic criteria over time
# uses output from find_diagnostic_symptoms()
heatmap(diag_symp_1f, scale = "none", Rowv = NA, Colv = NA, col = c("orangered3", "seagreen"), main = "Symptom inclusion, analysis 1.f") 



######### LAST VISUALIZATIONS, comparisons among outcomes ######### 

# create data frames containing the individuals who have achieved diagnosis sometime in the last 10 iterations of the updating procedure
# filter the ASD positive individuals from the last 10 iterations, analysis 1.a
ASD_final_1a <- sim_data_1a %>% filter(ASD101 == 1 | ASD100 == 1 | ASD99 == 1 | ASD98 == 1 | ASD97 == 1 | ASD96 == 1 | ASD95 == 1 | ASD94 == 1 | ASD93 == 1 | ASD92 == 1)
ASD_final_1a_f <- ASD_final_1a %>% filter(gender == "F")
ASD_final_1a_m <- ASD_final_1a %>% filter(gender == "M")
# filter the ASD positive individuals from the last 10 iterations, analysis 1.b
ASD_final_1b <- sim_data_1b %>% filter(ASD101 == 1 | ASD100 == 1 | ASD99 == 1 | ASD98 == 1 | ASD97 == 1 | ASD96 == 1 | ASD95 == 1 | ASD94 == 1 | ASD93 == 1 | ASD92 == 1)
ASD_final_1b_f <- ASD_final_1b %>% filter(gender == "F")
ASD_final_1b_m <- ASD_final_1b %>% filter(gender == "M")
# filter the ASD positive individuals from the last 10 iterations, analysis 1.c
ASD_final_1c <- sim_data_1c %>% filter(ASD101 == 1 | ASD100 == 1 | ASD99 == 1 | ASD98 == 1 | ASD97 == 1 | ASD96 == 1 | ASD95 == 1 | ASD94 == 1 | ASD93 == 1 | ASD92 == 1)
ASD_final_1c_f <- ASD_final_1c %>% filter(gender == "F")
ASD_final_1c_m <- ASD_final_1c %>% filter(gender == "M")
# filter the ASD positive individuals from the last 10 iterations, analysis 1.d
ASD_final_1d <- sim_data_1d %>% filter(ASD101 == 1 | ASD100 == 1 | ASD99 == 1 | ASD98 == 1 | ASD97 == 1 | ASD96 == 1 | ASD95 == 1 | ASD94 == 1 | ASD93 == 1 | ASD92 == 1)
ASD_final_1d_f <- ASD_final_1d %>% filter(gender == "F")
ASD_final_1d_m <- ASD_final_1d %>% filter(gender == "M")
# filter the ASD positive individuals from the last 10 iterations, analysis 1.e
ASD_final_1e <- sim_data_1e %>% filter(ASD101 == 1 | ASD100 == 1 | ASD99 == 1 | ASD98 == 1 | ASD97 == 1 | ASD96 == 1 | ASD95 == 1 | ASD94 == 1 | ASD93 == 1 | ASD92 == 1)
ASD_final_1e_f <- ASD_final_1e %>% filter(gender == "F")
ASD_final_1e_m <- ASD_final_1e %>% filter(gender == "M")
# filter the ASD positive individuals from the last 10 iterations, analysis 1.f
ASD_final_1f <- sim_data_1f %>% filter(ASD101 == 1 | ASD100 == 1 | ASD99 == 1 | ASD98 == 1 | ASD97 == 1 | ASD96 == 1 | ASD95 == 1 | ASD94 == 1 | ASD93 == 1 | ASD92 == 1)
ASD_final_1f_f <- ASD_final_1f %>% filter(gender == "F")
ASD_final_1f_m <- ASD_final_1f %>% filter(gender == "M")


# create data frames containing the False negative females and males who have been missed in diagnosis sometime in the last 10 iterations of the updating procedure
# filter the ASD positive individuals from the last 10 iterations, analysis 1.c
FN_final_1a <- sim_data_1a %>% filter(ASD == 1) %>% filter(ASD101 == 0 | ASD100 == 0 | ASD99 == 0 | ASD98 == 0 | ASD97 == 0 | ASD96 == 0 | ASD95 == 0 | ASD94 == 0 | ASD93 == 0 | ASD92 == 0)
FN_final_1a_f <- FN_final_1a %>% filter(gender == "F")
FN_final_1a_m <- FN_final_1a %>% filter(gender == "M")

# filter the ASD positive individuals from the last 10 iterations, analysis 1.c
FN_final_1c <- sim_data_1c %>% filter(ASD == 1) %>% filter(ASD101 == 0 | ASD100 == 0 | ASD99 == 0 | ASD98 == 0 | ASD97 == 0 | ASD96 == 0 | ASD95 == 0 | ASD94 == 0 | ASD93 == 0 | ASD92 == 0)
FN_final_1c_f <- FN_final_1c %>% filter(gender == "F")
FN_final_1c_m <- FN_final_1c %>% filter(gender == "M")

# filter the ASD positive individuals from the last 10 iterations, analysis 1.f
FN_final_1f <- sim_data_1f %>% filter(ASD == 1) %>% filter(ASD101 == 0 | ASD100 == 0 | ASD99 == 0 | ASD98 == 0 | ASD97 == 0 | ASD96 == 0 | ASD95 == 0 | ASD94 == 0 | ASD93 == 0 | ASD92 == 0)
FN_final_1f_f <- FN_final_1f %>% filter(gender == "F")
FN_final_1f_m <- FN_final_1f %>% filter(gender == "M")



# filter the true ASD positive females
D_1af <- D_1a %>% filter(ASD == 1) %>% filter(gender == "F")
D_1bf <- D_1b %>% filter(ASD == 1) %>% filter(gender == "F")
D_1cf <- D_1c %>% filter(ASD == 1) %>% filter(gender == "F")
D_1df <- D_1d %>% filter(ASD == 1) %>% filter(gender == "F")
D_1ef <- D_1e %>% filter(ASD == 1) %>% filter(gender == "F")
D_1ff <- D_1f %>% filter(ASD == 1) %>% filter(gender == "F")
# filter the true ASD positive males
D_1am <- D_1a %>% filter(ASD == 1) %>% filter(gender == "M")
D_1bm <- D_1b %>% filter(ASD == 1) %>% filter(gender == "M")
D_1cm <- D_1c %>% filter(ASD == 1) %>% filter(gender == "M")
D_1dm <- D_1d %>% filter(ASD == 1) %>% filter(gender == "M")
D_1em <- D_1e %>% filter(ASD == 1) %>% filter(gender == "M")
D_1fm <- D_1f %>% filter(ASD == 1) %>% filter(gender == "M")


# Visulize difference in distribution of severities of the 9 symptoms  (difference in symptomatic phenotyåe) in analysis 1.c, 1.a, 1.f
# overlay of FN females from last 10 rounds and all diagnosed individuals frokm last 10 rounds
## analysis 1.a
# S1
plot(density(ASD_final_1a$S1), main = "Symptom 1", col = "tan1", xlab='Severity', ylab='Density')
lines(density(FN_final_1a_f$S1), col = "seagreen")
# S2
plot(density(FN_final_1a_f$S2), main = "Symptom 2", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S2), col = "seagreen")
# S3
plot(density(FN_final_1a_f$S3), main = "Symptom 3", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S3), col = "seagreen")
# S4
plot(density(FN_final_1a_f$S4), main = "Symptom 4", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S4), col = "seagreen")
# S5
plot(density(FN_final_1a_f$S5), main = "Symptom 5", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S5), col = "seagreen")
# S6
plot(density(FN_final_1a_f$S6), main = "Symptom 6", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S6), col = "seagreen")
# S7
plot(density(FN_final_1a_f$S7), main = "Symptom 7", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S7), col = "seagreen")
# S8
plot(density(FN_final_1a_f$S8), main = "Symptom 8", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S8), col = "seagreen")
#S9
plot(density(FN_final_1a_f$S9), main = "Symptom 9", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S9), col = "seagreen")

#### analysis 1.c ###
# S1
plot(density(ASD_final_1c$S1), main = "Symptom 1", col = "seagreen", xlab='Severity', ylab='Density')
  lines(density(FN_final_1c_f$S1), col = "tan1")
# S2
plot(density(FN_final_1c_f$S2), main = "Symptom 2", col = "tan1", xlab='Severity', ylab='Density')
  lines(density(ASD_final_1c$S2), col = "seagreen")
# S3
plot(density(FN_final_1c_f$S3), main = "Symptom 3", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S3), col = "seagreen")
# S4
plot(density(FN_final_1c_f$S4), main = "Symptom 4", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S4), col = "seagreen")
# S5
plot(density(FN_final_1c_f$S5), main = "Symptom 5", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S5), col = "seagreen")
# S6
plot(density(FN_final_1c_f$S6), main = "Symptom 6", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S6), col = "seagreen")
# S7
plot(density(FN_final_1c_f$S7), main = "Symptom 7", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S7), col = "seagreen")
# S8
plot(density(FN_final_1c_f$S8), main = "Symptom 8", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S8), col = "seagreen")
#S9
plot(density(FN_final_1c_f$S9), main = "Symptom 9", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1c$S9), col = "seagreen")

#### analysis 1.f
# S1
plot(density(ASD_final_1f$S1), main = "Symptom 1", col = "tan1", xlab='Severity', ylab='Density')
lines(density(FN_final_1f_f$S1), col = "seagreen")
# S2
plot(density(FN_final_1f_f$S2), main = "Symptom 2", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S2), col = "seagreen")
# S3
plot(density(FN_final_1f_f$S3), main = "Symptom 3", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S3), col = "seagreen")
# S4
plot(density(FN_final_1f_f$S4), main = "Symptom 4", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S4), col = "seagreen")
# S5
plot(density(FN_final_1f_f$S5), main = "Symptom 5", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S5), col = "seagreen")
# S6
plot(density(FN_final_1f_f$S6), main = "Symptom 6", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S6), col = "seagreen")
# S7
plot(density(FN_final_1f_f$S7), main = "Symptom 7", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S7), col = "seagreen")
# S8
plot(density(FN_final_1f_f$S8), main = "Symptom 8", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S8), col = "seagreen")
#S9
plot(density(FN_final_1f_f$S9), main = "Symptom 9", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1f$S9), col = "seagreen")


# Visulize difference in distribution of severities of symptom 3 in all analysis
# tan lines are FN's while green lines signify diagnosed population of females
# difference in density (females) - symptom 3 - analysis 1.a
plot(density(ASD_final_1a_f$S3), main = "Analysis 1.a, females", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1af$S3), col = "tan1")

# difference in density (females) - symptom 3 - analysis 1.b
plot(density(ASD_final_1b_f$S3), main = "Analysis 1.b, females", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1bf$S3), col = "tan1")

# difference in peak density (females) - symptom 3 - analysis 1.c
plot(density(ASD_final_1c_f$S3), main = "Analysis 1.c, females", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1cf$S3), col = "tan1")

# difference in density (females) - symptom 3 - analysis 1.d
plot(density(ASD_final_1d_f$S3), main = "Analysis 1.d, females", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1df$S3), col = "tan1")

# difference in density (females) - symptom 3 - analysis 1.e
plot(density(ASD_final_1e_f$S3), main = "Analysis 1.e, females", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1ef$S3), col = "tan1")

# difference in density (females) - symptom 3 - analysis 1.f
plot(density(ASD_final_1f_f$S3), main = "Analysis 1.f, females", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1ff$S3), col = "tan1")



# visulize density distribution diference symptom 3 (both men and women who are diagnosed with AS compared to FN females)
# difference in symp density ASD (females + males) - symptom 3 - analysis 1.a
plot(density(D_1a$S3), main = "Analysis 1.a", col = "tan1", xlab='Severity', ylab='Density')
lines(density(ASD_final_1a$S3), col = "seagreen")

# difference in symp density ASD (females + males) - symptom 3 - analysis 1.b
plot(density(ASD_final_1b$S3), main = "Analysis 1.b", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1b$S3), col = "tan1")

# difference in symp density ASD (females + males) - symptom 3 - analysis 1.c
plot(density(ASD_final_1c$S3), main = "Analysis 1.c", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1c$S3), col = "tan1")

# difference in symp density ASD (females + males) - symptom 3 - analysis 1.d
plot(density(ASD_final_1d$S3), main = "Analysis 1.d", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1d$S3), col = "tan1")

# difference in symp density ASD (females + males) - symptom 3 - analysis 1.e
plot(density(ASD_final_1e$S3), main = "Analysis 1.e", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1e$S3), col = "tan1")

# difference in symp density ASD (females + males) - symptom 3 - analysis 1.f
plot(density(ASD_final_1f$S3), main = "Analysis 1.f", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1f$S3), col = "tan1")


# visulize density distribution diferences symptom 7
# difference in symp density ASD (females + males) - symptom 7 - analysis 1.a
plot(density(ASD_final_1a$S7), main = "Analysis 1.a", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1a$S7), col = "tan1")

# difference in symp density ASD (females + males) - symptom 7 - analysis 1.b
plot(density(ASD_final_1b$S7), main = "Analysis 1.b", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1b$S7), col = "tan1")

# difference in symp density ASD (females + males) - symptom 7 - analysis 1.c
plot(density(ASD_final_1c$S7), main = "Analysis 1.c", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1c$S7), col = "tan1")

# difference in symp density ASD (females + males) - symptom 7 - analysis 1.d
plot(density(ASD_final_1d$S7), main = "Analysis 1.d", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1d$S7), col = "tan1")

# difference in symp density ASD (females + males) - symptom 7 - analysis 1.e
plot(density(ASD_final_1e$S7), main = "Analysis 1.e", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1e$S7), col = "tan1")

# difference in symp density ASD (females + males) - symptom 7 - analysis 1.f
plot(density(ASD_final_1f$S7), main = "Analysis 1.f", col = "seagreen", xlab='Severity', ylab='Density')
lines(density(D_1f$S7), col = "tan1")
