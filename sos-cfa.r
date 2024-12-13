library(foreign)
library(lavaan)

experience_data <- read.csv("sos-experience-results.csv")
values_data <- read.csv("sos-values-results.csv")
# nrow(experience_data)
# colnames(experience_data)
View(experience_data)

# Clean the experience data for CFA
experience_data[experience_data == "Strongly agree"] <- 5.0
experience_data[experience_data == "Somewhat agree"] <- 4.0
experience_data[experience_data == "Neutral"] <- 3.0
experience_data[experience_data == "Somewhat disagree"] <- 2.0
experience_data[experience_data == "Strongly disagree"] <- 1.0
experience_data <- experience_data[-c(1),]
View(experience_data)

# Clean the values data for CFA
values_data[values_data == "Extremely important"] <- 5.0
values_data[values_data == "Very important"] <- 4.0
values_data[values_data == "Slightly important"] <- 3.0
values_data[values_data == "Moderately important"] <- 2.0
values_data[values_data == "Not important at all"] <- 1.0
values_data <- values_data[-c(1),]
View(values_data)

# Run the CFA for values
values_uncorrelated_model <- "climate =~ Q1_1 + Q1_2 + Q1_3 + Q1_4 + Q1_5
          structure =~ Q5_1 + Q5_2 + Q5_3 + Q5_4 + Q5_5
         vibrancy =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6 + Q10_7
         climate ~~ 0 * structure
         climate ~~ 0 * vibrancy
         structure ~~ 0 * vibrancy"
values_correlated_model <- "climate =~ Q1_1 + Q1_2 + Q1_3 + Q1_4 + Q1_5
          structure =~ Q5_1 + Q5_2 + Q5_3 + Q5_4 + Q5_5
         vibrancy =~ Q10_1 + Q10_2 + Q10_3 + Q10_4 + Q10_5 + Q10_6 + Q10_7"
values_uncorr_analysis <- cfa(values_uncorrelated_model, data = values_data, std.lv = TRUE)
values_corr_analysis <- cfa(values_correlated_model, data = values_data, std.lv = TRUE)
summary(values_uncorr_analysis, fit.measures = TRUE, standardized = TRUE)
summary(values_corr_analysis, fit.measures = TRUE, standardized = TRUE)

# Run the CFA for experiences
exp_uncorrelated_model <- "climate =~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q4_1
          structure =~ Q6_1 + Q6_2 + Q6_3 + Q6_4 + Q6_5 + Q6_6 + Q6_7 + Q6_8 + Q6_9 + Q9_1
         vibrancy =~ Q11_1 + Q11_2 + Q11_3 + Q11_4 + Q11_5 + Q11_6 + Q11_7 + Q11_8 + Q11_9 + Q11_10 + Q13_1
         climate ~~ 0 * structure
         climate ~~ 0 * vibrancy
         structure ~~ 0 * vibrancy"
exp_correlated_model <- "climate =~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q4_1
          structure =~ Q6_1 + Q6_2 + Q6_3 + Q6_4 + Q6_5 + Q6_6 + Q6_7 + Q6_8 + Q6_9 + Q9_1
         vibrancy =~ Q11_1 + Q11_2 + Q11_3 + Q11_4 + Q11_5 + Q11_6 + Q11_7 + Q11_8 + Q11_9 + Q11_10 + Q13_1"
exp_uncorr_analysis <- cfa(exp_uncorrelated_model, data = experience_data, std.lv = TRUE)
exp_corr_analysis <- cfa(exp_correlated_model, data = experience_data, std.lv = TRUE)
summary(exp_uncorr_analysis, fit.measures = TRUE, standardized = TRUE)
summary(exp_corr_analysis, fit.measures = TRUE, standardized = TRUE)

# I couldn't find an easy way to write these results to file
# so I just copy/paste it instead
# exp_uncorr_file_conn <- file("experiences-uncorrelated-results.txt")
# exp_corr_file_conn <- file("experiences-correlated-results.txt")
# exp_uncorr_res <- summary(exp_uncorr_analysis, fit.measures = TRUE, standardized = TRUE)
# exp_corr_res <- summary(exp_corr_analysis, fit.measures = TRUE, standardized = TRUE)
# lapply(exp_uncorr_res, write, exp_uncorr_file_conn, append=FALSE)
# writeLines(c(exp_corr_res), exp_corr_file_conn)