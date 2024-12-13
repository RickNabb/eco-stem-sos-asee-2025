library(foreign)
library(lavaan)

experience_data <- read.csv("sos-experience-results.csv")
# nrow(experience_data)
# colnames(experience_data)
View(experience_data)

experience_data[experience_data == "Strongly agree"] <- 5.0
experience_data[experience_data == "Somewhat agree"] <- 4.0
experience_data[experience_data == "Neutral"] <- 3.0
experience_data[experience_data == "Somewhat disagree"] <- 2.0
experience_data[experience_data == "Strongly disagree"] <- 1.0
experience_data <- experience_data[-c(1),]
View(experience_data)


# Run the CFA
uncorrelated_model <- "climate =~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q4_1
          structure =~ Q6_1 + Q6_2 + Q6_3 + Q6_4 + Q6_5 + Q6_6 + Q6_7 + Q6_8 + Q6_9 + Q9_1
         vibrancy =~ Q11_1 + Q11_2 + Q11_3 + Q11_4 + Q11_5 + Q11_6 + Q11_7 + Q11_8 + Q11_9 + Q11_10 + Q13_1
         climate ~~ 0 * structure
         climate ~~ 0 * vibrancy
         structure ~~ 0 * vibrancy"
correlated_model <- "climate =~ Q2_1 + Q2_2 + Q2_3 + Q2_4 + Q2_5 + Q4_1
          structure =~ Q6_1 + Q6_2 + Q6_3 + Q6_4 + Q6_5 + Q6_6 + Q6_7 + Q6_8 + Q6_9 + Q9_1
         vibrancy =~ Q11_1 + Q11_2 + Q11_3 + Q11_4 + Q11_5 + Q11_6 + Q11_7 + Q11_8 + Q11_9 + Q11_10 + Q13_1"
analysis <- cfa(correlated_model, data = experience_data, std.lv = TRUE)
summary(analysis, fit.measures = TRUE, standardized = TRUE)
