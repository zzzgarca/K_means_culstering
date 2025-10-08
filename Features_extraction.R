arguments<- commandArgs(trailingOnly = TRUE) #the script name is ignored


#checking for needed packages
if (length(arguments) < 4) {
  stop("Not enough arguments provided. Please provide at least 4 arguments.")
}

if (!require("dplyr")) {
  stop("Package 'dplyr' not found. Please install it using install.packages('dplyr').", call. = FALSE)
}

if (!require("ggplot2")) {
  stop("Package 'ggplot2' not found. Please install it using install.packages('ggplot2').", call. = FALSE)
}

if (!require("nlme")) {
  stop("Package 'nlme' not found. Please install it using install.packages('nlme').", call. = FALSE)
}

if (!require("psych")) {
  stop("Package 'psych' not found. Please install it using install.packages('psych').", call. = FALSE)
}

if (!require("tidyr")) {
  stop("Package 'tidyr' not found. Please install it using install.packages('tidyr').", call. = FALSE)
}



library(dplyr)
library(ggplot2)
library(nlme)
library(psych)
library(tidyr)
options(scipen=9999)
options(max.print = 1000)


input_file <-  arguments[1]
measurements<-arguments[2]
f_start<- arguments[3]
y_start<- arguments[4]


if (is.na(measurements) || is.na(f_start) || is.na(y_start)) {
  stop("Arguments 2-4 must be numeric.")
}


directory_location <- dirname(input_file)
file_name <- basename(input_file)
data<-read.csv(input_file)


##these are here for debugging purposes
#data<-read.csv("/Users/silviumatu/Desktop/Code/R/Software project 2/Demo/Imputation_1/imputed_data_1.csv")
#directory_location <- dirname("/Users/silviumatu/Desktop/Code/R/Software project 2/Demo/Imputation_1/Imputed_data_1.csv")
#file_name <- basename("/Users/silviumatu/Desktop/Code/R/Software project 2/Demo/Imputation_1/imputed_data_1.csv")
#measurements<-4
#f_start<- 4
#y_start<- 64


no_f_variables=(y_start-f_start)/measurements
no_y_variables=(ncol(data)-y_start+1)/measurements
no_f_y_variables=no_f_variables+no_y_variables

##these are for debugging purposes
column_numbers <- 1:(f_start-1)
column_names <- names(data)[column_numbers]
column_names



##transforming into long format
data1 <- data %>%
  pivot_longer(
    cols = -c(column_names),
    names_to = c("time_1", ".value"),
    names_pattern = "(m\\d\\d)_(f\\d\\d|y\\d\\d)"
  ) %>%
  mutate(time_1 = as.numeric(gsub("m", "", time_1)))

data1$time_1<-data1$time_1-1
data1$time_2<-data1$time_1^2
data1$time_3<-data1$time_1^3

processed_file_name<-paste0(directory_location, "/processed_", file_name)
write.csv(data1, processed_file_name, row.names = FALSE)

f_start_data1<-f_start+1 #we get the time_1 variable before the features
y_start_data1<-f_start_data1+no_f_variables

##descriptive statistics for the original features
features_for_descriptives <-subset(data1, select=-c(id, time_1, time_2, time_3))
descriptive_stats <- describe(features_for_descriptives,)
print(descriptive_stats)
desciptive_stats_file_name<-paste0(directory_location, "/desciptives_", file_name)
write.csv(descriptive_stats, desciptive_stats_file_name, row.names = TRUE)


##a violin plot for the original features
original_features <- data1 %>%
  pivot_longer(
    cols=all_of(f_start_data1:(f_start_data1+no_f_y_variables-1)),
    names_to = "Variable", 
    values_to = "Value"
  )

original_violin<-ggplot(original_features, aes(x = Variable, y = Value)) +
  geom_violin(trim = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Distributuions of original features")+
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
original_violin

original_violin_file_name<-paste0(directory_location, "/original_featrures_violin.pdf")
ggsave(original_violin_file_name, plot = original_violin, width = 10, height = 10, dpi = 300)


##new empty data set initialized to be filled with the features
data2 <- data.frame(
  id = rep(NA, nrow(data)))

data2$id <- as.numeric(rownames(data2))
data <- data[order(data$id), ]
data2$x1<-data$x1 #we initializing with values from data, because the extracted features are a short format data-set
data2$x2<-data$x2

##a vector with the features
features_for_analysis <- names(data1)[c(f_start_data1:(f_start_data1+no_f_variables-1))]
features_for_analysis

##this is where we compute the features (using just time^1 random effects)
for (var in features_for_analysis) {
  formula_string <- paste(var, "~ time_1")
  model_formula <- as.formula(formula_string)
  model <- lme(model_formula,
               random = list(id = pdSymm(~time_1)), #this is the random effects co-variance matrix
               na.action = na.omit,
               correlation = corSymm(form = ~ 1 | id), #this is the repeated measures correlation structure
               weights     = varIdent(form=~1|time_1),
               data = data1, method = 'REML',
               control = lmeControl(maxIter = 1000, msMaxIter = 1000))
  participant_effects <- ranef(model)
  participant_effects$id <- rownames(participant_effects)
  col_1 <- paste("intercept", var, sep = "_")
  col_2 <- paste("time_1", var, sep = "_")
  colnames(participant_effects)[1] <- col_1
  colnames(participant_effects)[2] <- col_2
  participant_effects[,1]<-(participant_effects[,1]-mean(participant_effects[,1]))/sd(participant_effects[,1])
  participant_effects[,2]<-(participant_effects[,2]-mean(participant_effects[,2]))/sd(participant_effects[,2])
  data2 <- merge(data2, participant_effects, by = "id", all.x = TRUE)
}

##there are the fixed features
additional_features <- names(data1)[c(2:(f_start_data1-1))]
additional_features <- additional_features[additional_features != "time_1"]
additional_features

##we are standardizing fixed features
for (var in additional_features) {
  data2[[var]]<-(data2[[var]]-mean(data2[[var]]))/sd(data2[[var]])
}

extracted_features_time_1_file_name<-paste0(directory_location, "/features_time_1_", file_name)
write.csv(data2, extracted_features_time_1_file_name, row.names = FALSE)

f_start_data2<-f_start_data1-1 #because time_1 variables is no longer in the data set

##a violin for the time^1 features
time_1_features_violin <- data2 %>%
  pivot_longer(all_of(f_start_data2:ncol(data2)),
               names_to = "Variable", values_to = "Value")

extracted_time_1_violin<-ggplot(time_1_features_violin, aes(x = Variable, y = Value)) +
  geom_violin(trim = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

extracted_time_1_violin_file_name<-paste0(directory_location, "/extracted_featrures_time_1_violin.pdf")
ggsave(extracted_time_1_violin_file_name, plot = extracted_time_1_violin, width = 10, height = 10, dpi = 300)

##descriptive statistics for the time^1 which also includes the fixed features
time_1_descriptives <-subset(data2, select=-c(id))
descriptives_time_1_features <- describe(time_1_descriptives,)
print(descriptives_time_1_features)
descriptives_time_1_features_file_name<-paste0(directory_location, "/desciptives_time_1_", file_name)
write.csv(descriptives_time_1_features, descriptives_time_1_features_file_name, row.names = TRUE)

##making a correlation plot for time 1 features; selecting all features
time_1_features <- subset(data2, select=-c(id))
time_1_correlation_matrix <- cor(time_1_features, use = "complete.obs")
time_1_correlation_matrix_file_name<-paste0(directory_location, "/time_1_correlation_matrix_", file_name)
time_1_correlation_plot_file_name<-paste0(directory_location, "/time_1_correlation_plot.pdf")
write.csv(time_1_correlation_matrix, time_1_correlation_matrix_file_name, row.names = TRUE)
pdf(time_1_correlation_plot_file_name, width = 10, height = 10)
corrplot(time_1_correlation_matrix, method = "circle", tl.col = "black", tl.cex=0.9)
dev.off()



###bellow is the code for time^1, time^2, and time^3 feature extraction

##new empty data set initialized to be filled with the features
data3 <- data.frame(
  id = rep(NA, nrow(data)))

data3$id <- as.numeric(rownames(data3))
data <- data[order(data$id), ]
data3$x1<-data$x1 #we initializing with values from data, because the extracted features are a short format data-set
data3$x2<-data$x2

##features_for_analysis analysis are the same as above (include all 15 f features)
features_for_analysis

##this is where we compute the features using time^1, time^2, and time^3  random effects
for (var in features_for_analysis) {
  formula_string_v2 <- paste(var, "~ time_1 + time_2 + time_3")
  model_formula_v2 <- as.formula(formula_string_v2)
  model_v2 <- lme(model_formula_v2,
               random = list(id = pdSymm(~time_1 + time_2 + time_3)), #this is the random effects co-variance matrix
               na.action = na.omit,
               correlation = corSymm(form = ~ 1 | id), #id goes here to indicate at which level to expect the correlation
               weights     = varIdent(form=~1|time_1), #only time 1 here because we are indicting with time moments are we referring two
               data = data1, method = 'REML',
               control = lmeControl(maxIter = 1000, msMaxIter = 1000))
  participant_effects_v2 <- ranef(model_v2)
  participant_effects_v2$id <- rownames(participant_effects_v2)
  col_1_v2 <- paste("intercept", var, sep = "_")
  col_2_v2 <- paste("time_1", var, sep = "_")
  col_3_v2 <- paste("time_2", var, sep = "_")
  col_4_v2 <- paste("time_3", var, sep = "_")
  colnames(participant_effects_v2)[1] <- col_1_v2
  colnames(participant_effects_v2)[2] <- col_2_v2
  colnames(participant_effects_v2)[3] <- col_3_v2
  colnames(participant_effects_v2)[4] <- col_4_v2
  participant_effects_v2[,1]<-(participant_effects_v2[,1]-mean(participant_effects_v2[,1]))/sd(participant_effects_v2[,1])
  participant_effects_v2[,2]<-(participant_effects_v2[,2]-mean(participant_effects_v2[,2]))/sd(participant_effects_v2[,2])
  participant_effects_v2[,3]<-(participant_effects_v2[,3]-mean(participant_effects_v2[,3]))/sd(participant_effects_v2[,3])
  participant_effects_v2[,4]<-(participant_effects_v2[,4]-mean(participant_effects_v2[,4]))/sd(participant_effects_v2[,4])
  data3 <- merge(data3, participant_effects_v2, by = "id", all.x = TRUE)
}


##we are standardizing fixed features
##the additional_features list is the same as above so we are re-using it
additional_features
for (var in additional_features) {
  data3[[var]]<-(data3[[var]]-mean(data3[[var]]))/sd(data3[[var]])
}

extracted_features_time_all_file_name<-paste0(directory_location, "/features_time_all_", file_name)
write.csv(data3, extracted_features_time_all_file_name, row.names = FALSE)

f_start_data3<-f_start_data2 #in data 2 and data 3, f variables start from the same point

##a violin for all times features; !!!is going to be very large
time_all_features_violin <- data3 %>%
  pivot_longer(all_of(f_start_data3:ncol(data3)),
               names_to = "Variable", values_to = "Value")

extracted_time_all_violin<-ggplot(time_all_features_violin, aes(x = Variable, y = Value)) +
  geom_violin(trim = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

extracted_time_all_violin_file_name<-paste0(directory_location, "/extracted_featrures_time_all_violin.pdf")
ggsave(extracted_time_all_violin_file_name, plot = extracted_time_all_violin, width = 30, height = 10, dpi = 300)

##descriptive statistics for all times features
time_all_descriptives <-subset(data3, select=-c(id))
descriptives_time_all_features <- describe(time_all_descriptives,)
print(descriptives_time_all_features)
descriptives_time_all_features_file_name<-paste0(directory_location, "/desciptives_time_all_", file_name)
write.csv(descriptives_time_all_features, descriptives_time_all_features_file_name, row.names = TRUE)

##making a correlation plot for all times features; selecting all features
time_all_features <- subset(data3, select=-c(id))
time_all_correlation_matrix <- cor(time_all_features, use = "complete.obs")
time_all_correlation_matrix_file_name<-paste0(directory_location, "/time_all_correlation_matrix_", file_name)
time_all_correlation_plot_file_name<-paste0(directory_location, "/time_all_correlation_plot.pdf")
write.csv(time_all_correlation_matrix, time_all_correlation_matrix_file_name, row.names = TRUE)
pdf(time_all_correlation_plot_file_name, width = 30, height = 30)
corrplot(time_all_correlation_matrix, method = "circle", tl.col = "black", tl.cex=0.9)
dev.off()



##extracting y features for computing a "correct" classification
y_features <- names(data)[c(y_start:ncol(data))]
y_features
y_data<-subset(data, select=c("id",y_features))
selected_y_features<-y_features[1:3]
selected_y_features

##a new data frame will contain the reliable change for y features
data4 <- data.frame(
  id = rep(NA, nrow(data)))
data4$id <- as.numeric(rownames(data4))
data <- data[order(data$id), ]

i<-1
steps=no_y_variables*(measurements-1)
for (j in y_start:(y_start+2)) {
  y_variable_name<-paste0("RC_y0",i)
  data4[[y_variable_name]]<-data[,j+steps]-data[,j]
  data4[[y_variable_name]]<-data4[[y_variable_name]]/sd(data4[[y_variable_name]])
  data4[[y_variable_name]] <- ifelse(data4[[y_variable_name]] >= 1.96, 1, 
                                     ifelse(data4[[y_variable_name]] <= -1.96, -1, 0))
  i<-i+1
}


RC_file_name<-paste0(directory_location, "/RC_", file_name)
write.csv(data4, RC_file_name, row.names = FALSE)

##we are also computing a milder version of the reliable change
data5 <- data.frame(
  id = rep(NA, nrow(data)))
data5$id <- as.numeric(rownames(data5))
data <- data[order(data$id), ]
i<-1
for (j in y_start:(y_start+2)) {
  y_variable_name_v2<-paste0("RC_y0",i)
  data5[[y_variable_name_v2]]<-data[,j+steps]-data[,j]
  data5[[y_variable_name_v2]]<-data5[[y_variable_name_v2]]/sd(data5[[y_variable_name_v2]])
  data5[[y_variable_name_v2]] <- ifelse(data5[[y_variable_name_v2]] >= 1, 1, 
                                     ifelse(data5[[y_variable_name_v2]] <= -1, -1, 0))
  i<-i+1
}

RC_mild_file_name<-paste0(directory_location, "/RC_mild_", file_name)
write.csv(data5, RC_mild_file_name, row.names = FALSE)

rm(list = ls())
