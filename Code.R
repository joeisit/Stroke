##################################################
#     CODE FOR THE FINAL REPORT OF STROKE        #
##################################################
# Author: José Ramón Riesgo Escovar              #
# Date: May 2021                                 #
##################################################

#########################################
# LOADING ALL LIBRARIES THAT WILL BE USED
#########################################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(qwraps2)
library(markdown)
library(lattice)
library(caret)
library(knitr)
library(corrplot)
library(mda)
library(arm)
#########################################

##################################################
#         LOAD THE DATA SET OF STROKE            #             
##################################################

# You should download the stroke.csv to your working directory
# Load the Data from 

stroke_data <- read.csv("stroke.csv", header = TRUE)

dim(stroke_data)

# 1 row:  id
# Due that the ID is just a identification of the patient we will just remove this column, it
# will not add value to our analysis so we will delete this column

stroke_data <- subset( stroke_data, select = -id )

# 2 row: gender
#We will now review what distribution we have in the gender column
stroke_data %>%
  group_by(gender) %>%
  summarise(total = n())

#We see just one patient in the entire dataset with Other in gender,
#so for the full dataset it is insignificant for the mode and when we
# split the dataset between training and test data there will not be
#sufficient information so we will delete the row.
stroke_data <- subset (stroke_data, stroke_data$gender !="Other")

# We are going to generate a pie chart of the distribution between genders
stroke_data %>%
  group_by(gender) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=gender)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100), "%")),
            position = position_stack(vjust = 0.5)) +
  # Define blue colors
  scale_fill_manual(values=c("#55DDE0", "#33658A")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT GENDER DISTRIBUTION") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 8, 
                                  face = "bold", hjust = 0.5))

# For the analysis 

# 3 row: age

# We review the information of the age of the patients
summary(stroke_data$age)

# We generate a chart of the distribution of the age of the patients, grouping in 10 by 10 years
stroke_data %>%
  mutate(cuts = cut(age, seq(from = 0, to = 100, by = 10))) %>% 
  group_by(cuts) %>%
  summarise(total = n()) %>%
  ggplot(aes(x=cuts,y=total)) +
  geom_bar(stat= "identity", fill = "#00AAFF") +
  scale_fill_gradient() +
  labs(title = " DISTRIBUTION OF AGE OF PATIENTS") +
  xlab("GROUP BY RANGE OF 10 YEARS") + 
  ylab("NUMBER OF PATIENTS WITHIN RANGE") +
  theme(plot.title = element_text(color = "#0066FF", size = 8, 
                                  face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(color = "#777777", size = 7, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "#777777", size = 7, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5, angle = 90)) +
  theme(axis.text.y = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) 

# Box plot Age against stroke to visualize any trend
stroke_data %>%
  mutate(text = ifelse(stroke==0,"Healthy","Stroke")) %>%
  ggplot(aes(x = text, y = age)) +
  geom_boxplot(fill=c("#FF0000","#44FFFF"), colour= "#0000FF") +
  scale_y_continuous(name = "Patients Age") +
  scale_x_discrete(name = "Patients Status") +
  ggtitle("PATIENTS") +
  theme(plot.title = element_text(color = "#0066FF", size = 13, 
                                  face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) +
  theme(axis.text.y = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) 

# We can see that there is a clear tendency for patients of higher age to have strokes
  
# 4 row : hypertension

#We will now review what distribution we have in the hypertension column within the patients
# move the 0 to without hypertension and the 1 to with hypertension to make it more easy to 
# understand the distribution of the patients
stroke_data %>%
  mutate(text = ifelse(hypertension==0,"Without Hypertension","With Hypertension")) %>%
  group_by(text) %>%
  summarise(total = n())


# We are going to generate a pie chart of the distribution between patients
# with hypertension and without hypertension
stroke_data %>%
  mutate(text = ifelse(hypertension==0,"Without Hypertension","With Hypertension")) %>%
  group_by(text) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=text)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100), "%")),
            position = position_stack(vjust = 0.5), size= 3) +
  # Define blue colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT HYPERTENSION DISTRIBUTION") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 8, 
                                  face = "bold", hjust = 0.5))

# 5 row : Heart disease 

#We will now review what distribution we have with patients within the heart problems
# move the 0 to without heat problems and the 1 to with heart problems to make it more easy to 
# understand the distribution of the patients
stroke_data %>%
  mutate(text = ifelse(heart_disease==0,"Without Heart Problem","With Heart Problem")) %>%
  group_by(text) %>%
  summarise(total = n())


# We are going to generate a pie chart of the distribution between patients
# with heart disease and without heart disease
stroke_data %>%
  mutate(text = ifelse(heart_disease==0,"Without Heart Problem","With Heart Problem")) %>%
  group_by(text) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=text)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100), "%")),
            position = position_stack(vjust = 0.5), color = "#000000", size=3) +
  # Define blue colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT HEART DISEASE DISTRIBUTION") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 7, 
                                  face = "bold", hjust = 0.5))

# row 6 : ever married

#We will now review what distribution we have with patients if they were married or not
# We put in a text of married, not married just to make it more clear
stroke_data %>%
  mutate(text = ifelse(ever_married=="Yes","Married","Not Married")) %>%
  group_by(text) %>%
  summarise(total = n())

# We are going to generate a pie chart of the distribution between patients
# that were married and patients that were never married
stroke_data %>%
  mutate(text = ifelse(ever_married=="Yes","Married","Not Married")) %>%
  group_by(text) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=text)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100), "%")),
            position = position_stack(vjust = 0.5), color = "#000000") +
  # Define blue colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT MARRIED DISTRIBUTION") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 7, 
                                  face = "bold", hjust = 0.5))

# row 7 : work_type 

# We will review the distribution of the patients work_type
# We have Children, Government_Job, Never_Worked, Private Sector, Self-employed
stroke_data %>%
  group_by(work_type) %>%
  summarise(total = n())

# We are going to generate a pie chart of the distribution of the patients
# and if they have worked and in what work, unless they are children
stroke_data %>%
  group_by(work_type) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=work_type)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100,digits = 1), "%")),
            position = position_stack(vjust = 0.5), color = "#FFFFFF", size= 2) +
  # Define blue colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF", "#FFAAFF", "#3322AA", "#11FFAA")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT WORK DISTRIBUTION") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 7, 
                                  face = "bold", hjust = 0.5))

# row 8: Residence_type

# We will review the distribution of the patients based on their residence_type
stroke_data %>%
  group_by(Residence_type) %>%
  summarise(total = n())

# We will show a pie chart of the distribution of the type pf residence of the patients
stroke_data %>%
  group_by(Residence_type) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=Residence_type)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100), "%")),
            position = position_stack(vjust = 0.5), color = "#FFFFFF") +
  # Define blue colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT RESIDENCE DISTRIBUTION") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 7, 
                                  face = "bold", hjust = 0.5))

# row 9: Glucose Levels

# We review the information of the average glucose levels
summary(stroke_data$avg_glucose_level)

# We generate a chart of the distribution of the average glucose level of the  patients, grouping in 10 by 10
stroke_data %>%
  mutate(cuts = cut(avg_glucose_level, seq(from = 50, to = 280, by = 10))) %>% 
  group_by(cuts) %>%
  summarise(total = n()) %>%
  ggplot(aes(x=cuts,y=total)) +
  geom_bar(stat= "identity", fill = "#00AAFF") +
  scale_fill_gradient() +
  labs(title = " DISTRIBUTION OF AVERAGE GLUCOSE LEVELS") +
  xlab("GROUP BY RANGE OF 10 LEVELES") + 
  ylab("NUMBER OF PATIENTS WITHIN RANGE") +
  theme(plot.title = element_text(color = "#0066FF", size = 8, 
                                  face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(color = "#777777", size = 7, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "#777777", size = 7, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5, angle = 90)) +
  theme(axis.text.y = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) 

# Let analyze if there is any tendency with the level of glucose and the probability 
# of a stroke
stroke_data %>%
  mutate(text = ifelse(stroke==0,"Healthy","Stroke")) %>%
  ggplot(aes(x = text, y = avg_glucose_level)) +
  geom_boxplot(fill=c("#FF0000","#44FFFF"), colour= "#0000FF") +
  scale_y_continuous(name = "Patients Glucose Level") +
  scale_x_discrete(name = "Patients Status") +
  ggtitle("PATIENTS") +
  theme(plot.title = element_text(color = "#0066FF", size = 13, 
                                  face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) +
  theme(axis.text.y = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) 

# Based on the boxplot, patients with higher glucose level tend to have strokes.


# row 10: bmi: Body Mass Index

# We review the information of the bmi levels, there are patients with N/A so we
# need to ignore them when making the calculation if not it will give us an error
# And also the dataset has the information of bmi as a string so we will convert to 
# a number for the summary
# To avoid warning due to N/A will disable them during this code execution
options(warn = -1) # To avoid Warning due to N/A
summary(as.numeric(stroke_data$bmi), na.rm=TRUE)

# How many records we have without bmi information and that have not had any stroke
sum(stroke_data$bmi=="N/A" & stroke_data$stroke==0)


# How many records we have without bmi information and that had a stroke
sum(stroke_data$bmi=="N/A" & stroke_data$stroke==1)

# Calculate the mean of bmi
bmimean <- mean(as.numeric(stroke_data$bmi), na.rm = TRUE)

# So the subjects that do not have information in bmi are relevant for our study
# So we will generate a new column in the dataset with the bmi_number where we will
# pass the string to a number and for the N/A we will assign them the bmimean 
# so the patients can be used in the models for prediction of stroke  
stroke_data <- stroke_data %>%
  mutate( bmi_num = ifelse(bmi=="N/A",bmimean,as.numeric(bmi)))
# Return to normal warnings 
options(warn = 0L)
# Clearing the temporary variable bmimean to keep the environment clean
rm(bmimean)

# box plot of the distribution of the patients that had a stroke and the healthy ones
stroke_data %>%
  mutate(text = ifelse(stroke==0,"Healthy","Stroke")) %>%
  ggplot(aes(x = text, y = bmi_num)) +
  geom_boxplot(fill=c("#FF0000","#44FFFF"), colour= "#0000FF") +
  scale_y_continuous(name = "Patients BMI Level") +
  scale_x_discrete(name = "Patients Status") +
  ggtitle("PATIENTS") +
  theme(plot.title = element_text(color = "#0066FF", size = 13, 
                                  face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) +
  theme(axis.text.y = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) 

# In a research of the BMI information we see the following:

# knitr::include_graphics("images/bmi.png")

# All the outliers of patients above 50 will be adjusted 50 to avoid distortion
# in the models

# With the following code we adjust this:

stroke_data <- stroke_data %>%
  mutate(bmi_num = ifelse(bmi_num >=50,50, bmi_num))

# Showing the boxplot after the adjustment:
# box plot of the distribution of the patients that had a stroke and the healthy ones
stroke_data %>%
  mutate(text = ifelse(stroke==0,"Healthy","Stroke")) %>%
  ggplot(aes(x = text, y = bmi_num)) +
  geom_boxplot(fill=c("#FF0000","#44FFFF"), colour= "#0000FF") +
  scale_y_continuous(name = "Patients BMI Level") +
  scale_x_discrete(name = "Patients Status") +
  ggtitle("PATIENTS") +
  theme(plot.title = element_text(color = "#0066FF", size = 13, 
                                  face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.title.y = element_text(color = "#777777", size = 11, 
                                    face = "bold", hjust = 0.5)) +
  theme(axis.text.x = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) +
  theme(axis.text.y = element_text(color = "#0000FF", size = 7, 
                                   face = "bold", hjust = 0.5)) 

# We do not see any specific trend with the BMI distribution between healthy patients
# and patients that had a stroke.


# row 11: smoking status

# We will review the distribution of the patients based on their smoking status
stroke_data %>%
  group_by(smoking_status) %>%
  summarise(total = n())

# The following is the pie chart of the distribution of the smoking status of the patients
stroke_data %>%
  group_by(smoking_status) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=smoking_status)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100,digits = 1), "%")),
            position = position_stack(vjust = 0.5), color = "#FFFFFF", size=2) +
  # Define the colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF", "#44AAFF", "#3322AA")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "PATIENT SMOKING HABITS") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 7, 
                                  face = "bold", hjust = 0.5))

# Dataset Stoke distribution

# We will review the distribution of stroke in the dataset
stroke_data %>%
  mutate(text = ifelse(stroke==0,"Healthy Patients","Stroke Patients")) %>%
  group_by(text) %>%
  summarise(total = n())

# The following Pie chart show the distribution between Stroke Patients to Patients
# that has not had a stoke.
stroke_data %>%
  mutate(text = ifelse(stroke==0,"Healthy Patients","Stroke Patients")) %>%
  group_by(text) %>%
  summarise(total = n()) %>%
  ggplot(aes(x="", y=total, fill=text)) +
  geom_bar(stat="identity", width=1) +
  # Use the Polar System for generating the Pie
  coord_polar("y", start=0) +
  # Define the Percentages inside the Slices
  geom_text(aes(label = paste0(round(total/nrow(stroke_data) *100), "%")),
            position = position_stack(vjust = 0.5), color = "#FFFFFF") +
  # Define blue colors
  scale_fill_manual(values=c("#55AAE0", "#3365FF")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "STOKE PATIENTS VS. HEALTH PATIENTS ") +
  theme_classic() +
  # Make sure that there are no additional labels generated
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  # Center the title and make it Blue
  theme(plot.title = element_text(color = "#0099FF", size = 6, 
                                  face = "bold", hjust = 0.5))

# For a correlation check it only accepts numerical variables, we are preprocessing all 
# categorical variables to numbers encoding them.

# Also we will scale age, avg_glucose_level and bmi 
# If we keep predictors that are measured at different scales they do not contribute
# equally to model fitting and could create a  bias. So to help  deal with
# this possible problem we will standardized the age, av_glucose_level and bmi to have a
# (μ=0, σ=1) before we start the fitting the models

# mean of age
age_mean <- mean(stroke_data$age)
# sd of age
age_sd <- sd(stroke_data$age)
# mean of glucose
glucose_mean <- mean(stroke_data$avg_glucose_level)
# sd of glucose
glucose_sd <- sd(stroke_data$avg_glucose_level)
# mean of bmi
bmi_mean <- mean(stroke_data$bmi_num)
# sd of bmi
bmi_sd <- sd(stroke_data$bmi_num)

# We need to change our categorical variables to 
stroke_data_num <- stroke_data %>%
  # Gender: Female 0, Male 1
  mutate(gender_num=ifelse(gender=="Female",0,1)) %>%
  # Married: Not_Married 0, Married 1
  mutate(married_num=ifelse(ever_married=="Yes",1,0)) %>%
  # In the following section we will be passing from text to numbers of work type
  # children 0, Govt_job 1, Never_worked2, Private 3, Self-employed 4
  mutate(work_type_num=sapply(work_type, function(x)
    switch(x,"children"= 0,"Govt_job"= 1,"Never_worked"= 2,"Private"= 3,
           "Self-employed"= 4))) %>%
  # Residence_type: Rural 0, Urban 1
  mutate(Residence_type_num=ifelse(Residence_type=="Urban",1,0)) %>%
  # In the following section we will be passing from text to numbers of smoking status
  # formerly smoked 0, never smoked 1, smokes 2, Unknown 3
  mutate(smoking_status_num=sapply(smoking_status, function(x)
    switch(x,"formerly smoked"= 0,"never smoked"= 1,"smokes"= 2,"Unknown"= 3))) %>%
  # Adjust/Fit the values of age
  mutate(age_fit=((age - age_mean)/age_sd)) %>%
  # Adjust/Fit the values of glucose
  mutate(glucose_fit=((avg_glucose_level - glucose_mean)/glucose_sd)) %>%
  # Adjust/Fit the values of bmi
  mutate(bmi_fit=((bmi_num - bmi_mean)/bmi_sd)) %>%
  dplyr::select(gender=gender_num, age=age_fit, hypertension, 
         heart_disease, ever_married=married_num, 
         work_type=work_type_num, Residence_type=Residence_type_num,
         avg_glucose= glucose_fit, bmi = bmi_fit, 
         smoking_status=smoking_status_num, stroke ) 

# We remove the temporary values to keep as clean as possible the environment
rm(age_mean,age_sd,bmi_mean,bmi_sd,glucose_mean,glucose_sd)


# We show the correlation matrix and adjusted some of the colors for having better 
# visibility of value
corrplot::corrplot(cor(stroke_data_num),method = "number",
          bg="#000000", 
          na.label.col = "#FFFFFF", 
          title="Correlation Matrix")


# There seems to be multicollinearity between age and ever_married because we
# have a high correlation of 0.68 in principle age contains more information if a 
# patient is susceptible to a stroke and we might discard ever_married

#With this code we delete the ever_married column from the dataset
stroke_data_num <- subset( stroke_data_num, select = -ever_married )

# Show the heatmap of the stroke dataset
heatmap(cor(stroke_data_num), margins = c(6,6))

##########################
# WORK WITH FULL DATASET #
##########################

# We split the dataset in training and test sets:
# Set seed to 5 to have always the same results and it generate
# and already review that there is a  balance betweeen the Stroke patients of
# the train and test set
set.seed(5, sample.kind = "Rounding") 
#We need to first create a partition of the dataset for training 80% and 20% testing
test_index <- createDataPartition(stroke_data_num$stroke, times = 1, p = 0.2, list = FALSE)
# Generate the sets for the models:
# For convenience and more clarity we will split in "x" the predictors and 
# "y" the actual value of stroke or not stoke
test_x <- stroke_data_num[test_index,1:9]
test_y <- stroke_data_num[test_index,10]
train_x <- stroke_data_num[-test_index,1:9]
train_y <- stroke_data_num[-test_index,10]


# Simplify the models by assigning a categorical response as:
# 1 = "S" the patient had a Stroke, 0 = "H" the patient is Healthy

train_y <- ifelse(train_y==1,"S","H")
test_y  <- ifelse(test_y==1,"S","H")


# Calculate the % of stoke patients in the train set
sum(train_y=="S")/length(train_y)

# How many stroke patients in Training set
sum(train_y=="S")

# Calculate the % of stoke patients in the test set
sum(test_y=="S")/length(test_y)
# How many stroke patients in Test set
sum(test_y=="S")

# Looks a resonable balance between both sets, almost the same percentage of stroke patients

# Running different Models

# We will use the  Generalized Linear Models or "glm" first

# We apply the method "glm" to the training set
generate_glm <- train(train_x, train_y, method = "glm")
# Then with the generated model we create the predictions for the test set
glm_predictions <- predict(generate_glm, test_x)
# We calculate the accuracy of the prediction
mean(glm_predictions == test_y)
# How many patients calculate with stroke
sum(glm_predictions=="S")

# We apply the method "gamLoess" to the training set
generate_loess <- train(train_x, train_y, method = "gamLoess")
# Then with the generated model we create the predictions for the test set
gamloess_predictions <- predict(generate_loess, test_x)
# We calculate the accuracy of the prediction
mean(gamloess_predictions == test_y)
# How many patients calculate with Stroke
sum(gamloess_predictions=="S")

# We apply the method knn to the training set
generate_knn <- train(train_x, train_y, method = "knn",
                      tuneGrid = data.frame(k = seq(1,40,2)))
# Then with the generated model we create the prediction for the test set
knn_predictions <- predict(generate_knn, test_x)
# We calculate the accuracy of the prediction
mean(knn_predictions == test_y)
# How many patients calculate with Stroke
sum(knn_predictions=="S")

#Stopped, we are not really predicting, we are just generating
#all registries as Healthy, the bias is the big distribution of 95%
# of the dataset as Healthy so we need to make a better and more balance dataset
# and as closed as 50% stroke and 50% helathy so we really model based on the 
# predictors.

#############################################
# STOP AND ADJUST THE DATASET TO BALANCE IT #
#############################################

# Reduction of the set to make a better balance between stroke patients and
# health patients

# Pass all Stroke patients to a dataset subset
positive_stroke_patients <- stroke_data_num %>%
  filter(stroke==1)

# We have 249 registries
nrow(positive_stroke_patients)

# Generate a subset of all helthy patients
health_stroke_patients <- stroke_data_num %>%
  filter(stroke==0)

# Set hte seed to 3
set.seed(3, sample.kind = "Rounding") 
# Generate a partition of 6% of the healthy patients around 250 registries
health_index <-createDataPartition(health_stroke_patients$age, times = 1, p = 0.06, list = FALSE)

# Generate this new healthy patients subset 
health_stroke_patients <- health_stroke_patients[health_index,1:10]

# Get the actual records that were generated
nrow(health_stroke_patients)

# Combine the stroke patients with the generated subset
stroke_data_num_adj <- positive_stroke_patients %>%
  union(health_stroke_patients)

# Visualize the new size of the dataset
nrow(stroke_data_num_adj)

# Cleaning the environment of the temporary objects
rm(health_stroke_patients,positive_stroke_patients, health_index)

# Set seed to 5 to have always the same results and it generate
# and already review that there is a  balance betweeen the Stroke patients of
# the train and test set
set.seed(5, sample.kind = "Rounding") 
#We need to first create a partition of the dataset for training 80% and 20% testing
test_index <- createDataPartition(stroke_data_num_adj$stroke, times = 1, p = 0.2, list = FALSE)
# Generate the sets for the models:
# For convenience and more clarity we will split in "x" the predictors and 
# "y" the actual value of stroke or not stoke
test_x <- stroke_data_num_adj[test_index,1:9]
test_y <- stroke_data_num_adj[test_index,10]
train_x <- stroke_data_num_adj[-test_index,1:9]
train_y <- stroke_data_num_adj[-test_index,10]

# Cleaning the environment
rm(test_index)

# Simplify the models by assigning a categorical response as:
# 1 = "S" the patient had a Stroke, 0 = "H" the patient is Healthy

train_y <- ifelse(train_y==1,"S","H")
test_y  <- ifelse(test_y==1,"S","H")

# Calculate the % of stoke patients in the train set
sum(train_y=="S")/length(train_y)

# How many stroke patients in Training set
sum(train_y=="S")

# Calculate the % of stoke patients in the test set
sum(test_y=="S")/length(test_y)
# How many stroke patients in Test set
sum(test_y=="S")

# Looks a good balance between both sets, almost the same percentage of stroke patients

# Running different Models

# We will use the  Generalized Linear Models or "glm" first

# We apply the method "glm" to the training set
generate_glm <- train(train_x, train_y, method = "glm")
# Then with the generated model we create the predictions for the test set
glm_predictions <- predict(generate_glm, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(glm_predictions == test_y)
# How many patients calculate with stroke
stroke_patients_predicted <- sum(glm_predictions=="S")
# I generate a Table with all the Methods and the Accuracy of each
accuracy_results <- data.frame(METHOD = "glm", ACCURACY = model_accuracy, 
                               STROKE_PATIENTS = stroke_patients_predicted)

# We apply the method "qda" to the training set
generate_qda <- train(train_x, train_y, method = "qda")
# Then with the generated model we create the predictions for the test set
qda_predictions <- predict(generate_qda, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(qda_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(qda_predictions=="S")
# Adding the results of the model qda
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "qda", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))


# We apply the method "gamLoess" to the training set
generate_loess <- train(train_x, train_y, method = "gamLoess")
# Then with the generated model we create the predictions for the test set
gamloess_predictions <- predict(generate_loess, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(gamloess_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(gamloess_predictions=="S")
# Adding the results of the model gamLoess
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "gamLoess", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))

# We apply the method knn to the training set
generate_knn <- train(train_x, train_y, method = "knn",
                      tuneGrid = data.frame(k = seq(1,40,2)))
# Then with the generated model we create the prediction for the test set
knn_predictions <- predict(generate_knn, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(knn_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(knn_predictions=="S")
# Adding the results of the model knn
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "knn", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))

# We apply the method rf to the training set
# We apply the tuning from 1 to 13 of odd numbers
tuning <- data.frame(mtry = c(  9, 11))
# We apply the method rf
generate_rf <- train(train_x, train_y, method = "rf",
                     tuneGrid = tuning, importance = TRUE)
# Then with the model we create the prediction for the test set
rf_predictions <- predict(generate_rf, test_x)
# How many patients calculate with Stroke
model_accuracy <- mean(rf_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(rf_predictions=="S")
# Adding the results of the model rf
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "rf", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))

# The predict_kmeans() function defined here takes two arguments - a matrix of
# observations x and a k-means object k - and assigns each row of x to a cluster
# from k.
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

# Perform k-means clustering on the training set with 2 
# centers and assign the output to k. 
k <- kmeans(train_x, centers = 2)

# Calculate the predictions with the different "k"
kmean_predictions <- ifelse(predict_kmeans(test_x,k)==1,"S", "H")
# Calculate the accuracy of the model kmean
model_accuracy <- mean(kmean_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(kmean_predictions=="S")
# Adding the results of the model k-means
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "k-means", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))


# We apply the method "bam" to the training set
generate_bam <- train(train_x, train_y, method = "bam")
# Then with the generated model we create the predictions for the test set
bam_predictions <- predict(generate_bam, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(bam_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(bam_predictions=="S")
# Adding the results of the model bam
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "bam", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))

# We apply the method "cforest" to the training set
# We apply the tuning from 3 to 7 of odd numbers
tuning <- data.frame(mtry = c(3, 5, 7))
# apply the model
generate_cforest <- train(train_x, train_y, method = "cforest",
                          tuneGrid = tuning)
# Then with the generated model we create the predictions for the test set
cforest_predictions <- predict(generate_cforest, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(cforest_predictions == test_y)
# How many patients calculate with Stroke
stroke_patients_predicted <- sum(cforest_predictions=="S")
# Adding the results of the model cforest
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "cforest", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))

# We apply the method "bayesglm" to the training set
generate_bayesglm <- train(train_x, train_y, method = "bayesglm")
# Then with the generated model we create the predictions for the test set
bayesglm_predictions <- predict(generate_bayesglm, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(bayesglm_predictions == test_y)
# How many patients calculate with stroke
stroke_patients_predicted <- sum(bayesglm_predictions=="S")
# Adding the results of the model bayesglm
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "bayesglm", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))


# We apply the method "pda2" to the training set
generate_pda2 <- train(train_x, train_y, method = "pda2")
# Then with the generated model we create the predictions for the test set
pda2_predictions <- predict(generate_pda2, test_x)
# We calculate the accuracy of the prediction
model_accuracy <- mean(pda2_predictions == test_y)
# How many patients calculate with stroke
stroke_patients_predicted <- sum(pda2_predictions=="S")
# Adding the results of the model pda2
accuracy_results <- bind_rows(accuracy_results,
                              data.frame(METHOD = "pda2", ACCURACY = model_accuracy, 
                                         STROKE_PATIENTS = stroke_patients_predicted))

# Show the table with all the values
accuracy_results %>% knitr::kable()

# We will generate an ensemble with athe 10 models used to try to enhance our 
# predictions

# This function returns a value of 1 if the patient under review was predicted with a
# stroke, if not 0 if he was healthy
change_value <- function(item) {
  values <- ifelse(item == "S",1,0)
}

# This value evaluates if 6 or more give a "Stroke" value we return "Stroke" if
# not the patient is Healthy based on the combination of predictions from all the models
return_value <- function(item) {
  valores <- ifelse(item >= 6 , "S","H")
}

# This value evaluates if 3 or more give a "Stroke" value we return "Stroke" if
# not the patient is Healthy based on the combination of predictions from the 5 more
# accurate models models
return_value_opt <- function(item) {
  valores <- ifelse(item >= 3 , "S","H")
}

# Combination of all predictions to try to get a better prediction in the ensemble
ensemble_res <- return_value(  change_value(glm_predictions) + 
                               change_value(qda_predictions) +
                               change_value(gamloess_predictions) +
                               change_value(knn_predictions) + 
                               change_value(rf_predictions) +
                               change_value(kmean_predictions) +
                               change_value(bam_predictions) +
                               change_value(cforest_predictions) +
                               change_value(bayesglm_predictions) + 
                               change_value(pda2_predictions))

# General ensemble accuracy
mean(ensemble_res == test_y)
sum(ensemble_res=="S")

# Combination of all predictions to try to get a better prediction in the ensemble
ensemble_res_optimal <- return_value_opt( change_value(glm_predictions) + 
                                            change_value(gamloess_predictions) +
                                            change_value(knn_predictions) + 
                                            change_value(rf_predictions) +
                                            change_value(bayesglm_predictions))
# Optimal ensemble accuracy
mean(ensemble_res_optimal == test_y)
sum(ensemble_res_optimal=="S")

# Cleaning of Variables
rm(k,generate_bam,generate_bayesglm,generate_cforest,generate_glm,generate_knn)
rm(generate_loess, generate_pda2, generate_qda, generate_rf,tuning)
rm(bam_predictions,bayesglm_predictions,cforest_predictions,gamloess_predictions)
rm(glm_predictions,knn_predictions,qda_predictions,rf_predictions,pda2_predictions)

# Summary 
# The best model and prediction was the Random Forest model, I believe that we 
# might be missing additional information or predictors to be more accurate in the
# prediction of a stroke, with the maximum accuracy of 79% based on the 

# In summary we can confirm that as age progresses it tends to increment the probability 
# of A potential stroke as well as high levels of glucose , however it seems that because of the
# relatively small number of patients with maybe limited predictors we were able to
# predict 79% of the cases, however maybe cholesterol or vascular risk factor and 
# other metabolic factors as well as depression and anxiety based on studies can also
# help in future studies to predict more accurate the potential strokes of patients.


