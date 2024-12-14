library(readxl)
library(tidyverse)
library(dplyr)
# install.packages("reshape2")
# install.packages("patchwork")
library(reshape2)
library(tidyr)
data <- read_csv("Computer Science/CoreRequirements2015-2023(Sheet).csv")
install.packages("patchwork")
library(ggplot2)
library(dplyr)
library(patchwork)
#_________________________________________________________Combining retention data
data2 <- Copy_of_RetainedStudents2015_2021 %>% 
  #unite("Student ID-Random", 1:7, sep = " ", remove = TRUE) %>% 
  pivot_longer(cols = everything(), values_to = "Student ID-Random") %>% 
  drop_na() %>% 
  select(`Student ID-Random`) %>% 
  mutate(Retention =1)
# Rename columns in data2
colnames(data2) <- c("Student ID-Random", "Retention_Data")
# Step 2: Create data1
data1 <- data %>% 
  mutate(Retention = 0)
# Step 3: Combine 2 datasets
combined_data2 <- data1 %>%
  left_join(data2, by = "Student ID-Random") %>%
  mutate(Retention = coalesce(Retention_Data, Retention)) %>%  # Override Retention with values from data2
  select(-Retention_Data)  # Optionally remove the Retention_Data column
#________________________________________________________________
as.logical(combined_data2$Retention)

combined_data2 %>% 
  filter(Retention=="1") %>% 
  filter(`Cohort Code`=="NT") %>% 
  View()
combined_data2 %>% 
  filter(`Cohort Code` == "NF") %>%
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  group_by(`Semester Number at Xavier`, Retention) %>%  # Group by both variables
  summarize(count = n(), .groups = 'drop') %>%  # Count occurrences
  ggplot(aes(x = `Semester Number at Xavier`, y = count, fill = as.factor(Retention))) +  # Fill by Retention
  geom_bar(stat = "identity") +  # Create stacked bar plot
  labs(title = "Timing Requirements Fulfilled  (NF)", 
       x = "Semester Number at Xavier", 
       y = "Number of Students") +
  coord_flip() +  # Flip coordinates for better visibility
  theme_minimal()

#_______________________________________________________________________________
<<<<<<< Updated upstream
sample1<-combined_data2 %>% 
  filter(Retention =="1") %>% 
  # mutate(Year=substr(Cohort,1,4)) %>% 
  # filter(Year != 2015) %>% 
  pivot_longer(12:33, names_to = "Credit", values_to = "Taken") %>% 
  filter(Taken =="1")
  # group_by(`Student ID-Random`, Credit,) %>%
  # summarise(Class_Count = n(), .groups = 'drop')

#Create a list of CORE requirement
CORE_requirements<-c( "CORE 100", "PHIL 100","THEO 111", "English", "language", "Lit and Moral Imagination", "Scientific Perspectives Lect",
=======
sample1 <- combined_data2 %>%
  filter(Retention == "1") %>%
  pivot_longer(cols = 12:33, names_to = "Credit", values_to = "Taken") %>%
  filter(Taken == "1") %>%
  group_by(`Student ID-Random`, Credit) %>%
  summarise(Class_Count = n(), .groups = 'drop') %>% 
  select(`Student ID-Random`,Credit, Class_Count)

Total.finished <- length(unique(sample1$`Student ID-Random`))
  

#Create a list of CORE requirement
CORE_requirements<-c( "CORE 100", "PHIL 100","THEO 111", "English", "language", "Lit and Moral Imagination", "Scientific Perspectives Lect","Scientific Perspectives Lab", "Nat Science Elect Lecture Core",
>>>>>>> Stashed changes
                        "Nat Science Elect Lab Core","ERS Focus Elective", "Diversity Flag","Creative Perspectives","Historical Perspectives",
                   "Mathematical Perspectives","Quantitative Reason Flag Full","Philosophy Perspectives",
                       "Theology Perspectives","Social Science Elective Core", "Humanities Elective","Oral Communication Flag", "Writing Flag Core")
#Create a function to check the list against the taken courses of students --> return the missing requirements
check_fulfillment <- function(Credit) {
  fulfilled <- CORE_requirements %in% Credit
  unfulfilled_list <- CORE_requirements[!fulfilled]
  return(unfulfilled_list)}
# Apply the function and handle empty Missing_Details
result <- sample1 %>%
<<<<<<< Updated upstream
  group_by(`Student ID-Random`) %>%
=======
  group_by(`Student ID-Random`) %>% 
>>>>>>> Stashed changes
  summarise(Missing_Details = unlist(check_fulfillment(Credit))) %>%
  mutate(
    Missing_Details = if_else(Missing_Details=="character(0)", "no transfer credit", Missing_Details)
  ) %>%
   ungroup() 
View(result)

result %>% 
  group_by(Missing_Details) %>%
  summarize(count = n(), .groups='drop') %>%
<<<<<<< Updated upstream
  mutate(Missing_Details = reorder(Missing_Details, count, .desc = FALSE)) %>%
  ggplot(aes(x = Missing_Details, y = count) ) +  # Corrected: use `y = count` instead of `x = Missing_Details`
  geom_bar(stat = "identity",fill="skyblue") +
  geom_text(aes(label=count, hjust = -0.25))+
  coord_flip() +
  labs(x = "Tranferred requirements", y = "Count") +
=======
  mutate(percentage=count/6391*100) %>% #6391 is the total number of students who graduated from Xavier (with 4 years)
  mutate(Missing_Details = reorder(Missing_Details, percentage, .desc = FALSE)) %>%
  ggplot(aes(x = Missing_Details, y = percentage) ) +  # Corrected: use `y = count` instead of `x = Missing_Details`
  geom_bar(stat = "identity",fill="skyblue") +
  geom_text(aes(label=paste(round(percentage,2),"%") , hjust = -0.25))+
  coord_flip() +
  labs(x = "Tranferred requirements", y = "Percentage") +
>>>>>>> Stashed changes
  theme_minimal()


