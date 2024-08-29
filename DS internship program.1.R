library(readxl)
library(tidyverse)
library(dplyr)
# install.packages("reshape2")
# install.packages("patchwork")
library(reshape2)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(patchwork)
data <- read_csv("Computer Science/CoreRequirements2015-2023(Sheet).csv")


#_______________________________________________
#### 1:Bar graph of the proportion of students finishing requirements the summer( 07/11/2024)####

summer_data<- data %>% 
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000~ "Summer",
    as.numeric(`Semester Number at Xavier`)>8 ~ "9+",
    TRUE ~ as.character(data$`Semester Number at Xavier`))) %>% 
  filter(str_detect(`Semester Number at Xavier`,"Summer")) %>% 
  pivot_longer(12:33, names_to = "Credit", values_to = "Taken") %>% 
  filter(Taken =="1") %>%
#Group the data by the Credit column so that subsequent operations can be performed within each group.  
  group_by(Credit) %>%
  summarize(count = n()) %>%  
  ungroup() %>% # 95->97 do the job similar to table() --> counting observations of each requirement
  mutate(count = (after_stat(count))/sum(after_stat(count))*100) %>% 
  mutate(Credit = fct_reorder(Credit, count, .desc = FALSE)) 

####Barplot ####  
summer_plot<- ggplot(summer_data, aes(x = Credit, y = count) )+ 
    geom_bar(stat = "identity", fill= "pink", color="black")+
    geom_text(aes(label= round(count)),
              hjust = -0.5)+
    labs(title = "Requirements were fulfilled during summer", x ="Requirements", y = "Proportion of students")+
    coord_flip()
print(summer_plot)

#_______________________________________________________________________
#### 1.2: looping the plot - With NF cohort filter####

#plot_list<- list() --> Creating a list of plots to put all the plots together at the end
indiv.courses = colnames(data)[12:33]
for (X in indiv.courses) {
  temp <- data %>%
    select(`Semester Number at Xavier`,`Cohort Code`, !!sym(X)) %>%
    filter(!!sym(X) == "1") %>%
    filter(`Cohort Code` =="NF") %>% 
    mutate(`Semester Number at Xavier` = case_when(
      as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
      as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
      TRUE ~ as.character(`Semester Number at Xavier`)
    )) 
  plot <- ggplot(temp, aes(x = `Semester Number at Xavier`)) +
    geom_bar(aes(y = (after_stat(count) / sum(after_stat(count)) * 100)), 
             fill = "blue", color = "black", stat = "count") +
    labs(title=paste("Timing of taking", X),y = "Percentage", x = "Semester Number at Xavier")
print(plot)
# plot_list[[X]]<-plot: assignning all the plots to the existing list
}
# combined_plot <- wrap_plots(plot_list, ncol = 3): wrap up all the plots
# print(combined_plot) -->  print out the combination of plots--> not necessary in this case since there are too many plots--> not efficient for visualization

 #___________________________________________________________________________________________
####Specific timing of enrollment for each requirements with a boxplot####
data %>%
  filter(as.numeric(`Semester Number at Xavier`) < 10) %>% 
  pivot_longer(12:33, names_to = "Credit", values_to = "Taken") %>%
  filter(Taken == "1") %>%
  mutate(`Semester Number at Xavier` = as.numeric(`Semester Number at Xavier`)) %>%
  ggplot(aes(x = reorder(Credit, `Semester Number at Xavier`, FUN = median), y = `Semester Number at Xavier`)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  coord_flip() +
  labs(x = "Credit", y = "Semester Number at Xavier") +
  scale_y_continuous(breaks= seq(1,10, by =2))+
  theme_minimal() 
#__________________________________________________________________________________________________
####Filter by the type of degree( only undergraduate), filter out the non existing anymore major, categorize the majors by their college####

major_filtered_data<- data %>% 
  filter(`Cohort Code`=="NF") %>% 
  filter(Degree %in% c("BS","BSBA","BLA","BSN","BA")) %>% 
  filter(Major %in% c("APPH", "MACS","BIOC","BSFB","BIOL","BIMS","BIOP","CHEM","CSCZ","DSCI","ENPH","ENVS","MATH","PHYS",
                      "ADVE","ARTS","CLHU","CSMT","DIFT","DIME","ECSS","ENGL","FREN","GDST","GRAP","HIST","INST","MVED","MUSC","MUTR","ORGL","PHIL","PPPU","POLI", "PUBL","SCOI","SPAN","THTR","THED","THEO","EXPL","LART",
                      "ACCT", "BAIS", "BUNN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG", "ECON",
                      "NURS", "NURE","MCED", "MONT", "ECED", "SPEC", "ELEM", "TLSC", "SEDU",
                      "CJUS", "EXES", "HSEA", "PSYC", "SOCW", "SPMG", "SPMK" )) %>% 
  pivot_longer(12:33, names_to = "Credit", values_to = "Taken") %>% 
  filter(Taken =="1") %>%
  mutate(College <- case_when(Major %in% c("APPH", "ENPH", "BIOL", "BIOC", "BIMS", "BIOP", "CHEM", "CSCI", "ENGE", "ENVS", "EXES", "MATH", "PHYS", 
                                           "CSCA", "INFO", "NATS", "CHMS", "MUED", "SECD", "APCH", "APBI", "PIPG", "TPHS") ~ "STEM",
                              Major %in% c("ARTS", "CLHU", "CMST", "CJUS", "DIFT", "GDST", "ECON", "ENGL", "FREN", "GDST", 
                                           "GRAP", "HIST", "INST", "LART", "MUSC", "MUTR", "PHIL", "POLI", "PUBL", "SOCI", 
                                           "SPAN", "THTR", "THED", "THEO", "DIME", "ADVE", "PPPU", "RADT", "CLPL", "LAND", 
                                           "SHRM", "MAKE", "PPHM", "MLIE", "ARTF", "GERM", "PERM", "CLAS", "PRIE", "CJU1", 
                                           "CMHC") ~ "Social Sciences",
                              Major %in% c("ACCT", "ACTS", "BANL", "ECOS", "ECSS", "ENTR", "FINC", "HESA", "INBU", "LSFB", "MGMT", "MKTG", 
                                           "ORGL", "SPMG", "SPMK", "BUUN", "MACC", "MBAN", "PMBA", "BAIS", "TLSP", "BSFB", "MBA1", "ORG1")~"Business",
                              Major %in% c("NURS", "NURE", "NUP1", "FREF")~"Nursing",
                              Major %in% c("ECED", "MCED", "MONT", "SPEC", "EXPL", "CPHA", "ELEM", "TLSC", "SEDU")~ "Education",
                              Major %in% c("SOCW", "UDEC", "OCTH", "PSYC", "RDSP", "TLFS", "HES1", "ENTI")~ "other",
                              TRUE ~ Major)) 

#_______________________________________________________________________________
####Language courses####
  

# Filter out only language requirements
language <- data %>%
  filter(language == "1") %>%
  filter(`Cohort Code` == "NF")

# Group the data by Student ID and count how many language classes students took
total_langcourse <- language %>%
  group_by(`Student ID-Random`) %>%
  summarise(total_rows = n(), .groups = 'drop')

# Find students with no language courses
number_students <- unique(data$`Student ID-Random`) %in% unique(language$`Student ID-Random`)
no_langcourse <- data.frame(`Student ID-Random` = unique(data$`Student ID-Random`)[!number_students], total_rows = 0)

# Combine the data
combined_lang <- bind_rows(
  total_langcourse %>% count(total_rows), 
  no_langcourse %>% count(total_rows)) # Add rows with zero language courses

# Plotting
num_lang_course <- ggplot(data = combined_lang, aes(x = factor(total_rows))) +
  geom_bar(aes(y = n), stat = "identity", fill = "pink", color = "black") +
  geom_text(aes(y = n, label = n), vjust = -0.5) +
  scale_x_discrete(breaks = seq(0, max(combined_lang$total_rows, na.rm = TRUE), by = 1)) +
  labs(title = "Total Language Requirement Courses Attempted by Student at Xavier",
       x = "Number of Attempted Language Courses", 
       y = "Count of Students") +
  theme_minimal()

print(num_lang_course)

#_______________________________________________________________________________

####Total earned  credit of language requirement####
earned_language_credit <-data %>% 
  filter(`Cohort Code`=="NF") %>% 
  pivot_longer(12:33, names_to = "Credit", values_to = "Taken") %>% 
  filter(Credit =="language") %>% 
  filter(Taken =="1") %>% 
  group_by(`Student ID-Random`) %>%
  summarize(total_credit=sum(`Credits earned for this course`)) %>% 
  ungroup() %>% 
  mutate(`Student ID-Random` = fct_reorder(`Student ID-Random`, total_credit, .desc = FALSE)) %>%
  ggplot( aes(x = total_credit))+
  geom_bar()+
  scale_x_continuous(breaks=seq(0,21, by = 3))+
  geom_text(stat = 'count', aes(label = after_stat(count)), 
            vjust = -0.5)+
  labs(title="how many credit to finish language requirement", y = "total student count", x="earned credit")
#_______________________________________________________________________________
####Summer Enrollment of NF and NT cohort code####
# Prepare data for NF cohort
data_nf <- data %>%
  filter(`Cohort Code` == "NF") %>%
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  filter(str_detect(`Semester Number at Xavier`, "Summer")) %>%
  pivot_longer(cols = 12:33, names_to = "Credit", values_to = "Taken") %>%
  filter(Taken == "1") %>%
  group_by(Credit) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(proportion = (count / sum(count)) * 100) %>%
  ungroup() %>%
  arrange(proportion)

# Prepare data for NT cohort
data_nt <- data %>%
  filter(`Cohort Code` == "NT") %>%
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  filter(str_detect(`Semester Number at Xavier`, "Summer")) %>%
  pivot_longer(cols = 12:33, names_to = "Credit", values_to = "Taken") %>%
  filter(Taken == "1") %>%
  group_by(Credit) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(proportion = (count / sum(count)) * 100) %>%
  ungroup()

# Extract sorted Credit levels from data_nf
sorted_credits <- data_nf %>%
  arrange(proportion) %>%
  pull(Credit)

# Apply the same order of levels to both datasets
data_nf$Credit <- factor(data_nf$Credit, levels = sorted_credits)
data_nt$Credit <- factor(data_nt$Credit, levels = sorted_credits)

# Create ggplot for Dataset 1
p1 <- ggplot(data_nf, aes(x = Credit, y = proportion, fill = "pink")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Transfer Credit of New Freshmen", x = "Percentage ", y = "Requirements") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Create ggplot for Dataset 2
p2 <- ggplot(data_nt, aes(x = Credit, y = proportion, fill = "lightblue")) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Transfer Credit of New Transfer(NT)", x = "Percentage", y = "Reuirements") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

# Display the plots side by side
gridExtra::grid.arrange(p1, p2, ncol = 2)

#_______________________________________________________________________________

combined_data <- data %>%
  filter(`Cohort Code` %in% c("NF", "NT")) %>%
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  filter(str_detect(`Semester Number at Xavier`, "Summer")) %>%
  pivot_longer(cols = 12:33, names_to = "Credit", values_to = "Taken") %>%
  filter(Taken == "1") %>%
  group_by(Credit, `Cohort Code`) %>%
  summarize(count = n(), .groups = 'drop') %>%
  #mutate(proportion = (count / sum(count)) * 100) %>%
  ungroup() %>%
  mutate(Credit = fct_reorder(Credit, count, .desc = FALSE))

# Create the plot with both cohorts
ggplot(combined_data, aes(x = Credit, y = count, fill = `Cohort Code`)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(count)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Requirements Fulfilled During Summer", x = "Requirements", y = "Proportion of Students") +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(combined_data$count) + 10)) 
#_________________________________________________________Combining retention data
#####Find students who got transferred credit instead of taking courses at Xavier (filter by group of major)####
major_filtered_data<- data %>% 
  mutate(College = case_when(
    Major %in% c("APPH", "MACS", "BIOC", "BSFB", "BIOL", "BIMS", "BIOP", "CHEM", "CSCI", "DSCI","ENPH", "ENVS", "MATH", "PHYS") ~ "Stem",
    Major %in% c("ADVE", "ARTS", "CLHU", "CMST", "DIFT", "DIME", "ECSS", "ENGL", "FREN", "GDST", "GRAP", "HIST", "INST", "MVED", "MUSC", "MUTR", "ORGL", "PHIL", "PPPU", "POLI", "PUBL", "SOCI", "SPAN", "THTR", "THED", "THEO", "EXPL", "LART") ~ "Humanities/Social Science",
    Major %in% c("ACCT", "BAIS", "BUNN", "ECON", "ENTR", "FINC", "INBU", "MGMT", "MKTG") ~ "WCB Majors",
    Major %in% c("MCED", "MONT", "ECED", "SPEC", "ELEM", "TLSC", "SEDU") ~ "Education",
    Major %in% c("NURS", "NURE") ~ "Nursing",
    Major %in% c("CJUS", "EXES", "HSEA", "PSYC", "SOCW", "SPMG", "SPMK") ~ "Other/CPS",
    TRUE~NA_character_ # excludes default case
  )) 
 # filter(!is.na(College)) 

# List of colleges to iterate over
college_categories <- c("Stem", "Humanities/Social Science", "WCB Majors", "Nursing", "Education", "Other/CPS")

# Define the CORE requirements
CORE_requirements <- c("CORE 100", "PHIL 100", "THEO 111", "English", "language", "Lit and Moral Imagination",
                       "Scientific Perspectives Lect", "Scientific Perspectives Lab", "Nat Science Elect Lecture Core",
                       "Nat Science Elect Lab Core", "ERS Focus Elective", "Diversity Flag", "Creative Perspectives",
                       "Historical Perspectives", "Mathematical Perspectives", "Quantitative Reason Flag Full",
                       "Philosophy Perspectives", "Theology Perspectives", "Social Science Elective Core",
                       "Humanities Elective", "Oral Communication Flag", "Writing Flag Core")

# Function to check fulfillment
check_fulfillment <- function(Credit) {
  fulfilled <- CORE_requirements %in% Credit
  unfulfilled_list <- CORE_requirements[!fulfilled]
  return(unfulfilled_list)
}
plot_list_transfer<-list()
transfer_requirement_order<-c("CORE 100","THEO 111","Quantitative Reason Flag Full","PHIL 100","Lit and Moral Imagination","Social Science Elective Core",
                              "Diversity Flag","Philosophy Perspectives","Oral Communication Flag","Mathematical Perspectives","Humanities Elective",
                              "Historical Perspectives","Writing Flag Core","ERS Focus Elective","Creative Perspectives","Nat Science Elect Lab Core",
                              "Nat Science Elect Lecture Core","Scientific Perspectives Lab","Scientific Perspectives Lect","English", "language","Theology Perspectives")
# # Iterate over each college category
for (X in college_categories) {
  data2 <- Copy_of_RetainedStudents2015_2021 %>%
    pivot_longer(cols = everything(), values_to = "Student ID-Random") %>%
    drop_na() %>%
    select(`Student ID-Random`) %>%
    mutate(Retention = 1) 
  
  colnames(data2) <- c("Student ID-Random", "Retention_Data")
  
  data1 <- major_filtered_data 
  # Combine datasets
  combined_data2 <- data1 %>%
    left_join(data2, by = "Student ID-Random") %>% 
    filter(Retention_Data=="1")
  # Process data
  sample1 <- combined_data2 %>%
    pivot_longer(cols = 12:33, names_to = "Credit", values_to = "Taken") %>%
    filter(Taken == "1")  %>% 
    group_by(`Student ID-Random`) %>%
    mutate(num_major = n_distinct(Major)) %>%
    mutate(Unique_Major = ifelse(n_distinct(Major) == 1, TRUE,FALSE)) %>%
    ungroup() %>%
    filter(Unique_Major==TRUE) %>%
    filter(College==X)
    
  Total.finished = length(unique(sample1$`Student ID-Random`))
  # Create result data frame
  result <- sample1 %>%
    group_by(`Student ID-Random`) %>%
    reframe(Transfer_Credits = unlist(check_fulfillment(Credit))) %>%
    ungroup()
  result$Transfer_Credits<- factor(result$Transfer_Credits,levels=transfer_requirement_order)
  # Generate plot
  plot_overall <- result %>%
    group_by(Transfer_Credits) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(percentage = count / Total.finished * 100) %>%
    ggplot(aes(x = Transfer_Credits, y = percentage)) +
    geom_bar(stat = "identity", fill="blue") +
    geom_text(aes(label = round(percentage,1), hjust=0)) +
    coord_flip() +
    labs(title = paste("Transferred Requirements in", X), y = "Percentage", x = "CORE requirements") +
    theme_minimal()
plot_list_transfer[[X]]<-plot_overall
}
# 
combined_plot <- wrap_plots(plot_list_transfer, ncol = 3)
print(combined_plot)
#_______________________________________________________________________________


  
#_______________________________________CHANGING MAJOR__________________________
changing_major1 <- combined_data2 %>%
  filter(`Cohort Code` == "NF")
start_end_year <- changing_major1 %>% 
  group_by(`Student ID-Random`) %>%
  summarise(
    Year_Start = min(as.integer(substr(Term, 1, 4))),
    Year_End = max(as.integer(substr(Term, 1, 4)))
  ) %>% 
  ungroup()
 changing_major1 %>% 
  left_join(start_end_year, by= `Student ID-Random`)  

num_major1<-changing_major1 %>% 
  pivot_longer(cols = 12:33, names_to = "Credit", values_to = "Taken") %>% 
  filter(Taken =="1") %>%
  group_by(`Student ID-Random`) %>%
  mutate(num_major = n_distinct(Major)) %>%
  mutate(Unique_Major = ifelse(n_distinct(Major) == 1, TRUE,FALSE)) %>%
  # filter(num_unique_majors > 1) %>%
  ungroup()
num_major2<-num_major1 %>% 
  mutate(num_major=case_when(num_major>=3~ "3+", TRUE~ as.character(num_major)))
#___Barplot of timing taking CORE requirement (Changing Major VS Non Changing)
num_major2 %>%
  filter(Retention == "1") %>% 
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  # group_by(`Semester Number at Xavier`) %>%
  # summarise(count = n(), .groups = 'drop') %>%
  # View()
  # mutate(Percentage = count / sum(count) * 100) %>%
  group_by(`Student ID-Random`) %>%
  mutate(num_major = n_distinct(Major)) %>%
  mutate(Unique_Major = ifelse(n_distinct(Major) == 1, TRUE,FALSE)) %>% 
  ggplot(aes(x = `Semester Number at Xavier`, fill=Unique_Major)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))*100)) +
  facet_wrap(~ Unique_Major) +
  labs(x = "Semester", y = "Percentage ", fill = "Changing Major") +
  theme_minimal()
#_____________Summer Classes (Changing Major VS Non Changing)___________________
num_major2 %>%   #Non Changing Major Summer classes
  filter(Retention == "1") %>% 
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  filter(`Semester Number at Xavier`=="Summer") %>% 
  group_by(`Student ID-Random`) %>%
  mutate(num_major = n_distinct(Major)) %>%
  mutate(Unique_Major = ifelse(n_distinct(Major) == 1, TRUE,FALSE)) %>% 
  ungroup() %>% 
  filter(Unique_Major=TRUE) %>% 
  # group_by(`Semester Number at Xavier`) %>%
  # summarise(count = n(), .groups = 'drop') %>%
  # View()
  # mutate(Percentage = count / sum(count) * 100) %>%
  group_by(Credit) %>%
  summarize(count = n(), .groups = 'drop') %>% 
  mutate(proportion = (count / sum(count)) * 100) %>%
  mutate(Credit = fct_reorder(Credit, proportion, .desc = FALSE)) %>% 
  ggplot(aes(x=Credit, y=proportion))+
  geom_bar(stat = "identity")+
  #facet_wrap(~Unique_Major)+
  coord_flip()

filtered_data <- num_major2 %>% #Changing Major Summer Classes
  filter(Retention == "1") %>% 
  mutate(`Semester Number at Xavier` = case_when(
    as.numeric(`Semester Number at Xavier`) > 20000 ~ "Summer",
    as.numeric(`Semester Number at Xavier`) > 8 ~ "9+",
    TRUE ~ as.character(`Semester Number at Xavier`)
  )) %>%
  filter(`Semester Number at Xavier` == "Summer") %>% 
  group_by(`Student ID-Random`) %>%
  mutate(num_major = n_distinct(Major)) %>%
  mutate(Unique_Major = ifelse(num_major == 1, TRUE, FALSE)) %>% 
  ungroup() %>% 
  filter(Unique_Major == FALSE) %>% 
  group_by(Credit) %>%
  summarise(count = n(), .groups = 'drop') %>% 
  mutate(proportion = (count / sum(count)) * 100) %>% 
  mutate(Credit = fct_reorder(Credit, proportion, .desc = FALSE))
filtered_data %>%
  ggplot(aes(x = Credit, y = proportion)) +
  geom_bar(stat = "identity") +
  coord_flip()
#_________________________________

