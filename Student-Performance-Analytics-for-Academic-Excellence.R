# Installing necessary packages

install.packages("tidyverse")
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("corrplot")



# load necessary libraries

library(googledrive)
library(googlesheets4)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(scales)
library(corrplot)


data <- read.csv("/Students_Grading_Dataset_Biased.csv", stringsAsFactors = FALSE)



#  View the first few rows and column names
head(data)

# view dimensions of data
dim(data)

# To list all column names
names(data)


# Check data types and structure
str(data)

# Check data types and structure
glimpse(data)

summary(data)

# View Data Summary and calculate basic statistics (mean, median, and standard deviation for numeric columns)
summary(data[, sapply(data, is.numeric)])

# Check total missing values
sum(is.na(data))

# Check for missing values in each column
colSums(is.na(data))

#Check for duplicates
sum(duplicated(data))

# Convert to character, impute, then convert back to factor
data$Parent_Education_Level <- as.character(data$Parent_Education_Level)
data$Parent_Education_Level[is.na(data$Parent_Education_Level)] <- "Unknown"
data$Parent_Education_Level <- as.factor(data$Parent_Education_Level)



# Impute numeric variable: Assignments_Avg using median
median_assignments <- median(data$Assignments_Avg, na.rm = TRUE)
data$Assignments_Avg[is.na(data$Assignments_Avg)] <- median_assignments




# Remove rows with missing values (if any)
data <- na.omit(data)

# Check for missing values after removing missing values
colSums(is.na(data))



# Changing columns names

data <- data %>%
  rename(
    Attendance_Percentage = `Attendance....`,
    Stress_Level = `Stress_Level..1.10.`
  )

# Drop unnecessary or irrelevant columns
data <- data %>% select(-Student_ID, -First_Name, -Last_Name, -Email)

# Create a new column for Performance levels
data <- data %>%
  mutate(Performance_Level = case_when(
    Total_Score >= 80 ~ "High",
    Total_Score >= 65 ~ "Medium",
    TRUE ~ "Low"
  ))


# Create a new column for Grades Numerical
data$Grade_Num <- as.numeric(factor(data$Grade, levels = c("F", "D", "C", "B", "A")))


# Convert categorical variables to factors for better analysis
data$Gender <- as.factor(data$Gender)
data$Department <- as.factor(data$Department)
data$Grade <- as.factor(data$Grade)
data$Parent_Education_Level <- as.factor(data$Parent_Education_Level)
data$Family_Income_Level <- as.factor(data$Family_Income_Level)

# View the Descriptive & Basic Statistics
summary(data)


# target variable (Count the occurrences of each category in the 'Grade' column)
table(data$Grade)


# Count of Students at each Department
table(data$Department)

# Perform Correlation analysis for numeric columns
cor_matrix <- cor(data[, sapply(data, is.numeric)])
print(cor_matrix)

# Boxplot of Total Score by Department
ggplot(data = data, aes(x = Department, y = Total_Score, fill = Department)) +
  geom_boxplot(color = "black") +
  labs(title = "Total Score by Department", x = "Department", y = "Total Score") +
  theme(plot.title = element_text(hjust = 0.5))


# Boxplot of Stress Level by Grade
ggplot(data = data, aes(x = Grade, y = Stress_Level, fill = Grade)) +
  geom_boxplot(color = "black") +
  labs(title = "Stress Level by Grade", x = "Grade", y = "Stress Level") +
  theme(plot.title = element_text(hjust = 0.5))




# Histogram of Midterm Score

ggplot(data = data, aes(x = Midterm_Score)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Midterm Score", x = "Midterm Score", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))


# Histogram of Projects Score
ggplot(data = data, aes(x = Projects_Score)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Projects Score", x = "Projects Score", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))



# Histogram of Assignments Average

ggplot(data = data, aes(x = Assignments_Avg)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Assignments Average", x = "Assignments Average", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))



# Bar Chart: Number of Students by Department

ggplot(data = data, aes(x = Department)) +
  geom_bar(fill = "skyblue") +
  stat_count(aes(label = ..count..), vjust = -0.3, geom = "text", size = 3.5) +
  labs(title = "Number of Students by Department (Bar Chart)", x = "Department", y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


# Gender Distribution (Pie Chart)
#####################

# Prepare gender data
gender_data <- data %>%
  count(Gender) %>%
  mutate(percent = round(n / sum(n) * 100, 0),
         label = paste0(Gender, "\n", n, " (", percent, "%)"))

# Create pie chart
ggplot(gender_data, aes(x = "", y = n, fill = Gender)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  labs(title = "Gender Distribution (Pie Chart)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# Extracurricular Activities Distribution (Pie Chart)
#####################

# Prepare extracurricular activity data
extra_data <- data %>%
  count(Extracurricular_Activities) %>%
  mutate(percent = round(n / sum(n) * 100, 0),
         label = paste0(Extracurricular_Activities, "\n", n, " (", percent, "%)"))

# Define custom colors
custom_colors <- c('skyblue', 'salmon', 'green', 'pink', 'yellow', 'purple', 'orange')

# Create pie chart
ggplot(extra_data, aes(x = "", y = n, fill = Extracurricular_Activities)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", direction = -1) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Extracurricular Activities Distribution (Pie Chart)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# Performance Levels Distribution (Bar Chart)
#####################

# Create a chart for performance level
ggplot(data, aes(x = Performance_Level, fill = Performance_Level)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Performance Levels Distribution (Bar Chart)",
       x = "Performance Level",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# Internet Access at Home Distribution (Pie Chart)
#####################

# Prepare internet access data
internet_data <- data %>%
  count(Internet_Access_at_Home) %>%
  mutate(percent = round(n / sum(n) * 100, 0),
         label = paste0(Internet_Access_at_Home, "\n", n, " (", percent, "%)"))

# Define custom colors
custom_colors <- c('skyblue', 'salmon')

# Create pie chart
ggplot(internet_data, aes(x = "", y = n, fill = Internet_Access_at_Home)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", direction = -1) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Internet Access at Home Distribution (Pie Chart)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))


# Family Income Level Distribution (Pie Chart)
#####################

# Prepare family income level data
income_data <- data %>%
  count(Family_Income_Level) %>%
  mutate(percent = round(n / sum(n) * 100, 0),
         label = paste0(Family_Income_Level, "\n", n, " (", percent, "%)"))

# Define custom colors
custom_colors <- c('skyblue', 'salmon', 'green', 'pink', 'yellow', 'purple', 'orange')

# Create pie chart
ggplot(income_data, aes(x = "", y = n, fill = Family_Income_Level)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", direction = -1) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Family Income Level Distribution (Pie Chart)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))



# Step 1: Categorize study hours
data <- data %>%
  mutate(Study_Hours_Category = case_when(
    Study_Hours_per_Week < 10 ~ "Low",
    Study_Hours_per_Week <= 20 ~ "Medium",
    TRUE ~ "High"
  ))

# Step 2: Compute percentage
study_pct <- data %>%
  count(Department, Study_Hours_Category) %>%
  group_by(Department) %>%
  mutate(Percentage = n / sum(n) * 100)

# Step 3: Polar chart
ggplot(study_pct, aes(x = Department, y = Percentage, fill = Study_Hours_Category)) +
  geom_col() +
  coord_polar() +
  labs(title = "Study Hours Category by Department (Polar View)",
       x = "Department", y = "Percentage") +
  theme_minimal()

# Grade Distribution by Gender (Side-by-Side Bar Chart)
#####################

# Prepare the data for grouped bar chart
grade_gender_data <- data %>%
  count(Gender, Grade)

# Create the grouped bar chart with labels
ggplot(grade_gender_data, aes(x = Grade, y = n, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  labs(title = "Grade Distribution by Gender (Side-by-Side Bar Chart)",
       x = "Grade",
       y = "Count") +
  scale_fill_manual(values = c("salmon", "lightblue")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Extracurricular Activity Distribution by Grade (Stacked Bar Chart 100%)

# Prepare the data
grade_extra_data <- data %>%
  count(Grade, Extracurricular_Activities) %>%
  group_by(Grade) %>%
  mutate(percentage = round(n / sum(n) * 100, 0),
         label = ifelse(n == 0, "", paste0(percentage, "%")))

# Create the 100% stacked bar chart
ggplot(grade_extra_data, aes(x = Grade, y = n, fill = Extracurricular_Activities)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            color = "black", size = 3.5) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("salmon", "skyblue", "green", "pink", "yellow")) +
  labs(title = "Extracurricular Activity Distribution by Grade (Stacked Bar Chart 100%)",
       x = "Grade",
       y = "Percentage") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#Grade Distribution by Internet Access at Home (Stacked Bar Chart)

# Prepare the data with percentages calculated by Grade
grade_internet_data <- data %>%
  count(Grade, Internet_Access_at_Home) %>%
  group_by(Grade) %>%
  mutate(percentage = round(n / sum(n) * 100, 0),
         label = ifelse(n == 0, "", paste0(percentage, "%")))

# Create a normal stacked bar chart (not scaled to 100%)
ggplot(grade_internet_data, aes(x = Grade, y = n, fill = Internet_Access_at_Home)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  scale_fill_manual(values = c("salmon", "lightblue")) +
  labs(title = "Grade Distribution by Internet Access at Home (Stacked Bar Chart)",
       x = "Grade",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Grade Distribution by Family Income Level

# Prepare data with counts
grade_income_counts <- data %>%
  count(Grade, Family_Income_Level)

# Dodged Bar Chart: Grade on x-axis, filled by Income Level
ggplot(grade_income_counts, aes(x = Grade, y = n, fill = Family_Income_Level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = n),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(title = "Number of Students by Grade and Family Income Level",
       x = "Grade",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Grade Distribution by Parent Education Level (Stacked Bar Chart)

# Prepare the data
grade_parent_data <- data %>%
  count(Grade, Parent_Education_Level) %>%
  group_by(Grade) %>%
  mutate(percentage = round(n / sum(n) * 100, 0),
         label = ifelse(n == 0, "", paste0(percentage, "%")))

# Create stacked bar chart with percentage labels (count-based height)
ggplot(grade_parent_data, aes(x = Grade, y = n, fill = Parent_Education_Level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  scale_fill_manual(values = c("skyblue", "salmon", "green", "pink", "yellow", "purple")) +
  labs(title = "Grade Distribution by Parent Education Level (Stacked Bar Chart)",
       x = "Grade",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# Grade Distribution by Family Income Level (Stacked Bar Chart)

# Prepare the data
grade_income_data <- data %>%
  count(Grade, Family_Income_Level) %>%
  group_by(Grade) %>%
  mutate(percentage = round(n / sum(n) * 100, 0),
         label = ifelse(n == 0, "", paste0(percentage, "%")))

# Create the stacked bar chart with percentage labels
ggplot(grade_income_data, aes(x = Grade, y = n, fill = Family_Income_Level)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3.5) +
  scale_fill_manual(values = c("skyblue", "salmon", "green", "pink", "yellow", "purple", "orange")) +
  labs(title = "Grade Distribution by Family Income Level (Stacked Bar Chart)",
       x = "Grade",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Total Score by Department (Box Plot)

# Create horizontal box plot of Total Score by Department
ggplot(data, aes(x = Department, y = Total_Score, fill = Department)) +
  geom_boxplot(color = "black") +
  coord_flip() +
  labs(title = "Total Score by Department (Box Plot)",
       x = "Department",
       y = "Total Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Correlation Matrix Plot

cor_matrix <- cor(select(data, where(is.numeric)), use = "complete.obs")
corrplot(cor_matrix, method = "color", tl.cex = 0.8,addCoef.col = "black", number.cex = 0.7)

# Scatter Plot: Study Hours vs Total Score
ggplot(data = data, aes(x = Study_Hours_per_Week, y = Total_Score, color = Grade)) +
  geom_point() +
  labs(title = "Study Hours vs Total Score (Scatter Plot)", x = "Study Hours/Week", y = "Total Score")

# Scatter Plot of Study Hours vs Total Score
#####################

ggplot(data, aes(x = Study_Hours_per_Week, y = Total_Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkgreen") +
  labs(title = "Study Hours vs Total Score")+
  theme(plot.title = element_text(hjust = 0.5))



# Faceted Scatter Plot with LOESS Fitting Lines by Department
ggplot(data = data, aes(x = Study_Hours_per_Week, y = Total_Score)) +
  geom_point(alpha = 0.3, color = "black") +
  geom_smooth(aes(group = Department), method = "loess", se = FALSE, color = "red", linewidth = 1) +
  facet_wrap(~ Department) +
  labs(title = "Study Hours vs Total Score by Department (with Fitted Lines)",
       x = "Study Hours/Week", y = "Total Score") +
  theme(plot.title = element_text(hjust = 0.5))


# Scatter Plot of Sleep Hours Vs. Total Score
#####################

ggplot(data, aes(x = Sleep_Hours_per_Night, y = Total_Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Sleep Hours vs Total Score (Scatter Plot)") +
  theme(plot.title = element_text(hjust = 0.5))


# Sleep Hours per Night by Grade (Violin Plot)
#####################

# Create violin for grades

ggplot(data, aes(x = Grade, y = Sleep_Hours_per_Night, fill = Grade)) +
  geom_violin(trim = FALSE) +
  labs(title = "Sleep Hours per Night by Grade (Violin Plot)",
       x = "Grade",
       y = "Sleep Hours per Night") +
  theme_minimal()

### End of Code ###