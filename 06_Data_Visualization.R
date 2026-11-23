
# WEEK 2: DATA VISUALIZATION


# Step 1: Load ggplot2

library(ggplot2)

# Step 2: Load Titanic dataset

titanic <- read.csv("Output/Titanic_Processed.csv")

# Step 3: Check the dataset

head(titanic)
dim(titanic)
str(titanic)

# Step 4: Calculate survival count

survival_data <- as.data.frame(
  table(titanic$Survived)
)

colnames(survival_data) <- c(
  "Survived", "Count"
)

survival_data$Survived <- factor(
  survival_data$Survived,
  levels = c(0, 1),
  labels = c("Not Survived", "Survived")
)

# Step 5: Create survival bar chart

ggplot(survival_data,
       aes(x = Survived, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Titanic Passenger Survival Analysis",
    x = "Passenger Status",
    y = "Number of Passengers"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Step 6: Save chart

ggsave(
  "Output/Survival_Count_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)


# ==========================================
# CHART 2: AGE DISTRIBUTION HISTOGRAM
# ==========================================

ggplot(titanic, aes(x = Age)) +
  
  geom_histogram(
    binwidth = 5,
    fill = "skyblue",
    color = "black"
  ) +
  
  labs(
    title = "Age Distribution of Titanic Passengers",
    x = "Passenger Age",
    y = "Number of Passengers"
  ) +
  
  theme_minimal()

# Save histogram

ggsave(
  "Output/Age_Distribution_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)


# ==========================================
# CHART 3: GENDER-WISE SURVIVAL ANALYSIS
# ==========================================

gender_data <- aggregate(
  Survived ~ Sex,
  data = titanic,
  FUN = mean
)

gender_data$Survival_Percentage <-
  gender_data$Survived * 100

ggplot(gender_data,
       aes(x = Sex, y = Survival_Percentage, fill = Sex)) +
  
  geom_bar(stat = "identity", width = 0.6) +
  
  labs(
    title = "Survival Rate by Gender",
    x = "Gender",
    y = "Survival Percentage (%)"
  ) +
  
  theme_minimal() +
  
  theme(legend.position = "none")

# Save chart

ggsave(
  "Output/Gender_Survival_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display results

print(gender_data)

# =====================================
# CHART 4: AGE VS FARE SCATTER PLOT
# =====================================

ggplot(titanic, aes(x = Age, y = Fare)) +
  geom_point(color = "blue", alpha = 0.5) +
  labs(
    title = "Age vs Fare of Titanic Passengers",
    x = "Passenger Age",
    y = "Ticket Fare"
  ) +
  theme_minimal()

# Save scatter plot
ggsave(
  "Output/Age_vs_Fare_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# =====================================
# CHART 5: PASSENGER CLASS DISTRIBUTION
# =====================================

class_data <- as.data.frame(
  table(titanic$Pclass)
)

colnames(class_data) <- c("Class", "Count")

class_data$Class <- factor(
  class_data$Class,
  labels = c("First Class", "Second Class", "Third Class")
)

ggplot(class_data, aes(x = Class, y = Count, fill = Class)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Passenger Distribution by Class",
    x = "Passenger Class",
    y = "Number of Passengers"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save chart
ggsave(
  "Output/Passenger_Class_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# ==========================================
# CHART 6: SURVIVAL BY PASSENGER CLASS
# ==========================================

# Step 1: Calculate survival rate
class_survival_data <- aggregate(
  Survived ~ Pclass,
  data = titanic,
  FUN = mean
)

# Step 2: Convert survival into percentage
class_survival_data$Survival_Percentage <-
  class_survival_data$Survived * 100

# Step 3: Create class labels
class_survival_data$Class <- factor(
  class_survival_data$Pclass,
  levels = c(1, 2, 3),
  labels = c("First Class", "Second Class", "Third Class")
)

# Step 4: Create bar chart
ggplot(class_survival_data,
       aes(x = Class, y = Survival_Percentage, fill = Class)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Survival Rate by Passenger Class",
    x = "Passenger Class",
    y = "Survival Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Step 5: Save chart
ggsave(
  "Output/Survival_By_Class_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Step 6: Display results
print(class_survival_data)


# =====================================
# CHART 7: SURVIVAL COUNT BY GENDER
# =====================================

gender_count <- as.data.frame(
  table(titanic$Sex, titanic$Survived)
)

colnames(gender_count) <- c(
  "Gender", "Survived", "Count"
)

gender_count$Survived <- factor(
  gender_count$Survived,
  levels = c(0, 1),
  labels = c("Not Survived", "Survived")
)

ggplot(gender_count,
       aes(x = Gender, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Survival Count by Gender",
    x = "Gender",
    y = "Number of Passengers",
    fill = "Passenger Status"
  ) +
  theme_minimal()

# Save chart
ggsave(
  "Output/Survival_Count_By_Gender_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

print(gender_count)


# =====================================
# CHART 8: FARE DISTRIBUTION HISTOGRAM
# =====================================

ggplot(titanic, aes(x = Fare)) +
  geom_histogram(
    binwidth = 10,
    fill = "skyblue",
    color = "black"
  ) +
  labs(
    title = "Distribution of Titanic Passenger Fares",
    x = "Ticket Fare",
    y = "Number of Passengers"
  ) +
  theme_minimal()

# Save chart
ggsave(
  "Output/Fare_Distribution_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display summary
summary(titanic$Fare)

# =====================================
# CHART 9: AGE DISTRIBUTION BY GENDER
# =====================================

ggplot(titanic, aes(x = Sex, y = Age, fill = Sex)) +
  geom_boxplot() +
  labs(
    title = "Passenger Age Distribution by Gender",
    x = "Gender",
    y = "Passenger Age"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save chart
ggsave(
  "Output/Age_By_Gender_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display summary
aggregate(Age ~ Sex, data = titanic, summary)

# =====================================
# CHART 10: PASSENGER CLASS DISTRIBUTION
# =====================================

class_data <- as.data.frame(
  table(titanic$Pclass)
)

colnames(class_data) <- c("Class", "Count")

class_data$Class <- factor(
  class_data$Class,
  levels = c(1, 2, 3),
  labels = c("First Class", "Second Class", "Third Class")
)

ggplot(class_data, aes(x = Class, y = Count, fill = Class)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Titanic Passenger Distribution by Class",
    x = "Passenger Class",
    y = "Number of Passengers"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save chart
ggsave(
  "Output/Passenger_Class_Distribution_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display results
print(class_data)

# =====================================
# CHART 11: SURVIVAL COUNT BY CLASS
# =====================================

class_survival_count <- as.data.frame(
  table(titanic$Pclass, titanic$Survived)
)

colnames(class_survival_count) <- c(
  "Class", "Survived", "Count"
)

class_survival_count$Class <- factor(
  class_survival_count$Class,
  levels = c(1, 2, 3),
  labels = c("First Class", "Second Class", "Third Class")
)

class_survival_count$Survived <- factor(
  class_survival_count$Survived,
  levels = c(0, 1),
  labels = c("Not Survived", "Survived")
)

ggplot(class_survival_count,
       aes(x = Class, y = Count, fill = Survived)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Survival Count by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers",
    fill = "Survival Status"
  ) +
  theme_minimal()

# Save chart
ggsave(
  "Output/Survival_Count_By_Class_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display results
print(class_survival_count)


# =====================================
# CHART 12: FARE DISTRIBUTION BY CLASS
# =====================================

ggplot(titanic, aes(x = factor(Pclass), y = Fare,
                    fill = factor(Pclass))) +
  geom_boxplot() +
  labs(
    title = "Fare Distribution by Passenger Class",
    x = "Passenger Class",
    y = "Ticket Fare",
    fill = "Class"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save chart
ggsave(
  "Output/Fare_By_Class_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display summary
aggregate(Fare ~ Pclass, data = titanic, summary)

# =====================================
# CHART 13: SURVIVAL RATE BY AGE GROUP
# =====================================

titanic$Age_Group <- cut(
  titanic$Age,
  breaks = c(0, 12, 18, 35, 60, Inf),
  labels = c(
    "Children",
    "Teenagers",
    "Young Adults",
    "Adults",
    "Seniors"
  ),
  include.lowest = TRUE
)

age_survival <- aggregate(
  Survived ~ Age_Group,
  data = titanic,
  FUN = mean,
  na.rm = TRUE
)

age_survival$Survival_Percentage <-
  age_survival$Survived * 100

ggplot(age_survival,
       aes(x = Age_Group, y = Survival_Percentage,
           fill = Age_Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Survival Rate by Age Group",
    x = "Age Group",
    y = "Survival Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save chart
ggsave(
  "Output/Survival_By_Age_Group_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display results
print(age_survival)

# =====================================
# CHART 14: SURVIVAL RATE BY EMBARKATION
# =====================================

embarked_data <- aggregate(
  Survived ~ Embarked,
  data = titanic,
  FUN = mean,
  na.rm = TRUE
)

embarked_data$Survival_Percentage <-
  embarked_data$Survived * 100

ggplot(embarked_data,
       aes(x = Embarked, y = Survival_Percentage,
           fill = Embarked)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Survival Rate by Embarkation Port",
    x = "Embarkation Port",
    y = "Survival Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save chart
ggsave(
  "Output/Survival_By_Embarkation_Week2.png",
  width = 8,
  height = 6,
  dpi = 300
)

# Display results
print(embarked_data)

# =====================================
# CHART 15: CORRELATION HEATMAP
# =====================================

library(corrplot)

# Select numerical variables
correlation_data <- titanic[, c(
  "Survived", "Pclass", "Age",
  "SibSp", "Parch", "Fare"
)]

# Calculate correlation matrix
cor_matrix <- cor(
  correlation_data,
  use = "complete.obs"
)

# Display correlation heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  title = "Titanic Dataset Correlation Heatmap",
  mar = c(0, 0, 2, 0)
)

# Save chart
png(
  "Output/Correlation_Heatmap_Week2.png",
  width = 1800,
  height = 1400,
  res = 200
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45
)

dev.off()

# Display correlation matrix
print(round(cor_matrix, 3))

