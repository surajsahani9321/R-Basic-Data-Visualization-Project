# Load necessary libraries
library(dplyr)
library(tidyr)  # Load tidyr for the spread function
library(ggplot2)

# Read the CSV file
df <- read.csv("C:/GitHub/Python Projects/Master-Data-Science-In-Python/Master Data Science In Python/Project/Bank_Personal_Loan_Modelling.csv")

# Data Preprocessing
# Remove negative values in the 'Experience' column
df$Experience <- abs(df$Experience)

# Explore the dataset
summary(df)

# Data Analysis
# Let's analyze the distribution of the 'Personal Loan' column
loan_distribution <- table(df$Personal.Loan)
print("Distribution of Personal Loan:")
print(loan_distribution)

# Analyze the relationship between 'Education' and 'Personal Loan'
education_loan <- df %>%
  group_by(Education, Personal.Loan) %>%
  summarize(count = n()) %>%
  spread(key = Personal.Loan, value = count, fill = 0)

print("Relationship between Education and Personal Loan:")
print(education_loan)

# Data Visualization
# Visualize the distribution of 'Age'
ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency")

# Visualize the correlation between 'Income' and 'CCAvg'
ggplot(df, aes(x = Income, y = CCAvg)) +
  geom_point(color = "green") +
  labs(title = "Correlation between Income and CCAvg", x = "Income", y = "CCAvg")

# Visualize the count of 'Personal Loan' by 'Education'
ggplot(df, aes(x = Education, fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by Education Level", x = "Education Level", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

# Visualize the count of 'Personal Loan' by 'Family'
ggplot(df, aes(x = Family, fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by Family Size", x = "Family Size", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))





# Visualize the distribution of 'Experience'
ggplot(df, aes(x = Experience)) +
  geom_histogram(binwidth = 5, fill = "orange", color = "black") +
  labs(title = "Distribution of Experience", x = "Experience", y = "Frequency")

# Visualize the distribution of 'Income' by 'Personal Loan'
ggplot(df, aes(x = Income, fill = factor(Personal.Loan))) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Income Distribution by Personal Loan", x = "Income", y = "Frequency") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  facet_wrap(~Personal.Loan, ncol = 2)

# Visualize the distribution of 'CCAvg' by 'Personal Loan'
ggplot(df, aes(x = CCAvg, fill = factor(Personal.Loan))) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "CCAvg Distribution by Personal Loan", x = "CCAvg", y = "Frequency") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  facet_wrap(~Personal.Loan, ncol = 2)

# Visualize the distribution of 'Mortgage' by 'Personal Loan'
ggplot(df, aes(x = Mortgage, fill = factor(Personal.Loan))) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Mortgage Distribution by Personal Loan", x = "Mortgage", y = "Frequency") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red")) +
  facet_wrap(~Personal.Loan, ncol = 2)

# Visualize the distribution of 'Family' by 'Personal Loan'
ggplot(df, aes(x = factor(Family), fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by Family Size", x = "Family Size", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

# Visualize the count of 'Personal Loan' by 'Securities Account'
ggplot(df, aes(x = factor(Securities.Account), fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by Securities Account", x = "Securities Account", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

# Visualize the count of 'Personal Loan' by 'CD Account'
ggplot(df, aes(x = factor(CD.Account), fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by CD Account", x = "CD Account", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

# Visualize the count of 'Personal Loan' by 'Online'
ggplot(df, aes(x = factor(Online), fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by Online Usage", x = "Online", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))

# Visualize the count of 'Personal Loan' by 'CreditCard'
ggplot(df, aes(x = factor(CreditCard), fill = factor(Personal.Loan))) +
  geom_bar(position = "dodge") +
  labs(title = "Personal Loan Count by CreditCard Usage", x = "CreditCard", y = "Count") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"))
