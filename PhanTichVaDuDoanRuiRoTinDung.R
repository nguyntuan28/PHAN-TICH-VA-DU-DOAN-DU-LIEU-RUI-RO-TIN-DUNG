library(readr) 
library(readxl) 
library(dplyr) 
library(ggplot2)

df <- read.csv("C:/Users/Admin/Downloads/BigData/loan/loan.csv")   
dic <- read_excel("C:/Users/Admin/Downloads/BigData/LCDataDictionary.xlsx")  

str(df)   
str(dic)  
dim(df)
str(df)
columns <- colnames(df)
print(columns)
null_percentages <- sapply(df, function(x) mean(is.na(x)) * 100)

for (column in names(null_percentages)) {
  column_percentage <- round(null_percentages[column], 5)
  column_type <- class(df[[column]])
  print(paste(column, ": ", column_percentage, "%, Type:", column_type))
}
first_row <- df[1, ]

first_row_vector <- as.vector(first_row)

print(first_row_vector[1:50])  
for (i in colnames(df)) {
  print(paste(i, ":", length(unique(df[[i]]))))
}
unique(df$loan_status)

status_counts <- table(df$loan_status)

ggplot(as.data.frame(status_counts), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Distribution of Loan Status", x = "Loan Status", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Xoay nhãn trục X
df_selected <- df[, c('loan_amnt', 'int_rate', 'term', 'dti', 'annual_inc', 'delinq_2yrs', 'open_acc', 'grade',
                      'home_ownership', 'collections_12_mths_ex_med', 'revol_bal', 'total_acc', 'loan_status')]

colSums(is.na(df_selected))
df_selected <- na.omit(df_selected)
colSums(is.na(df_selected))
dim(df_selected)
head(df_selected, 10)
library(dplyr)

df_selected <- df_selected %>%
  mutate(loan_status = case_when(
    loan_status %in% c("Fully Paid", "In Grace Period", "Issued") ~ "Normal",
    loan_status %in% c("Late (16-30 days)", "Late (31-120 days)") ~ "Delinquent",
    loan_status %in% c("Charged Off", "Default") ~ "Default",
    grepl("Does not meet the credit policy", loan_status) ~ "Not Compliant",
    grepl("Current", loan_status) ~ "Current",
    TRUE ~ "Unknown"
  ))
unique(df_selected$loan_status)
ggplot(df_selected, aes(x = loan_status, y = loan_amnt)) +
  geom_violin() +
  labs(title = "Loan Amount Distribution by Loan Status",
       x = "Loan Status",
       y = "Loan Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df_selected$grade <- factor(df_selected$grade, levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))

ggplot(df_selected, aes(x = grade, y = loan_amnt)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Loan Amount Distribution by Grade",
       x = "Grade",
       y = "Loan Amount") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df_selected$grade <- factor(df_selected$grade, levels = c('A', 'B', 'C', 'D', 'E', 'F', 'G'))

ggplot(df_selected, aes(x = grade, fill = loan_status)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Loan Status Distribution by Grade",
       x = "Grade",
       y = "Count",
       fill = "Loan Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(df_selected, aes(x = loan_status, y = int_rate)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  labs(title = "Interest Rate by Loan Status",
       x = "Loan Status",
       y = "Interest Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(df_selected, aes(x = loan_status, y = annual_inc)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  scale_y_continuous(trans = 'log10') +
  labs(title = "Annual Income by Loan Status (Log Scale)",
       x = "Loan Status",
       y = "Annual Income (Log Scale)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cross_tab <- table(df_selected$home_ownership, df_selected$loan_status)

cross_tab_percentage <- prop.table(cross_tab, 1)

cross_tab_percentage_df <- as.data.frame(cross_tab_percentage)
colnames(cross_tab_percentage_df) <- c("Home_Ownership", "Loan_Status", "Proportion")

ggplot(cross_tab_percentage_df, aes(x = Home_Ownership, y = Proportion, fill = Loan_Status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Proportion of Loan Status by Home Ownership",
       x = "Home Ownership",
       y = "Proportion") +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.title = element_text(text = "Loan Status")) +
  theme(legend.position = "top") +
  theme(legend.box = "horizontal")
SC_LabelEncoder1 <- function(text) {
  if (text == "A") {
    return(1)
  } else if (text == "B") {
    return(2)
  } else if (text == "C") {
    return(3)
  } else if (text == "D") {
    return(4)
  } else if (text == "E") {
    return(5)
  } else if (text == "F") {
    return(6)
  } else if (text == "G") {
    return(7)
  }
}

SC_LabelEncoder2 <- function(text) {
  if (text == " 36 months") {
    return(1)
  } else {
    return(2)
  }
}

SC_LabelEncoder3 <- function(text) {
  if (text == "RENT") {
    return(1)
  } else if (text == "MORTGAGE") {
    return(2)
  } else if (text == "OWN") {
    return(3)
  } else {
    return(0)
  }
}

df_selected$grade <- sapply(df_selected$grade, SC_LabelEncoder1)
df_selected$term <- sapply(df_selected$term, SC_LabelEncoder2)
df_selected$home_ownership <- sapply(df_selected$home_ownership, SC_LabelEncoder3)
head(df_selected[, c('grade', 'term', 'home_ownership')])
as.data.frame(table(df_selected$loan_status))
loan_status_counts <- as.data.frame(table(df_selected$loan_status))
colnames(loan_status_counts) <- c("Loan Status", "Count")

ggplot(loan_status_counts, aes(x = Count, y = `Loan Status`)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  ggtitle("Distribution of Loan Status") +
  xlab("Count") +
  ylab("Loan Status")
df_selected$loan_status_encoded <- ifelse(df_selected$loan_status == "Normal", 0,
                                          ifelse(df_selected$loan_status %in% c("Default", "Delinquent", "Not Compliant"), 1, 2))

table(df_selected$loan_status_encoded)
loan_status_counts <- as.data.frame(table(df_selected$loan_status_encoded))

colnames(loan_status_counts) <- c('Loan Status', 'Count')

ggplot(loan_status_counts, aes(x = `Loan Status`, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribution of Loan Status", x = "Loan Status", y = "Count") +
  theme_minimal()
unique(df_selected$loan_status)

df_selected <- df_selected[df_selected$loan_status != "Current", ]

unique(df_selected$loan_status)

