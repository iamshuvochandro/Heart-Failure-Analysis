library(readxl)
heart_dataset <- read_csv("heart.csv")
View(heart_dataset)


df <- heart_dataset
str(df)
summary(df)

colSums(is.na(df))


# Create problems 

df_bad <- df   
n <- nrow(df_bad)

# Introduce missing values into multiple numeric columns

set.seed(123)

missing_index_age        <- sample(1:n, 60)
missing_index_bp         <- sample(1:n, 60)
missing_index_chol       <- sample(1:n, 60)
missing_index_maxhr      <- sample(1:n, 60)

df_bad$Age[missing_index_age]               <- NA
df_bad$RestingBP[missing_index_bp]          <- NA
df_bad$Cholesterol[missing_index_chol]      <- NA
df_bad$MaxHR[missing_index_maxhr]           <- NA


# Invalid Sex values
df_bad$Sex[sample(1:n, 30)] <- "X"
df_bad$Sex[sample(1:n, 15)] <- "INVALID"

# Invalid Chest Pain Type
df_bad$ChestPainType[sample(1:n, 15)] <- "unknown_type"


# Inconsistent Cholesterol entries
idx_chol_incons <- sample(1:n, 30)
df_bad$Cholesterol[idx_chol_incons] <- paste0(df$Cholesterol[idx_chol_incons], " mg/dl")

# Inconsistent RestingBP entries
idx_bp_incons <- sample(1:n, 30)
df_bad$RestingBP[idx_bp_incons] <- paste0(df$RestingBP[idx_bp_incons], " mmHg")


# Duplicate 60 random rows
duplicate_rows <- df_bad[sample(1:n, 60), ]
df_bad <- rbind(df_bad, duplicate_rows)



# Outliers for Age
df_bad$Age[sample(1:n, 20)] <- sample(c(-10, -5, 150, 180, 200), 20, replace = TRUE)

# Outliers for Cholesterol
df_bad$Cholesterol[sample(1:n, 20)] <- sample(c(700, 800, 900, 1000), 20, replace = TRUE)

# Outliers for RestingBP
df_bad$RestingBP[sample(1:n, 20)] <- sample(c(0, 300, 350), 20, replace = TRUE)



#check missing values
colSums(is.na(df_bad))

#check invalid values
table(df_bad$Sex, useNA = "ifany")
table(df_bad$ChestPainType, useNA = "ifany")

#Inconsistent
head(df_bad$Cholesterol)
head(df_bad$RestingBP)

#duplicate
sum(duplicated(df_bad))

#Outliers
summary(df_bad$Age)
summary(df_bad$RestingBP)
summary(df_bad$Cholesterol)



library(dplyr)

df_clean <- df_bad

# Remove text from Cholesterol and convert to numeric
df_clean$Cholesterol <- gsub("[^0-9.]", "", df_clean$Cholesterol)
df_clean$Cholesterol <- as.numeric(df_clean$Cholesterol)

# Remove text from RestingBP and convert to numeric
df_clean$RestingBP <- gsub("[^0-9.]", "", df_clean$RestingBP)
df_clean$RestingBP <- as.numeric(df_clean$RestingBP)

# Fix Sex
df_clean$Sex[df_clean$Sex %in% c("X", "INVALID", "", NA)] <- NA

# Fix ChestPainType
valid_cpt <- c("ATA","NAP","ASY","TA")
df_clean$ChestPainType[!(df_clean$ChestPainType %in% valid_cpt)] <- NA

# Age must be 0–120
df_clean$Age[df_clean$Age < 0 | df_clean$Age > 120] <- NA

# Remove impossible RestingBP
df_clean$RestingBP[df_clean$RestingBP < 40 | df_clean$RestingBP > 250] <- NA

# Remove impossible Cholesterol
df_clean$Cholesterol[df_clean$Cholesterol < 70 | df_clean$Cholesterol > 600] <- NA

#Replace impossible values
# Age must be 0–120
df_clean$Age[df_clean$Age < 0 | df_clean$Age > 120] <- NA

# Remove impossible RestingBP
df_clean$RestingBP[df_clean$RestingBP < 40 | df_clean$RestingBP > 250] <- NA

# Remove impossible Cholesterol
df_clean$Cholesterol[df_clean$Cholesterol < 70 | df_clean$Cholesterol > 600] <- NA

#Remove duplicates

df_clean <- df_clean[!duplicated(df_clean), ]

#Impute missing values
numeric_cols <- c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak")

for(col in numeric_cols){
  df_clean[[col]][is.na(df_clean[[col]])] <- median(df_clean[[col]], na.rm = TRUE)
}




#Mode for categorical variables
# Mode function
getmode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df_clean$Sex[is.na(df_clean$Sex)] <- getmode(df_clean$Sex)
df_clean$ChestPainType[is.na(df_clean$ChestPainType)] <- getmode(df_clean$ChestPainType)




#Recheck dataset after cleaning
summary(df_clean)

colSums(is.na(df_clean))
sum(duplicated(df_clean))



#exploratory analysis
# 1. Trend Analysis: MaxHR vs Cholesterol

library(ggplot2)
ggplot(df_clean, aes(x = MaxHR, y = Cholesterol)) +
  geom_point(color = "darkred") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Trend: MaxHR vs Cholesterol",
       x = "Maximum Heart Rate",
       y = "Cholesterol")



# 2. Outlier Detection – Cholesterol
ggplot(df_clean, aes(y = Cholesterol)) +
  geom_boxplot(fill = "tomato") +
  labs(title = "Boxplot of Cholesterol Outliers",
       y = "Cholesterol")

# 3. Outlier Detection – Age
ggplot(df_clean, aes(y = Age)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Age Outliers",
       y = "Age")


# 4. Correlation Heatmap

library (GGally)
library(dplyr)

numeric_df <- df_clean %>%
  select(Age, RestingBP, Cholesterol, MaxHR, Oldpeak)

ggcorr(numeric_df, label = TRUE) +
  ggtitle("Correlation Heatmap of Numeric Variables")



# Group-wise Descriptive Statistics by Heart Disease Status

group_stats <- df_clean %>%
  group_by(HeartDisease) %>%
  summarise(
    count = n(),
    mean_age = mean(Age),
    sd_age = sd(Age),
    
    mean_restingBP = mean(RestingBP),
    sd_restingBP = sd(RestingBP),
    
    mean_chol = mean(Cholesterol),
    sd_chol = sd(Cholesterol),
    
    mean_maxhr = mean(MaxHR),
    sd_maxhr = sd(MaxHR),
    
    mean_oldpeak = mean(Oldpeak),
    sd_oldpeak = sd(Oldpeak)
  )

group_stats




#chi square test
table_sex <- table(df_clean$Sex, df_clean$HeartDisease)
chisq.test(table_sex)


#train and split
df_clean$HeartDisease <- as.factor(df_clean$HeartDisease)

set.seed(123)

sample_index <- sample(1:nrow(df_clean), 0.8 * nrow(df_clean))

train <- df_clean[sample_index, ]
test  <- df_clean[-sample_index, ]


#Logistic Regression Model
model <- glm(HeartDisease ~ Age + Sex + ChestPainType + RestingBP +
               Cholesterol + MaxHR + ExerciseAngina + Oldpeak + ST_Slope,
             data = train,
             family = binomial)


summary(model)

pred_prob <- predict(model, test, type = "response")

pred_class <- ifelse(pred_prob > 0.5, "1", "0")
pred_class <- as.factor(pred_class)



#Confusion Matrix (Main Evaluation)
library(caret)

confusionMatrix(pred_class, test$HeartDisease)







