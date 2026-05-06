library(tidyverse)
Dementia_risk_data <- read.csv(file.choose())
view(Dementia_risk_data)
sum(is.na(Dementia_risk_data))
ggplot(Dementia_risk_data, aes(HDL, BMI)) +
  geom_point()
boxplot(HDL~Smoking,
        data = Dementia_risk_data,
        main = "Distribution of HDL levels in different smoking groups",
        xlab = "Smoking groups",
        ylab = "HDL levels (mmol/l)",
        col= c("#1a80bb", "#ea801c", "#b8b8b8"))
Dementia_risk_data |>
  group_by(Smoking) |>
  summarise(mean = mean(HDL),
            median = median(HDL),
            sd = sd(HDL))
#fitting the one-way ANOVA model
ModelSmokingHDL <- aov(HDL~as.factor(Smoking), data = Dementia_risk_data)
summary(ModelSmokingHDL)

#checking the assumptions of the ANOVA test
#Normality
hist(Dementia_risk_data$HDL)
qqnorm(ModelSmokingHDL$residuals)
qqline(ModelSmokingHDL$residuals)
shapiro.test(Dementia_risk_data$HDL)
hist(Dementia_risk_data$HDL,
     main = "Distribution of HDL data",
     xlab = "HDL levels (mmol/l)")
#testing variances
library(car)
leveneTest(HDL ~ as.factor(Smoking), Dementia_risk_data)
#Post hoc analysis
TukeyHSD(ModelSmokingHDL)
TukeyHSD(ModelSmokingHDL, conf.level = 0.95)

#Logistic model with all predictor variables (full model)
Logisticfull <- glm(Dementia~HDL + BMI + Age + Irritability + Physical_activity + Sleep + Smoking,
                    data = Dementia_risk_data,
                    family = "binomial")
summary(Logisticfull)
# creating a pie chart showing proportion of people with dementia compared to the total.
library(ggplot2)
CountD <- Dementia_risk_data |>
  group_by(Dementia)
ggplot(CountD, aes(x = "", y = Dementia_proportions , fill = Dementia )) +
  geom_col(color = "black")
  geom_text(aes(label = value),
    position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#1a80bb", "#b8b8b8"))+
  theme_void()

#confusion matrix for q4
library(caret)
conf_matrix <- confusionMatrix(as.factor(Dementia_risk_data$Dementia_diag), 
                               as.factor(Dementia_risk_data$Dementia))
print(conf_matrix)

#Logistic model with all predictor variables (full model)
full_model <- glm(Dementia~HDL + BMI + Age + Irritability + Physical_activity + Sleep + Smoking,
                    data = Dementia_risk_data,
                    family = "binomial")
summary(full_model)
#null model- just the intercept
install.packages("MASS")
library(MASS)
null_model <- glm(Dementia~1, data = Dementia_risk_data, family = "binomial")
stepAIC(null_model, scope=list(lower= null_model, upper = full_model),
        direction = "both")
#using Likelihood ratio test to compare the fit of the proposed model to the full model.
proposed_model <- glm(Dementia~ Age + Smoking + BMI, data = Dementia_risk_data, 
                      family = "binomial")
anova(null_model, proposed_model, test = "LRT")

#creating a pie chart showing proportions again
library(tidyverse)
table(Dementia_risk_data$Dementia)
table(Dementia_risk_data$Dementia)
prop.table(Dementia_frequency)

frequency_Dementia <- c(945, 52)
names (frequency_Dementia) <- c("No Dementia (94.69%)", "Dementia (5.21%)")
pie(frequency_Dementia,
    col = c("#b8b8b8","#1a80bb"))

#testing correlations between all numerical variables (using correlation plot)
continuous <- Dementia_risk_data[, c(2, 4:5)]
library(ggstatsplot)
ggcorrmat(data = continuous,
          cor.vars = ,
          matrix.type = "upper",
          type = "parametric",
          colors = c("#b8b8b8","white","#1a80bb"))
ggplot(Dementia_risk_data, aes(HDL, BMI)) +
  geom_point(colour = "#1a80bb") +
  geom_smooth(method = "lm")
ggplot(Dementia_risk_data, aes(Age, BMI)) +
  geom_point(colour = "#1a80bb") +
  geom_smooth(method = "lm")
ggplot(Dementia_risk_data, aes(Age, HDL)) +
  geom_point(colour = "#1a80bb") +
  geom_smooth(method = "lm")
#more graphs
#boxplot for HDL split by dementia status
boxplot(HDL~Dementia,
        data = Dementia_risk_data,
        main = "HDL distribution WITHOUT Dementia vs WITH Dementia",
        xlab = "Dementia Status",
        ylab = "HDL levels (mmol/l)",
        col= c("#b8b8b8","#1a80bb"))
#boxplot for sleep split by dementia status
boxplot(Sleep~Dementia,
        data = Dementia_risk_data,
        main = "Sleep hours distribution WITHOUT Dementia vs WITH Dementia",
        xlab = "Dementia Status",
        ylab = "Sleep in hours per day",
        col= c("#b8b8b8","#1a80bb"))
#boxplot for ages split by dementia status
boxplot(Age~Dementia,
        data = Dementia_risk_data,
        main = "Age distribution WITHOUT Dementia vs WITH Dementia",
        xlab = "Dementia Status",
        ylab = "Age",
        col= c("#b8b8b8","#1a80bb"))
#boxplot for BMI split by dementia status
boxplot(BMI~Dementia,
        data = Dementia_risk_data,
        main = "BMI distribution WITHOUT Dementia vs WITH Dementia",
        xlab = "Dementia Status",
        ylab = "BMI",
        col= c("#b8b8b8","#1a80bb"))
#creating a graph showing distributions of irritibility and physical activity
df <- data.frame(Q = 1:997, Irritability = Dementia_risk_data$Irritability, 
                 Physical_activity = Dementia_risk_data$Physical_activity)
tidy_df <- df %>% 
  gather(key = "Risk factors- binary", value = "Answer", 2:3)
ggplot(data = tidy_df, aes(x= `Risk factors- binary`, fill = as.factor(Answer)),
       col = c("#b8b8b8","#1a80bb"))+
  geom_bar()

#just to show that LRT and chi-squared produce same output for the model fit test
anova(null_model, proposed_model, test = "Chi")
#proportions bar plot again
Dementia1_frequency <- table(Dementia_risk_data$Dementia== 1)
prop.table(Dementia1_frequency)
Dementia1_frequency
