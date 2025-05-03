#ANOVA TEST (BMI ~ Diabetes)
diabeted_multiclass$Diabetes_012 <- as.factor(diabeted_multiclass$Diabetes_012)
anova_model <- aov(BMI ~ Diabetes_012, data = diabeted_multiclass)
summary(anova_model)

#The results showed a statistically significant difference in BMI among the three diabetes groups
#This suggests that mean BMI significantly varies depending on diabetes status. However, to determine which specific groups differ, a post-hoc test such as Tukey's HSD should be performed.

#Assumptions check

#Normality of residuals (Shapiro-Wilk test and Q-Q plot)

#Due to the large sample size, the Shapiro-Wilk test could not be used. Instead, visual inspection of the Q-Q plot and histogram of residuals suggests that the assumption of normality is reasonably satisfied, as the residuals appear approximately normally distributed.
# Q-Q plot
qqnorm(residuals(anova_model))
qqline(residuals(anova_model), col = "red")
#The plot shows noticeable deviations from the red reference line at both tails, indicating some departure from normality, especially in the upper and lower extremes of the residuals.

#Homogeneity of variances (Levene’s Test)

install.packages("car")
library(car)

leveneTest(BMI ~ Diabetes_012, data = diabeted_multiclass)

#p-value is extremely small (way less than 0.05), so we reject the null hypothesis of equal variances.

#Post-Hoc Test (as ANOVA is significant)
TukeyHSD(anova_model)

#all pairwise differences in mean BMI among the three diabetes groups are statistically significant at the 0.05 level

#Since the variances are not equal, the regular ANOVA could be biased
#We use Welch's ANOVA, which does not assume equal variances and is better suited when this assumption is violated

# Welch's ANOVA
oneway.test(BMI ~ Diabetes_012, data = diabeted_multiclass, var.equal = FALSE)

#p-value<0.05, confirms that BMI varies meaningfully across diabetes statuses

# Boxplot to visualize BMI distribution per group
boxplot(BMI ~ Diabetes_012, 
        data = diabeted_multiclass,
        main = "BMI Distribution by Diabetes Status",
        xlab = "Diabetes Status (0 = None, 1 = Prediabetes, 2 = Diabetes)",
        ylab = "BMI",
        col = c("skyblue", "orange", "lightgreen"))
