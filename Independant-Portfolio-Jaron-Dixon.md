Individual Portfolio
================
Jaron Dixon
2025-11-13

- [ABSTRACT](#abstract)
- [BACKGROUND](#background)
- [STUDY QUESTION and HYPOTHESIS](#study-question-and-hypothesis)
  - [Questions](#questions)
  - [Hypothesis](#hypothesis)
  - [Prediction](#prediction)
- [Methods](#methods)
- [Analyses](#analyses)
  - [Figures](#figures)
  - [Statistical Analysis](#statistical-analysis)
- [DISCUSSION](#discussion)
  - [Interpretation 1](#interpretation-1)
- [CONCLUSION](#conclusion)
- [REFERENCES](#references)

# ABSTRACT

# BACKGROUND

Every year about 805,000 Americans have a heart attack. (Tsao et al.,
2023) A heart attack can pose pain from a mild discomfort to severe
crushing pain. (Kulkarni, 2025) This health risk has become like a
ticking bomb being able to strike anyone. With this disease that has the
ability to strike a variety of different individuals, research has begun
to try and predict risk factors or conditions that can cause an
individual to be more susceptible to having a heart attack. Long-term or
chronic stress can lead to higher levels of inflammation in the body
which leads to more plague buildup in the arteries. (Katella, 2024)
Additionally, stress also causes hormones such as adrenaline. (Katella,
2024) Adrenaline increases mental alertness and the heart beats faster
and raises blood pressure. (Katella, 2024) Prolonged stress leads to
heart damage.

With this in mind we turn to what might cause an individual to have
prolonged stress. Specifically, undergraduate students often experience
stress related to adapting to higher education and managing coursework.
(Pérez-Jorge, 2025) Postgraduate students also exhibit stress about
advanced research, thesis completion, and balancing academic work and
professional responsibilities. (Pérez-Jorge, 2025) When it comes to
pursuing higher education there are a lot of stressful responsibilities
for individuals. This could be a cause of having a risk of heart attacks
because of the impact of the stress on the body.

Another factor that might contribute to a higher risk of heart attacks
is gender. Researchers have found that men are twice as likely to have a
heart attack than women. (Harvard Health Publishing, 2016) There is also
some suspicion that hormones in women before menopause cause more
defense for women against heart attacks. (Harvard Health Publishing,
2016)

Knowing this information brings a lot of question of how much factors
like stress, gender, or education could lead to an individual being more
likely to have a heart attack.

# STUDY QUESTION and HYPOTHESIS

## Questions

Do certain factors cause a increased chance in having a heart attack?

## Hypothesis

If the person is male, have high stress level, and have a post-graduate
education level, then we expect to see an increase in amounts of heart
attacks.

## Prediction

We expect to see people who are male, that have a post graduate degree,
and who are under high stress to experience the highest chance of
obtaining a heart attack.

# Methods

# Analyses

## Figures

``` r
# Summarize counts
edu_counts <- heart_data %>%
  group_by(EducationLevel, Outcome) %>%
  summarise(count = n(), .groups = "drop")

# lollipop chart
ggplot(edu_counts, aes(x = EducationLevel, y = count, color = Outcome)) +
  geom_segment(aes(x = EducationLevel, xend = EducationLevel, y = 0, yend = count), linewidth = 1.2) +
  geom_point(size = 6) +
  scale_color_manual(values = c("No Heart Attack" = "skyblue", "Heart Attack" = "tomato")) +
  labs(
    title = "Heart Attack Outcomes by Education Level",
    x = "Education Level",
    y = "Count of Individuals",
    color = "Heart Attack Outcome"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

![](Independant-Portfolio-Jaron-Dixon_files/figure-gfm/Heart%20Attack%20Vs%20Education%20Level%20lollipop%20Plot-1.png)<!-- -->

``` r
ggplot(heart_data, aes(x = Outcome, y = StressLevel, fill = Outcome)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  labs(
    title = "Stress Levels by Heart Attack Outcome",
    x = "Heart Attack Outcome",
    y = "Stress Level (1–9)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none"
  )
```

![](Independant-Portfolio-Jaron-Dixon_files/figure-gfm/Heart%20Attack%20vs%20StressLevel%20Violin%20Plot-1.png)<!-- -->

``` r
ggplot(heart_data, aes(x = StressLevel, y = Outcome, fill = Outcome)) +
  geom_density_ridges(alpha = 0.7, color = "black", scale = 1) +
  scale_fill_manual(values = c("No Heart Attack" = "skyblue", "Heart Attack" = "tomato")) +
  labs(
    title = "Distribution of Stress Levels by Heart Attack Outcome",
    x = "Stress Level (1–9)",
    y = "Heart Attack Outcome",
    fill = "Outcome"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none",
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  )
```

    ## Picking joint bandwidth of 0.674

![](Independant-Portfolio-Jaron-Dixon_files/figure-gfm/Stress%20vs%20Heart%20Attack%20Ridge%20Plot-1.png)<!-- -->

``` r
# Summarize counts
gender_counts <- heart_data %>%
  group_by(Gender, Outcome) %>%
  summarise(count = n(), .groups = "drop")

#Heat map
ggplot(gender_counts, aes(x = Gender, y = Outcome, fill = count)) +
  geom_tile(color = "white", size = 0.8) +  # white borders between tiles
  geom_text(aes(label = count), color = "white", size = 6, fontface = "bold") +
  scale_fill_viridis(option = "C", direction = -1) +  # smooth and modern gradient
  labs(
    title = "Heart Attack Outcomes by Gender",
    x = "Gender",
    y = "Heart Attack Outcome",
    fill = "Count"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12)
  )
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Independant-Portfolio-Jaron-Dixon_files/figure-gfm/Gender%20vs%20Heart%20Attack%20Heat%20Plot-1.png)<!-- -->

``` r
ggplot(heart_data, aes(
  x = EducationLevel,
  y = StressLevel,
  color = Outcome,       
  alpha = Outcome,  
  shape = Gender
)) +
  geom_jitter(width = 0.3, size = 4) +
  scale_alpha_manual(values = c("No Heart Attack" = .9, "Heart Attack" = 1)) +
  scale_shape_manual(values = c("Female" = 1, "Male" = 16)) +  # hollow vs filled
  scale_color_manual(values = c("No Heart Attack" = "lightblue3", "Heart Attack" = "indianred2")) + 
  labs(
    title = "Stress Levels by Education, Gender, and Heart Attack Outcome",
    x = "Education Level",
    y = "Stress Level (1–9)",
    color = "Heart Attack Outcome",
    alpha = "Heart Attack Outcome",
    shape = "Gender"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
```

![](Independant-Portfolio-Jaron-Dixon_files/figure-gfm/Stress%20Levels,%20Education%20Level,%20Gender%20and%20Heart%20Attack%20combined%20plot-1.png)<!-- -->

## Statistical Analysis

``` r
# Load necessary libraries
library(ggplot2)
library(broom)  # for tidy model output

# Ensure Outcome is a factor with the correct reference level
heart_data$Outcome <- factor(heart_data$Outcome, levels = c("No Heart Attack", "Heart Attack"))

# Fit logistic regression model
stress_model <- glm(Outcome ~ StressLevel, data = heart_data, family = binomial)

# View model summary
summary(stress_model)
```

    ## 
    ## Call:
    ## glm(formula = Outcome ~ StressLevel, family = binomial, data = heart_data)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  0.17154    0.13931   1.231    0.218
    ## StressLevel -0.02551    0.02444  -1.044    0.296
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1384.5  on 998  degrees of freedom
    ## Residual deviance: 1383.4  on 997  degrees of freedom
    ## AIC: 1387.4
    ## 
    ## Number of Fisher Scoring iterations: 3

``` r
# Calculate Odds Ratios and 95% Confidence Intervals
odds_ratios <- exp(cbind(OR = coef(stress_model), confint(stress_model)))
```

    ## Waiting for profiling to be done...

``` r
print(odds_ratios)
```

    ##                    OR     2.5 %   97.5 %
    ## (Intercept) 1.1871375 0.9037483 1.560905
    ## StressLevel 0.9748107 0.9291423 1.022604

``` r
# Extract tidy model output for p-value
model_stats <- tidy(stress_model)
p_value <- model_stats$p.value[2]  # p-value for StressLevel
cat("P-value for Stress Level:", round(p_value, 4), "\n")
```

    ## P-value for Stress Level: 0.2965

``` r
# Create data for predicted probabilities
pred_data <- data.frame(
  StressLevel = seq(min(heart_data$StressLevel),
                    max(heart_data$StressLevel), length.out = 100)
)
pred_data$PredictedProb <- predict(stress_model, newdata = pred_data, type = "response")

# Plot predicted probability curve with dotted points
ggplot() +
  # Add raw data points (dotted)
  geom_jitter(data = heart_data, aes(x = StressLevel, y = as.numeric(Outcome) - 1),
              width = 0.15, alpha = 0.5, shape = 21, color = "black", fill = "grey80") +
  # Add predicted probability curve
  geom_line(data = pred_data, aes(x = StressLevel, y = PredictedProb),
            color = "red", size = 1.2) +
  # Rug for distribution
  geom_rug(data = heart_data, aes(x = StressLevel), sides = "b", alpha = 0.3) +
  # Labels
  labs(
    title = paste0(
      "Predicted Probability of Heart Attack by Stress Level\n",
      "p-value = ", round(p_value, 4)
    ),
    x = "Stress Level (1–9)",
    y = "Predicted Probability of Heart Attack"
  ) +
  theme_minimal(base_size = 14)
```

![](Independant-Portfolio-Jaron-Dixon_files/figure-gfm/Stress%20Levels%20vs%20Heart%20Attack%20Logistic%20Regression-1.png)<!-- -->

``` r
# Create a contingency table of counts
edu_table <- table(heart_data$EducationLevel, heart_data$Outcome)
print("Contingency Table:")
```

    ## [1] "Contingency Table:"

``` r
print(edu_table)
```

    ##               
    ##                No Heart Attack Heart Attack
    ##   College                  158          175
    ##   High School              174          165
    ##   Postgraduate             157          170

``` r
# Perform Chi-Square Test
chi_result <- chisq.test(edu_table)
print("Chi-Square Test Results:")
```

    ## [1] "Chi-Square Test Results:"

``` r
print(chi_result)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  edu_table
    ## X-squared = 1.1827, df = 2, p-value = 0.5536

``` r
# Create contingency table
gender_table <- table(heart_data$Gender, heart_data$Outcome)
print("Contingency Table:")
```

    ## [1] "Contingency Table:"

``` r
print(gender_table)
```

    ##         
    ##          No Heart Attack Heart Attack
    ##   Female             236          254
    ##   Male               253          256

``` r
# Run Chi-Square Test
chi_gender <- chisq.test(gender_table)
print("Chi-Square Test Results:")
```

    ## [1] "Chi-Square Test Results:"

``` r
print(chi_gender)
```

    ## 
    ##  Pearson's Chi-squared test with Yates' continuity correction
    ## 
    ## data:  gender_table
    ## X-squared = 0.17987, df = 1, p-value = 0.6715

``` r
# Display expected counts
print("Expected Counts:")
```

    ## [1] "Expected Counts:"

``` r
print(chi_gender$expected)
```

    ##         
    ##          No Heart Attack Heart Attack
    ##   Female        239.8498     250.1502
    ##   Male          249.1502     259.8498

``` r
# If Chi-Square assumptions are violated (expected counts < 5), run Fisher's Exact Test
if(any(chi_gender$expected < 5)) {
  print("Expected counts too low — running Fisher's Exact Test instead:")
  fisher_result <- fisher.test(gender_table)
  print(fisher_result)
}
```

# DISCUSSION

## Interpretation 1

# CONCLUSION

# REFERENCES

1.  Harvard Health Publishing. “Throughout Life, Heart Attacks Are Twice
    as Common in Men than Women - Harvard Health.” Harvard Health,
    Harvard Health, 8 Nov. 2016,
    www.health.harvard.edu/heart-health/throughout-life-heart-attacks-are-twice-as-common-in-men-than-women.
    2.Katella, Kathy. “Yes, Stress Can Hurt Your Heart: 3 Things to
    Know.” Yale Medicine, 12 Feb. 2024,
    www.yalemedicine.org/news/stress-affects-your-heart.
2.  Kulkarni, Anandita. “What Does a Heart Attack Feel Like? 8 Warning
    Signs You Shouldn’t Ignore.” @Bswhealth, 2025,
    www.bswhealth.com/blog/what-does-a-heart-attack-feel-like-8-warning-signs.
3.  Panday, Ankush. “Heart Attack Prediction in United States.”
    Kaggle.com, 2025,
    www.kaggle.com/datasets/ankushpanday2/heart-attack-prediction-in-united-states/data.
    Accessed 6 Nov. 2025.
4.  Pérez-Jorge, David, et al. “Examining the Effects of Academic Stress
    on Student Well-Being in Higher Education.” Humanities and Social
    Sciences Communications, vol. 12, no. 1, 28 Mar. 2025, pp. 1–13,
    www.nature.com/articles/s41599-025-04698-y,
    <https://doi.org/10.1057/s41599-025-04698-y>.
5.  Tsao, Connie W., et al. “Heart Disease and Stroke Statistics—2023
    Update: A Report from the American Heart Association.” Circulation,
    vol. 147, no. 8, 25 Jan. 2023,
    www.ahajournals.org/doi/10.1161/CIR.0000000000001123,
    <https://doi.org/10.1161/cir.0000000000001123>.
