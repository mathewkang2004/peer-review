options(repos = c(CRAN = "https://cloud.r-project.org"))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)

ma.2010 <- read_csv("../data/output/data-2010.csv")
ma.2011 <- read_csv("../data/output/data-2011.csv")
ma.2012 <- read_csv("../data/output/data-2012.csv")
ma.2013 <- read_csv("../data/output/data-2013.csv")
ma.2014 <- read_csv("../data/output/data-2014.csv")
ma.2015 <- read_csv("../data/output/data-2015.csv")

fix_type <- function(df) {
  df$org_parent <- as.character(df$org_parent)
  df
}
ma.2011 <- fix_type(ma.2011)

data.full <- bind_rows(ma.2010, ma.2011, ma.2012, ma.2013, ma.2014, ma.2015)  %>%
  mutate(market_share = avg_enrollment / avg_enrolled)

## Summarize the data

# Question1. 
# Provide a table of summary statistics showing the mean star rating, mean number of enrollments, and mean market share for plans by year. The variables (star rating, enrollments, market share) and total plans number

summary_table <- data.full %>%
  group_by(year) %>%
  summarise(
    'Average Star Rating' = round(mean(Star_Rating, na.rm = TRUE), 2),
    'Average Enrollments' = round(mean(avg_enrollment, na.rm = TRUE), 2),
    'Average Market Share' = round(mean(market_share, na.rm = TRUE), 4),
    'Total Number Of Plans' = n()
  )

print(summary_table)

# Question2.
# Repeat part 1 but focusing only on plans without a star rating. Naturally, in this case, you need only present the mean enrollments and market share, not the mean star rating, along with a column showing the count of all such plans in each year.

without_rating_table <- data.full %>%
  filter(is.na(Star_Rating)) %>%
  group_by(year) %>%
  summarise(
    'Average Enrollments' = round(mean(avg_enrollment, na.rm = TRUE), 2),
    'Average Market Share' = round(mean(market_share, na.rm = TRUE), 4),
    'Total Number Of Plans' = n()
  )

print(without_rating_table)

# Question3.
# Provide bar graphs showing the distribution of star ratings in 2010, 2012, and 2015. How has this distribution changed over time?

data.full %>%
  filter(year %in% c(2010, 2012, 2015), !is.na(Star_Rating)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = as.factor(Star_Rating))) +
  geom_bar() +
  facet_wrap(~ year) +
  labs(
    title = "Distribution of Star Ratings over 2010, 2012, and 2015",
    x = "Star Rating",
    y = "Number Of Plans"
  ) +
  theme_minimal()

# In 2010, we observed a significant decline in the number of highly rated plans. By 2012, the difference was not as pronounced, and in 2015, there was a noticeable improvement, with an increase in the availability of high-rated plans.

# Question4.
# Provide a table showing the regression resuls from an ordinary least squares regression of market share on star ratings, again for each year from 2010 through 2015. In this table, the rows should reflect your coefficient estimates and the columns should reflect different estimates for each year. In your regression specifications, please treat star ratings of 2.5 or below as your excluded category, and include indicator variables for star ratings of 3, 3.5, 4, and 4.5 or above.

data.full <- data.full %>%
  mutate(
    star_3   = as.integer(Star_Rating == 3),
    star_35  = as.integer(Star_Rating == 3.5),
    star_4   = as.integer(Star_Rating == 4),
    star_45  = as.integer(Star_Rating >= 4.5)
  )

estimate <- list()

for(yr in 2010:2015){
  ols <- lm(market_share ~ star_3 + star_35 + star_4 + star_45, 
            data = data.full %>% filter(year == yr, !is.na(Star_Rating), !is.na(market_share)))
  estimate[[as.character(yr)]] <- coef(ols)
}

ols_table <- as.data.frame(estimate)
colnames(ols_table) <- c("2010", "2011", "2012", "2013", "2014", "2015")
rownames(ols_table) <- c("Excluded (<= 2.5 stars)", "3 Stars", "3.5 Stars", "4 Stars", ">= 4.5 Stars")

print(round(ols_table, 4))

## Estimate ATEs

# Question5.
# Calculate the running variable underlying the star rating. Provide a table showing the number of plans that are rounded up into a 3-star, 3.5-star, 4-star, 4.5-star, and 5-star rating.

quality_vars <- c(
  "breastcancer_screen",
  "rectalcancer_screen",
  "cv_diab_cholscreen",
  "glaucoma_test",
  "monitoring",
  "flu_vaccine",
  "pn_vaccine",
  "physical_health",
  "mental_health",
  "osteo_test",
  "physical_monitor",
  "primaryaccess",
  "osteo_manage",
  "diab_healthy",
  "bloodpressure",
  "ra_manage",
  "copd_test",
  "bladder",
  "falling",
  "nodelays",
  "doctor_communicate",
  "carequickly",
  "customer_service",
  "overallrating_care",
  "overallrating_plan",
  "complaints_plan",
  "appeals_timely",
  "appeals_review",
  "leave_plan",
  "audit_problems",
  "hold_times",
  "info_accuracy",
  "ttyt_available"
)

library(dplyr)

ma.2010 <- ma.2010 %>%
  rowwise() %>%
  mutate(
    raw_rating = mean(c_across(all_of(quality_vars)), na.rm = TRUE)
  ) %>%
  ungroup()

round_up_counts <- ma.2010 %>%
  filter(!is.na(raw_rating)) %>%
  filter(!is.na(partc_score)) %>%
  mutate(
    round_ups = case_when(
      raw_rating >= 2.75 & partc_score == 3.0  ~ "Rounded up to 3",
      raw_rating >= 3.25 & partc_score == 3.5  ~ "Rounded up to 3.5",
      raw_rating >= 3.75 & partc_score == 4.0  ~ "Rounded up to 4",
      raw_rating >= 4.25 & partc_score == 4.5  ~ "Rounded up to 4.5",
      raw_rating >= 4.75 & partc_score == 5.0  ~ "Rounded up to 5",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(round_ups)) %>%
  count(round_ups) %>%
  rename('Star Rating' = round_ups, 'Corresponding Number Of Plans' = n)
print(round_up_counts)

# Question6.
# Using the RD estimator with a bandwidth of 0.125, provide an estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments. Repeat the exercise to estimate the effects at 3.5 stars, and summarize your results in a table.

ma_25star_candidates <- ma.2010 %>%
  filter(
    !is.na(raw_rating),
    !is.na(partc_score),
    Star_Rating %in% c(2.5, 3)
  )

n_candidates_total <- nrow(ma_25star_candidates)
n_candidates_by_score <- ma_25star_candidates %>% count(partc_score)

ma_25star <- ma_25star_candidates %>%
  filter(
    raw_rating >= 2.5,
    raw_rating <= 3,
    (raw_rating >= 2.75 & Star_Rating == 3) | (raw_rating < 2.75 & Star_Rating == 2.5)
  )

ma.rd1 <- ma_25star %>%
  mutate(market_share = avg_enrollment / avg_enrolled,
         score = raw_rating - 2.75,
         treat = (score>=0),
         window = (score>=-.125 & score<=.125),
         score_treat=score*treat)

library(rdrobust)

star25.1 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd1 %>% filter(window==TRUE)))
est1 <- as.numeric(star25.1$coef[3])

ma_25star_candidates <- ma.2010 %>%
  filter(
    !is.na(raw_rating),
    !is.na(partc_score),
    Star_Rating %in% c(3, 3.5)
  )

n_candidates_total <- nrow(ma_25star_candidates)
n_candidates_by_score <- ma_25star_candidates %>% count(partc_score)

ma_25star <- ma_25star_candidates %>%
  filter(
    raw_rating >= 3,
    raw_rating <= 3.5,
    (raw_rating >= 3.25 & Star_Rating == 3.5) | (raw_rating < 3.25 & Star_Rating == 3)
  )

ma.rd2 <- ma_25star %>%
  mutate(market_share = avg_enrollment / avg_enrolled,
         score = raw_rating - 3.25,
         treat = (score>=0),
         window = (score>=-.125 & score<=.125),
         score_treat=score*treat)

library(rdrobust)

star25.2 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd2 %>% filter(window==TRUE)))
est2 <- as.numeric(star25.2$coef[3])


# our two estimates
est1 <- as.numeric(star25.1$coef[3])
est2 <- as.numeric(star25.2$coef[3])

comparison_table <- data.frame(row.names = c("RD Estimate"), `3 vs 3.5` = est1, `2.5 vs 3` = est2 )

print(comparison_table)

# Question7.
# Repeat your results for bandwidhts of 0.1, 0.12, 0.13, 0.14, and 0.15 (again for 3 and 3.5 stars). Show all of the results in a graph. How sensitive are your findings to the choice of bandwidth?

ma_25star_candidates <- ma.2010 %>%
  filter(
    !is.na(raw_rating),
    !is.na(partc_score),
    Star_Rating %in% c(2.5, 3)
  )

n_candidates_total <- nrow(ma_25star_candidates)
n_candidates_by_score <- ma_25star_candidates %>% count(partc_score)

ma_25star <- ma_25star_candidates %>%
  filter(
    raw_rating >= 2.5,
    raw_rating <= 3,
    (raw_rating >= 2.75 & Star_Rating == 3) | (raw_rating < 2.75 & Star_Rating == 2.5)
  )

ma.rd1 <- ma_25star %>%
  mutate(market_share = avg_enrollment / avg_enrolled,
         score = raw_rating - 2.75,
         treat = (score>=0),
         window1 = (score>=-.1 & score<=.1),
         window2 = (score>=-.12 & score<=.12),
         window3 = (score>=-.13 & score<=.13),
         window4 = (score>=-.14 & score<=.14),
         window5 = (score>=-.15 & score<=.15),
         score_treat=score*treat)

library(rdrobust)

star25.1 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd1 %>% filter(window1==TRUE)))
star25.2 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd1 %>% filter(window2==TRUE)))
star25.3 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd1 %>% filter(window3==TRUE)))
star25.4 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd1 %>% filter(window4==TRUE)))
star25.5 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd1 %>% filter(window5==TRUE)))
est1 <- as.numeric(star25.1$coef[3])
est2 <- as.numeric(star25.2$coef[3])
est3 <- as.numeric(star25.3$coef[3])
est4 <- as.numeric(star25.4$coef[3])
est5 <- as.numeric(star25.5$coef[3])


library(ggplot2)
library(ggplot2)
library(rdrobust)

# Create a dataframe of estimates and bandwidths
rd_results <- data.frame(
  bandwidth = c(0.10, 0.12, 0.13, 0.14, 0.15),
  estimate  = c(est1, est2, est3, est4, est5)
)

# Add confidence intervals
se_list <- c(
  summary(star25.1)$coef[3, 2],
  summary(star25.2)$coef[3, 2],
  summary(star25.3)$coef[3, 2],
  summary(star25.4)$coef[3, 2],
  summary(star25.5)$coef[3, 2]
)

rd_results <- rd_results %>%
  mutate(
    se    = se_list,
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  )

# Plot
ggplot(rd_results, aes(x = bandwidth, y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.003, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = rd_results$bandwidth) +
  labs(
    title    = "RD Estimates at 2.5-Star Threshold",
    subtitle = "Effect of 3-Star Rating on Market Share",
    x        = "Bandwidth",
    y        = "Estimated Treatment Effect"
  ) +
  theme_minimal()
ma_25star_candidates <- ma.2010 %>%
  filter(
    !is.na(raw_rating),
    !is.na(partc_score),
    Star_Rating %in% c(3, 3.5)
  )

n_candidates_total <- nrow(ma_25star_candidates)
n_candidates_by_score <- ma_25star_candidates %>% count(partc_score)

ma_25star <- ma_25star_candidates %>%
  filter(
    raw_rating >= 3,
    raw_rating <= 3.5,
    (raw_rating >= 3.25 & Star_Rating == 3.5) | (raw_rating < 3.25 & Star_Rating == 3)
  )

ma.rd2 <- ma_25star %>%
  mutate(market_share = avg_enrollment / avg_enrolled,
         score = raw_rating - 3.25,
         treat = (score>=0),
         window1 = (score>=-.1 & score<=.1),
         window2 = (score>=-.12 & score<=.12),
         window3 = (score>=-.13 & score<=.13),
         window4 = (score>=-.14 & score<=.14),
         window5 = (score>=-.15 & score<=.15),
         score_treat=score*treat)

star25.1 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd2 %>% filter(window1==TRUE)))
star25.2 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd2 %>% filter(window2==TRUE)))
star25.3 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd2 %>% filter(window3==TRUE)))
star25.4 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd2 %>% filter(window4==TRUE)))
star25.5 <- lm(market_share ~ score + treat + score_treat, data= (ma.rd2 %>% filter(window5==TRUE)))
est1 <- as.numeric(star25.1$coef[3])
est2 <- as.numeric(star25.2$coef[3])
est3 <- as.numeric(star25.3$coef[3])
est4 <- as.numeric(star25.4$coef[3])
est5 <- as.numeric(star25.5$coef[3])


# Create a dataframe of estimates and bandwidths
rd_results <- data.frame(
  bandwidth = c(0.10, 0.12, 0.13, 0.14, 0.15),
  estimate  = c(est1, est2, est3, est4, est5)
)

# Add confidence intervals
se_list <- c(
  summary(star25.1)$coef[3, 2],
  summary(star25.2)$coef[3, 2],
  summary(star25.3)$coef[3, 2],
  summary(star25.4)$coef[3, 2],
  summary(star25.5)$coef[3, 2]
)

rd_results <- rd_results %>%
  mutate(
    se    = se_list,
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se
  )

# Plot
ggplot(rd_results, aes(x = bandwidth, y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  geom_line(color = "steelblue") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.003, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_continuous(breaks = rd_results$bandwidth) +
  labs(
    title    = "RD Estimates at 3-Star Threshold",
    subtitle = "Effect of 3.5-Star Rating on Market Share",
    x        = "Bandwidth",
    y        = "Estimated Treatment Effect"
  ) +
  theme_minimal()  

# We note that the findings are indeed sensitive to the choice of bandwidth. For instance, the treatment effect when comparing 3-star versus 3.5-star plans appears more pronounced at a threshold of 0.1 compared to 0.15. Similarly, we observe a difference in the calculated effect when a bandwidth of 0.12 is chosen instead of 0.14 while comparing 2.5-star plans to 3-star plans.

# Question8.
# Examine (graphically) whether contracts appear to manipulate the running variable. In other words, look at the distribution of the running variable before and after the relevent threshold values. What do you find?

# Assuming 'window1' is the desired condition for filtering
p1 <- ggplot(ma.rd1 %>% filter(window1 == TRUE), 
             aes(x = score, fill = treat)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"),
                    labels = c("2.5 Stars", "3 Stars")) +
  labs(title = "Density of 2.5 vs 3 Stars", x = "Running Variable (Score)",
       y = "Density", fill = "Rating") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),  # Smaller title
        axis.title = element_text(size = 8),   # Smaller axis titles
        legend.title = element_text(size = 8),  # Smaller legend title
        legend.text = element_text(size = 7))   # Smaller legend text

# Display the plot
print(p1)

# Assuming 'window1' is the desired condition for filtering
p2 <- ggplot(ma.rd2 %>% filter(window1 == TRUE), 
             aes(x = score, fill = treat)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c("FALSE" = "coral", "TRUE" = "steelblue"),
                    labels = c("3 Stars", "3.5 Stars")) +
  labs(title = "Density of 3 vs 3.5 Stars", x = "Running Variable (Score)",
       y = "Density", fill = "Rating") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),  # Smaller title
        axis.title = element_text(size = 8),   # Smaller axis titles
        legend.title = element_text(size = 8),  # Smaller legend title
        legend.text = element_text(size = 7))   # Smaller legend text

# Display the plot
print(p2)

# We observe a sharp increase in market share in relation to values on either side of the threshold, especially noticeable when comparing 2.5-star to 3-star plans (as seen in the first plot). Specifically, there is a decline from -0.10 to -0.05 in the upper plot, along with a slight rise from 0.05 to 0.10 in the lower plot.

# Question9.
# Examine whether plans just above the threshold values have different characteristics than contracts just below the threshold values. Use HMO and Part D status as your plan characteristics.

# Assuming 'ma.2010' is your original data frame
ma.2010 <- ma.2010 %>%
  filter(year == 2010) %>%
  filter(!is.na(Star_Rating)) %>%
  mutate(
    plan_uid = paste0(contractid, "-", planid),
    is_hmo = as.integer(grepl("HMO", plan_type, ignore.case = TRUE)),
    has_partd = ifelse(tolower(trimws(partd)) == "yes", 1, 0)
  ) %>%
  distinct(plan_uid, .keep_all = TRUE)


# Display the first 10 rows of specified columns
print(head(ma.2010[, c("plan_type", "is_hmo", "partd", "has_partd")], 10))

# Count values of has_partd
print(table(ma.2010$has_partd, useNA = "ifany"))

# Extract plans with star ratings of 2.5 and 3
rd_3 <- ma.2010 %>% filter(Star_Rating %in% c(2.5, 3))

# Group and summarize for the 2.5 and 3 star ratings
balance_3 <- rd_3 %>%
  group_by(Star_Rating) %>%
  summarise(
    n_plans = n(),
    HMO_rate = mean(is_hmo),
    PartD_rate = mean(has_partd)
  ) %>%
  ungroup()


# Extract plans with star ratings of 3 and 3.5
rd_35 <- ma.2010 %>% filter(Star_Rating %in% c(3, 3.5))

# Group and summarize for the 3 and 3.5 star ratings
balance_35 <- rd_35 %>%
  group_by(Star_Rating) %>%
  summarise(
    n_plans = n(),
    HMO_rate = mean(is_hmo),
    PartD_rate = mean(has_partd)
  ) %>%
  ungroup()


# Function to compute the difference table
diff_table <- function(balance_df, below, above) {
  b <- balance_df %>% filter(Star_Rating == below) %>% slice(1)
  a <- balance_df %>% filter(Star_Rating == above) %>% slice(1)
  
  return(data.frame(
    Cutoff = paste(above, "vs", below),
    `Δ HMO rate` = aHMO_rate,
    `Δ Part D rate` = aPartD_rate,
    n_below = bn_plans
  ))
}

# Calculate differences
diff_3 <- diff_table(balance_3, 2.5, 3.0)
diff_35 <- diff_table(balance_35, 3.0, 3.5)

# Combine results
diff_results <- bind_rows(diff_3, diff_35)
print(diff_results)

# Function to calculate the standardized mean difference
smd <- function(x_above, x_below) {
  m1 <- mean(x_above)
  m0 <- mean(x_below)
  s1 <- sd(x_above)
  s0 <- sd(x_below)
  pooled <- sqrt((s1^2 + s0^2) / 2)
  return((m1 - m0) / pooled)
}

# Check covariate balance for ratings 2.5 and 3
covars <- c("is_hmo", "has_partd")
smd_vals_3 <- sapply(covars, function(c) {
  above <- rd_3 %>% filter(Star_Rating == 3) %>% pull(c)
  below <- rd_3 %>% filter(Star_Rating == 2.5) %>% pull(c)
  smd(above, below)
})


# Plot Love Plot for 2.5 vs 3.0
ggplot(data.frame(smd_vals = smd_vals_3, covars = covars), aes(x = smd_vals, y = covars)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = c(0.1, -0.1), linetype = "dashed") +
  labs(x = "Standardized Mean Difference", title = "Love Plot: Covariate Balance Around 3.0-Star Cutoff") +
  theme_minimal()

# Check covariate balance for ratings 3 and 3.5
smd_vals_35 <- sapply(covars, function(c) {
  above <- rd_35 %>% filter(Star_Rating == 3.5) %>% pull(c)
  below <- rd_35 %>% filter(Star_Rating == 3) %>% pull(c)
  smd(above, below)
})


# Plot Love Plot for 3.0 vs 3.5
ggplot(data.frame(smd_vals = smd_vals_35, covars = covars), aes(x = smd_vals, y = covars)) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "solid") +
  geom_vline(xintercept = c(0.1, -0.1), linetype = "dashed") +
  labs(x = "Standardized Mean Difference", title = "Love Plot: Covariate Balance Around 3.5-Star Cutoff") +
  theme_minimal()

# With a bandwidth of 0.125, we observe that there are significantly more Plan D contracts below the threshold than above, across both categories. Additionally, the number of HMO-approved plans (calculated using the adjusted raw rating, as HMO is defined through Part C) shows considerable variation above and below the threshold. This indicates that there are distinct characteristics among the plans.

# Question10.
# Summarize your findings from 5-9. What is the effect of increasing a star rating on enrollments? Briefly explain your results.

# Summary:
# The impact of an increased star rating is directly linked to a higher number of enrollments, especially evident in the upward and downward slopes around the threshold in the Problem 8 plot comparing 2.5-star plans with 3-star plans. However, these findings are highly sensitive to the selected bandwidth (as discussed in Problems 6 and 7), leading to an asymmetric distribution of Plan D contracts above and below the threshold (Problem 9). Additionally, the 3-star ratings undergo the most significant number of round-ups compared to higher-rated plans, making that area particularly volatile for analysis.