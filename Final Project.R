#Link to Dataset https://www.kaggle.com/datasets/footprintnetwork/ecological-footprint ####

#Load Packages ####
library(gapminder)
library(tidyverse)
library(ggimage)
library(gganimate)
library(patchwork)
library(GGally)
library(ggplot2)
library(dplyr)
library(grid)
library(easystats)
library(janitor)
library(skimr)
library(caret)
library(png)
library(jpeg)
library(ggpubr)
library(rnaturalearth)
library(sf)

#Read in dataset ####
pol <- read_csv('countries.csv')
View(pol)


#Let's clean it up ####
pol <- pol %>%
  clean_names() %>% #standardize col names
  mutate(gdp_per_capita = gsub("\\$", "", gdp_per_capita), #remove $ symbols
         gdp_per_capita = gsub("\\,", "", gdp_per_capita), #remove ,'s
         gdp_per_capita = as.numeric(gdp_per_capita)) #change gdp_per_capita from chr to num
View(pol)

#Investigative Goals/Ideas ####
## 1. What variables seem to correlate with ecological footprint ####
###Compare columns to look for single variable correlations ####
#### Total ecological footprint #### 
total_ecological_correlations <- pol %>%
  select(where(is.numeric)) %>%
  summarise(across(-total_ecological_footprint, 
                   ~ cor(.x, total_ecological_footprint, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(total_ecological_correlations)
#predictor                      correlation
#<chr>                                <dbl>
# 1 earths_required                     1.00  
# 2 carbon_footprint                    0.952 
# 3 gdp_per_capita                      0.799 
# 4 hdi                                 0.739 
# 5 cropland_footprint                  0.588 

#### Cropland footprint #### 
cropland_correlations <- pol %>%
  select(where(is.numeric)) %>%
  summarise(across(-cropland_footprint, 
                   ~ cor(.x, cropland_footprint, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(cropland_correlations)
#predictor                      correlation
#<chr>                                <dbl>
# 1 cropland                            0.765 
# 2 total_ecological_footprint          0.588 
# 3 earths_required                     0.588 
# 4 hdi                                 0.567 
# 5 gdp_per_capita                      0.506 

#### Grazing footprint #### 
grazing_correlations <- pol %>%
  select(where(is.numeric)) %>%
  summarise(across(-grazing_footprint, 
                   ~ cor(.x, grazing_footprint, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(grazing_correlations)
#predictor                      correlation
#<chr>                                <dbl>
# 1 grazing_land                        0.696 
# 2 total_ecological_footprint          0.275 
# 3 earths_required                     0.275 
# 4 carbon_footprint                    0.126 

#### Forest footprint #### 
forest_correlations <- pol %>%
  select(where(is.numeric)) %>%
  summarise(across(-forest_footprint, 
                   ~ cor(.x, forest_footprint, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(forest_correlations)
#predictor                      correlation
#<chr>                                <dbl>
# 1 urban_land                          0.457 
# 2 cropland_footprint                  0.438 
# 3 cropland                            0.416 

#### Carbon footprint #### 
carbon_correlations <- pol %>%
  select(where(is.numeric)) %>%
  summarise(across(-carbon_footprint, 
                   ~ cor(.x, carbon_footprint, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(carbon_correlations)
# predictor                      correlation
# <chr>                                <dbl>
# 1 earths_required                     0.952 
# 2 total_ecological_footprint          0.952 
# 3 gdp_per_capita                      0.824 
# 4 hdi                                 0.699 

#### Fish footprint #### 
fish_correlations <- pol %>%
  select(where(is.numeric)) %>%
  summarise(across(-fish_footprint, 
                   ~ cor(.x, fish_footprint, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(fish_correlations)
# predictor                      correlation
# <chr>                                <dbl>
# 1 hdi                                 0.210 
# 2 total_ecological_footprint          0.193 
# 3 earths_required                     0.193 
# 4 fishing_water                       0.167 

### Summary ####
#For total, gdp

## 2. Ecological Sustainability Gaps ####
### Investigate which countries/regions have largest biocapacity deficits
#### bar plot of countries in deficit ####
pol2_country_deficit <- pol %>%
  filter(biocapacity_deficit_or_reserve < 0) %>%
  arrange(biocapacity_deficit_or_reserve) %>%
  mutate(country = factor(country, levels = country),
         half_group = if_else(row_number() <= n() / 2, "Most Severe", "Less Severe")) %>%
  ggplot(aes(x = country, y = biocapacity_deficit_or_reserve)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  scale_y_reverse() +
  facet_wrap(~half_group, scales = "free_y") +
  labs(title = "Countries in Biocapacity Deficit",
       subtitle = "Split by Severity: Most Severe vs Less Severe",
       y = "Biocapacity Deficit",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, hjust = 1),
        strip.text = element_text(face = "bold", size = 10))
pol2_country_deficit


#### bar plot of countries with reserve ####
pol2_country_reserve <- pol %>%
  filter(biocapacity_deficit_or_reserve > 0) %>%
  arrange(biocapacity_deficit_or_reserve) %>%
  mutate(country = factor(country, levels = country)) %>%
  ggplot(aes(x = country, y = biocapacity_deficit_or_reserve)) +
  geom_col(fill = "forestgreen") +
  labs(title = "Countries in Biocapacity Reserve",
       y = "Biocapacity Reserve",
       x = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 70, size = 6, hjust = 1))

pol %>%
  filter(biocapacity_deficit_or_reserve < 0) %>%
  count()
#### bar plot of regions reserve/deficit ####
pol2region_bar <- pol %>%
  group_by(region) %>%
  summarise(total_deficit_or_reserve = sum(biocapacity_deficit_or_reserve, na.rm = TRUE)) %>%
  arrange(total_deficit_or_reserve) %>%
  mutate(region = factor(region, levels = region)) %>%
  ggplot(aes(x = region, y = total_deficit_or_reserve, fill = total_deficit_or_reserve > 0)) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "forestgreen", "FALSE" = "darkred"),
                    labels = c("Deficit", "Reserve")) +
  labs(title = "Total Biocapacity Deficit or Reserve by Region",
       y = "Total Biocapacity",
       x = "Region",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(color = "black", angle = 50, size = 8, hjust = 1)) +
  coord_cartesian(ylim = c(-100, 250))
  
## 3. Human Development vs. Environmental Impact ####
#Explore the relationship between HDI and ecological footprint
#Is there a tradeoff between quality of life and sustainability?
#Scatterplot of HDI vs. Ecological Footprint with color by region.
pol3 <- pol %>%
  ggplot(aes(x = hdi, y = total_ecological_footprint)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(color = "green4") +
  theme_minimal() +
  labs( title = "Human Development Index vs. Total Ecological Footprint",
        x = "Human Development Index",
        y = "Total Ecological Footprint")

## *** 4. GDP and Footprint ####
#Does GDP per capita predict carbon or total footprint?
#Great to explore economic development vs. environmental cost.
#Could split countries into income groups and compare average footprints.
pol4 <- pol %>%
  ggplot(aes(x = gdp_per_capita, y = total_ecological_footprint)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()


## 5. Region-Based Comparisons ####
#Which regions are most/least sustainable overall?
#Group by region → compare average footprint, biocapacity, and deficits.
#Could create bar charts or density plots by region.



## 6. Earths Required vs. Footprint ####
#Are countries with high ecological footprints the same ones needing more Earths?
#Look at the correlation between “Earths Required” and footprint types.
#Might reveal which activities contribute most to overshoot.
Earths_required_vs_footprints <- pol %>%
  select(earths_required, ends_with('_footprint')) %>%
  summarise(across(-earths_required, 
                   ~ cor(.x, earths_required, use = "complete.obs"))) %>%
  pivot_longer(everything(), names_to = "predictor", values_to = "correlation") %>%
  arrange(desc(abs(correlation)))

print(Earths_required_vs_footprints)

#Appears that carbon footprint and cropland contributes most to earths required

## 7. Build a predictive model of total ecological footprint ####
#Build a model to predict total ecological footprint
#Use GDP, HDI, population, etc. as predictors.
#Identify most important variables using regression or random forest.
#Goal: What drives a country’s footprint the most?
library(MASS)
mod1 <- glm(data = pol,
            formula = total_ecological_footprint ~ population_millions * gdp_per_capita * cropland_footprint,
            family = 'gaussian')

mod2 <- glm(data = pol,
            formula = total_ecological_footprint ~ population_millions * hdi * gdp_per_capita * urban_land,
            family = 'gaussian')

mod3 <- glm(data = pol,
            formula = total_ecological_footprint ~ population_millions * hdi * gdp_per_capita * forest_land,
            family = 'gaussian')

mod4 <- glm(data = pol,
            formula = total_ecological_footprint ~ population_millions * hdi * gdp_per_capita * cropland_footprint * urban_land * forest_land,
            family = 'gaussian')

compare_performance(mod1, mod2, mod3, mod4) %>% plot()

# Predictions
cv <- createDataPartition(pol$total_ecological_footprint, p = 0.8, list = F)
pol_train <- pol[cv, ]
pol_test <- pol[-cv,]
dim(pol_train)
dim(pol) 

train_mod4 <- glm(data = pol_train,
                 formula = mod4$formula,
                 family = 'gaussian')

pol_test$pred4 <- predict(train_mod4, pol_test, type = 'response')

pol_test_clean <- pol_test %>% 
  filter(!is.na(population_millions)) %>%
  filter(!is.na(hdi)) %>%
  filter(!is.na(gdp_per_capita)) %>%
  filter(!is.na(cropland_footprint)) %>%
  filter(!is.na(urban_land)) %>%
  filter(!is.na(forest_land))

pol_test_clean$pred4 <- predict(train_mod4, newdata = pol_test_clean, type = "response")

ggplot(pol_test, aes(x = total_ecological_footprint, y = pred4)) +
  geom_point(color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs. Actual Ecological Footprint",
       x = "Actual", y = "Predicted") +
  theme_minimal()


## Calculate performance metrics manually
library(ModelMetrics)

rmse_val4 <- rmse(pol_test_clean$total_ecological_footprint, pol_test_clean$pred4) 
mae_val4 <- mae(pol_test_clean$total_ecological_footprint, pol_test_clean$pred4)
r2_val4 <- cor(pol_test_clean$total_ecological_footprint, pol_test_clean$pred4)^2

cat("Root Mean Square Error (RMSE):", round(rmse_val4, 2), "\n")
cat("Mean Absolute Error (MAE):", round(mae_val4, 2), "\n")
cat("R-squared on Test Set:", round(r2_val4, 3), "\n")

# The predictive model performance drops significantly, meaning the model was likely overfitted to the training data. 

# Try again with mod3, less variables

train_mod3 <- glm(data = pol_train,
                  formula = mod3$formula,
                  family = 'gaussian')

pol_test_clean$pred3 <- predict(train_mod3, newdata = pol_test_clean, type = "response")

ggplot(pol_test_clean, aes(x = total_ecological_footprint, y = pred3)) +
  geom_point(color = "darkblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Predicted vs. Actual Ecological Footprint",
       x = "Actual", y = "Predicted") +
  theme_minimal()

rmse_val3 <- rmse(pol_test_clean$total_ecological_footprint, pol_test_clean$pred3) 
mae_val3 <- mae(pol_test_clean$total_ecological_footprint, pol_test_clean$pred3)
r2_val3 <- cor(pol_test_clean$total_ecological_footprint, pol_test_clean$pred3)^2

cat("Root Mean Square Error (RMSE):", round(rmse_val3, 2), "\n")
cat("Mean Absolute Error (MAE):", round(mae_val3, 2), "\n")
cat("R-squared on Test Set:", round(r2_val3, 3), "\n")
