GE <- read_csv('../../Data_Course_WILSON/countries.csv')
View(GE)

# Remove unwanted columns
SP1 <- SP %>%
  select(-subject_id, -cohort, -demo_firstgen, -demo_race, -demo_gender) 

#Compare all columns against each other
ggpairs(GE, cardinality_threshold = NULL)














