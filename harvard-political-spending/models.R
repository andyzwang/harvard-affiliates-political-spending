

# creating a dataframe for us to work with as we explore gender, race, school

gender_race_school_data <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  group_by(name, title, school, gender, race) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  ungroup() %>%
  arrange(desc(spending_sum)) %>%
  distinct(name, school, .keep_all = T)

# checking to see if race and gender interact

uni_race_gender_breakdown <- gender_race_school_data %>%
  group_by(race, gender) %>%
  summarize(mean_spending = mean(spending_sum)) %>%
  ungroup() %>%
  arrange(desc(mean_spending))

gender_race_school_model <- lm(
  spending_sum ~ gender * race + school,
  gender_race_school_data
) 