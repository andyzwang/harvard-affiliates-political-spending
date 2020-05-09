# data frame for the school as a whole

race_gender_data <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  group_by(name, title, gender, race) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  ungroup() %>%
  arrange(desc(spending_sum)) %>%
  distinct(name, .keep_all = T)

race_gender_data_hist <- race_gender_data %>%
  mutate(demographic = paste(paste(gender, race))) %>%
  filter(spending_sum != 0) %>%
  filter(spending_sum < 10000)

# checking to see if the two variables interact

race_gender_breakdown <- race_gender_data %>%
  group_by(race, gender) %>%
  summarize(mean_donation = mean(spending_sum)) %>%
  arrange(desc(mean_donation))


# creating a dataframe for us to work with as we explore gender, race, school

gender_race_school_data <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  group_by(name, title, school, gender, race) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  ungroup() %>%
  arrange(desc(spending_sum)) %>%
  distinct(name, school, .keep_all = T)

gender_race_school_model <- lm(
  spending_sum ~ gender * race + school,
  gender_race_school_data
)

# data frame for subject-level exploration

gender_race_subject_data <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  filter(school %in% c("FAS", "SEAS")) %>%
  group_by(name, title, school, gender, race, department) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  ungroup() %>%
  arrange(desc(spending_sum)) %>%
  mutate(field = case_when(
    school == "SEAS" ~ "Engineering",
    department %in% c(
      "AAAS",
      "ANTHRO",
      "ECON", 
      "GOV", 
      "HIST", 
      "PSCH", 
      "SOCSTUD", 
      "SOCIO", 
      "WGS"
    ) ~ "Social Sciences",
    department %in% c(
      "ASTRO",
      "CHEM",
      "EPS", 
      "HISTSCI", 
      "HEBIO", 
      "MATH", 
      "MCBIO",
      "OEBIO", 
      "PHY", 
      "STAT",
      "STRBIO", 
      "ESPP"
    ) ~ "Math/Science",
    department %in% c(
      "CELTIC",
      "CLASSICS",
      "COMPLIT",
      "EALC", 
      "ENG", 
      "FOLKMYTH", 
      "GERM", 
      "HISTLIT", 
      "HAA", 
      "LING", 
      "MUSIC", 
      "NELC", 
      "PHIL", 
      "RELI", 
      "ROMANCE", 
      "SLAV", 
      "SAS"
    ) ~ "Humanities",
    department == "ADMIN" ~ "Admin"
  )) %>%
  distinct(name, field, .keep_all = T)

# black box of the model

subject_race_gender_model <- lm(
  spending_sum ~ gender * race + field,
  gender_race_subject_data
)
