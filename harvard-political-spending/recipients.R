# first, filtering by name and transaction number to ensure that we don't double
# count people who are in two departments. Then, summarizing based on receiving
# committee name and arranging based on descending values of spending.

spending_recipients <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  distinct(name, pdf_url, .keep_all = T) %>%
  filter(contribution_receipt_amount != 0) %>%
  group_by(committee_name, committee_id) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  arrange(desc(spending_sum)) %>%
  ungroup()


# joining with committee table

spending_recipients <- left_join(spending_recipients,
  committees,
  by = "committee_id"
)

# creating a tidier version for output and display.

spending_recipients_ui <- spending_recipients %>%
  select(
    committee_name,
    spending_sum,
    cmte_st1,
    cmte_city,
    cmte_st,
    cmte_pty_affiliation
  ) %>%
  mutate(
    committee_name = str_to_title(committee_name),
    cmte_city = str_to_title(cmte_city),
    cmte_st1 = str_to_title(cmte_st1),
  ) %>%
  rename(
    "Commitee Name" = committee_name,
    "Sum of Spending" = spending_sum,
    "Address" = cmte_st1,
    "City" = cmte_city,
    "State" = cmte_st,
    "Party" = cmte_pty_affiliation
  )

# making a summary based on party affiliation

parties <- spending_recipients %>%
  rename(party = cmte_pty_affiliation) %>%
  group_by(party) %>%
  summarize(total_spending = sum(spending_sum)) %>%
  filter(!is.na(party)) %>%
  arrange(desc(total_spending))

# joining tables of individual donations with committee information, to be able
# to sort by party. group together by party, name, title, etc, and then
# summarize.

individual_donations_party <- faculty_fec %>%
  filter(contribution_receipt_amount != 0) %>%
  left_join(committees, by = "committee_id") %>%
  mutate(
    name = paste(last_name, ", ", first_name, sep = ""),
    party = cmte_pty_affiliation
  ) %>%
  group_by(name, title, school, department, party) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  ungroup() %>%
  select(name, party, spending_sum, title, school, department) %>%
  arrange(desc(spending_sum)) %>%
  filter(!is.na(party)) %>%
  distinct(name, party, .keep_all = T) %>%
  rename(
    "Name" = name,
    "Party" = party,
    "Sum of Spending" = spending_sum,
    "Title" = title,
    "School" = school,
    "Department" = department
  )

# summarizing spending by states

states <- spending_recipients %>%
  mutate(state = cmte_st) %>%
  group_by(state) %>%
  summarize(total_spending = sum(spending_sum)) %>%
  arrange(desc(total_spending))

# filtering for unique name / donation id (again to discount people in two
# departments), then mutating to create a month variable. From there,
# summarizing per month

spending_over_time <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  distinct(name, pdf_url, .keep_all = T) %>%
  mutate(month = floor_date(contribution_receipt_date, "month")) %>%
  filter(!is.na(contribution_receipt_amount)) %>%
  group_by(month) %>%
  summarize(spending = sum(contribution_receipt_amount)) %>%
  mutate(cycle = case_when(
    month <= "2018-11-01" ~ 2018,
    month > "2018-11-01" ~ 2020,
  )) %>%
  filter(!is.na(cycle))