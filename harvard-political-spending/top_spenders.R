# Creating a table of the top spenders. Creating a new column called name, based
# on the paste function, and then summarizing on name (keeping title, school,
# and department for table readability). Arranging for spending in decreasing
# sums, then looking for distinct names (we are losing a little bit of context
# of having all titles of a professor, but this is an okay cost to weigh against
# dozens of duplicated entries.)


spending_by_individual <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  group_by(name, title, school, department) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  ungroup() %>%
  arrange(desc(spending_sum)) %>%
  distinct(name, .keep_all = T) %>%
  filter(spending_sum != 0)  %>%
  rename('Name' = name,
         'Title' = title,
         'School' = school,
         'Department' = department,
         'Sum of Spending' = spending_sum)