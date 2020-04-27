# first, filtering by name and transaction number to ensure that we don't double
# count people who are in two departments. Then, summarizing based on receiving
# committee name and arranging based on descending values of spending.

spending_recipients <- faculty_fec %>%
  mutate(name = paste(last_name, ", ", first_name, sep = "")) %>%
  distinct(name, pdf_url, .keep_all = T) %>%
  filter(contribution_receipt_amount != 0) %>%
  group_by(committee_name, committee_id) %>%
  summarize(spending_sum = sum(contribution_receipt_amount)) %>%
  arrange(desc(spending_sum))

# joining with committee table

spending_recipients <- left_join(spending_recipients,
  committees,
  by = "committee_id"
) %>%
  select(-cmte_id, -cmte_nm)

# creating a tidier version for output and display.

spending_recipients_ui <- spending_recipients %>%
  select(
    committee_name,
    spending_sum,
    cmte_st1,
    cmte_city, cmte_st, cmte_pty_affiliation
  )
