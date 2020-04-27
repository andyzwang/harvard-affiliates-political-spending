# importing the data from the faculty, as well as reading in the correct column
# types.

faculty <- read_csv("raw-data/faculty.csv", col_types = "ccffcc") %>%
  clean_names()

# reading in the FEC data (using a bulk search on the term "harvard", filtering
# for years from 2017 - 2020, including the correct column types)

fec <- read_csv("raw-data/fec1720.csv",
  col_types = "ccicdccicffcccccccccccccciccccccccTddcccccccccccccccccccccccfcccccccccdcccccccc"
)

# cleaning up the faculty data: I first split the names into a full name, which
# I then split into a last name and a first name. I also did a bunch of error
# checking, since some people did in fact go by both names, or some people used
# their middle names instead of their first names. I then selected out the
# information that wasn't necessary. From there, I simplified to only use the
# first first name given, since I can do that with the FEC data too to make sure
# that no one is left out.

faculty <- faculty %>%
  mutate(
    full_name = map(
      name, ~ unlist(strsplit(., ","))
    ),
    last_name = as.character(map(
      full_name, ~ .[1]
    )),
    full_first_name = map(
      full_name, ~ .[2]
    ),
    first_split = map(
      full_first_name, ~ unlist(
        strsplit(., "\ ")
      )
    ),
    first_name = as.character(
      map(
        first_split, ~ .[2]
      )
    )
  ) %>%
  select(last_name, first_name, title, gender, race, school, department)

# used for error checking on last names to see if anyone has two last names

# mutate(last_split = map(last_name, ~ unlist(strsplit(., "\ ")))) %>%
# mutate(last2 = map(last_split, ~ .[2])) %>%
# filter(!is.na(last2))

# Now cleaning up the FEC data to put the information that we most need to the
# front, as well as adding consistent column names so that merging is easier
# later. Changed names' case from all-caps to title, so that it matches our
# Harvard dataset.

fec <- fec %>%
  mutate(
    first_split = map(
      contributor_first_name, ~ unlist(strsplit(., "\ "))
    ),
    first_name = str_to_title(map(
      first_split, ~ .[1]
    )),
    last_name = str_to_title(contributor_last_name)
  ) %>%
  select(last_name, first_name, everything())

# importing FEC committees data, cleaning up names and changing up column names
# to be consistent with other data set for easy join later.

committees <- read_delim("raw-data/committees1920.txt",
  "|",
  escape_double = FALSE, col_names = TRUE,
  trim_ws = TRUE
) %>%
  clean_names() %>%
  mutate(committee_id = cmte_id)

# created the master of all tables with this data, merging on first and last.
# Also using coalesce function to replace all NA amounts in contributions to 0
# (to make for accurate averages later).

faculty_fec <- left_join(faculty, fec,
  by = c("first_name", "last_name")
) %>%
  mutate(contribution_receipt_amount = coalesce(contribution_receipt_amount, 0))
