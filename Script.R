library(tidyverse)
library(janitor)
library(tidyr)

ga2025 <- read_csv("https://mgaleg.maryland.gov/2025rs/misc/billsmasterlist/BillMasterList.csv") %>% clean_names()

###List bills to track here
tracker <- ga2025 %>% filter(bill_number == "SB0001" | bill_number == "SB0021")

tracker <- replace_na(tracker, list(x_file_bill_number="NULL"))

tracker$hyperlinked_billnum <- paste("<a href='https://mgaleg.maryland.gov/mgawebsite/Legislation/Details/", tracker$bill_number, "?ys=2025RS' target='_blank'>", tracker$bill_number, "</a>", sep = "") 

tracker$hyperlinked_xfile <- paste("<a href='https://mgaleg.maryland.gov/mgawebsite/Legislation/Details/", tracker$x_file_bill_number, "?ys=2025RS' target='_blank'>", tracker$x_file_bill_number, "</a>", sep = "") 

###remove xfile hyperlinks for bills without an xfile
tracker <- tracker %>%
  mutate(
    dplyr::across(
      .cols = hyperlinked_xfile, 
      .fns = ~ dplyr::if_else(stringr::str_detect(.x, "NULL"), "None", .x)
    )
  )

###grab columns for table
tracktab <- select(tracker,hyperlinked_billnum,title,current_status,hyperlinked_xfile)

write_csv(tracktab, "tracktab.csv")