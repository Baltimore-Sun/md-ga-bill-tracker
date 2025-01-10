library(tidyverse)
library(janitor)
library(tidyr)

tracker <- read_csv("https://mgaleg.maryland.gov/2025rs/misc/billsmasterlist/BillMasterList.csv") %>% clean_names()

tracker <- replace_na(tracker, list(x_file_bill_number="NULL"))

tracker$hyperlinked_billnum <- paste("<a href='https://mgaleg.maryland.gov/mgawebsite/Legislation/Details/", tracker$bill_number, "?ys=2025RS' target='_blank'>", tracker$bill_number, "</a>", sep = "") 

tracker$hyperlinked_xfile <- paste("<a href='https://mgaleg.maryland.gov/mgawebsite/Legislation/Details/", tracker$x_file_bill_number, "?ys=2025RS' target='_blank'>", tracker$x_file_bill_number, "</a>", sep = "") 

tracker <- tracker %>%
  mutate(
    dplyr::across(
      .cols = hyperlinked_xfile, 
      .fns = ~ dplyr::if_else(stringr::str_detect(.x, "NULL"), "None", .x)
    )
  )

###List bills to track here
gentrack <- tracker %>% filter(bill_number == "SB0001" | bill_number == "SB0029")

###Define Anne Arundel lawmakers here
aatracker <- tracker %>% filter(grepl("Lehman|Bagnall|Anne Arundel|Bartlett|Chang|Chisholm|Howard|Jones, D|Kipke|Pruski|Rogers|Schmidt|Simmons|Melnyk|Barnes|Beidle|Gile|Henson|Lam|Rosapepe|Simonaire", sponsor, ignore.case=TRUE))
aatracker <- aatracker %>% arrange(sponsor, bill_number)

###grab columns for tables
tracktab <- select(gentrack,hyperlinked_billnum,title,current_status,hyperlinked_xfile)
aatab <- select(aatracker,hyperlinked_billnum,sponsor,broad_subject_name,title,current_status,hyperlinked_xfile)

write_csv(tracktab, "tracktab.csv")
write_csv(aatab, "aatab.csv")
