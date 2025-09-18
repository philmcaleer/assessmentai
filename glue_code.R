library(glue)
library(tidyverse)
library(readxl)

# load form of terms and definitions
dat <- read_xlsx("genai_activity_guidance_table.xlsx", sheet = 1) %>%
  rename(terms = `Main Skills Category`)

# get first letter of each term 
dat <- dat %>% 
  mutate(first_letter = substr(terms, 0, nchar(terms)), # capitalise first letter of term
         entry = "") %>%  # blank column for later
  arrange(first_letter, terms) # organise by first letter then term for entries in alphabetical order

# What pages to add to .yml to make copy and paste easier?
cat(paste0("     - ", sort.int(unique(dat$first_letter)), ".qmd", collapse = "\n"),
    file = "letters.txt")

# function to create individual entries by adding in terms and definitions 
make_entry <- function(dat){
  # for each entry, use glue to add the term, short definition, and long definition 
  entry <- glue("

## {dat$Activity}

**Gold standard for Professional Practice:** {dat$`Gold standard for professional practice`}

**How GenAI can be helpful (good practice):** {dat$`How GenAI can be helpful (good practice)`} 

**How GenAI can pose risks (poor practice):** {dat$`How GenAI can pose risks (poor practice)`}

**Assessment Type:** {dat$`Assessment type`}

**Example Assessments:** {dat$`Example assessments`}

---

")
} # careful spacing to make sure it formats the .qmds nicely later

# Now we are going to take the terms data and organise them into one .qmd per letter
# Step 1 - use the make entry function to combine the term, short, and long def 
for (i in 1:nrow(dat)){
  dat$entry[i] <- make_entry(dat[i, ])
}

# Step 2 - create a new data frame of unique letters to add all the entries in
entries_by_letter <- dat %>% 
  distinct(first_letter) %>% 
  mutate(entries = "")

# Save the unique letters to make loop iteration easier
unique_letters <-unique(dat$first_letter)

# Step 3 - For each unique letter, subset the original data frame
# this will produce n entries per letter
# for subset, paste all the entries together and save it in it's respective letter
for (i in 1:length(unique_letters)){
  subset_dat <- filter(dat, first_letter == unique_letters[i])
  
  entries_by_letter$entries[i] <- paste(subset_dat$entry, # paste together all entries in the col
                                        collapse = " ", # empty collapse to create one cell
                                        sep = "\n") # create a new line after every entry
}

# Step 4 - Create the main .qmd content 
# combine the letter for level 1 header, then all the entries following
entries_by_letter <- entries_by_letter %>% 
  mutate(qmd_entry = paste("# ", first_letter, "\n", entries))

# Step 5 - For each row of entries, create a .qmd file per letter 
# For each qmd cell, create a .qmd file for the header and all the entries
for (i in 1:nrow(entries_by_letter)){
  file_name <- paste0(entries_by_letter$first_letter[i], ".qmd")
  
  cat(entries_by_letter$qmd_entry[i], 
      file = file_name)
}

# Now, make sure letters are up to date in yml and rerender