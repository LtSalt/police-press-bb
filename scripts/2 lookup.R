################################################################################
# 2 Lookup
# 
# Creates a lookup table of nationalities
################################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(here, tidyverse, rvest, jsonlite)


# Load Data ---------------------------------------------------------------

nations <- read_html("https://deutsch.lingolia.com/de/wortschatz/laender-nationalitaeten") %>% 
  html_table() %>% 
  pluck(1) %>% 
  rename_with(~ c("land", "einwohner", "adjektiv"))


# Keywords ----------------------------------------------------------------

lookup <- nations %>% 
    mutate(across(everything(), ~ ifelse(.x == "–",
                                       NA,
                                       .x)),
           across(everything(), ~ str_remove_all(.x, "der\\/die|der |die |seltener|weiblich|…|Schweiz:")),
           across(everything(), ~ str_replace_all(.x, "auch: \\/|auch:|\\/:|\\/|offiziell:", ",")),
           nation = str_remove_all(land, "\\(.*\\)"),
           people = str_replace(einwohner, "BosnierinHerzegowiner", "Bosnierin, Herzegowiner"),
           adjective = str_replace(adjektiv, "bosnischherzegowinisch", "bosnisch, herzegowinisch"),
           keywords = pmap(list(nation, people, adjective), function(.x, .y, .z) { list(.x, .y, .z) }),
           keywords = map(keywords, ~ unlist(.x[!is.na(.x)])),
           keywords = map(keywords, ~ paste(.x, collapse = ", ")),
           keywords = map(keywords, ~ trimws(unlist(str_split(.x,",")))),
           keywords = map_chr(keywords, ~ paste0(.x, collapse = "|")),
           nation = trimws(str_remove_all(nation, ",.*"))
    ) %>% 
    select(nation, keywords) %>% 
    mutate(id = row_number(), .before = everything())

write_csv(lookup, here("data", "lookup.csv"))
