################################################################################
# 3 Split
# 
# Search for nations in corpus & create training / validation data
################################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, here, lubridate, writexl)


# Load Data ---------------------------------------------------------------

archive <- read_csv(here("data", "press-archive.csv"))
lookup <- read_csv(here("data", "lookup.csv")) 


# Find relevant cases -----------------------------------------------------

relevant <- archive %>% 
    mutate(datum = dmy(datum),
           jahr = year(datum)) %>% 
    filter(
        # only ~ 450 entries before 2017, coverage probably dubious -> remove these cases
        jahr > 2016 & jahr < 2024,
        # keep only press reports filed under category "crime"
        kategorie == "Kriminalität") %>% 
    select(-jahr) %>% 
    mutate(
        # for each press release, search the lookup table for keywords
        # if anything is found, keep ids of the resulting dataframe
        nations_lookup = map(text, 
                         ~ filter(lookup, str_detect(.x, keywords)) %>% pull(id)),
        # match ids with the corresponding nations and concatenate in string, separating with comma 
        # (keeping a simple data structure)
           nations_lookup = map_chr(nations_lookup, 
                             function(x) if (length(x) == 0) NA_character_ else paste(x, collapse = ",")))


# Create training & validation samples ------------------------------------

# create reproducable training & split samples, drawing from all cases where keywords were found
set.seed(123)
indices <- sample(1:nrow(filter(relevant, !is.na(nations_lookup))), 100)

sample_cases <- relevant %>% 
    filter(!is.na(nations_lookup)) %>% 
    slice(indices)

training <- sample_cases %>% slice(1:50)
validation <- sample_cases %>% slice(51:100)


# Export ------------------------------------------------------------------

write_csv(relevant, here("data", "relevant.csv"))
write_xlsx(training, here("data", "training.xlsx"))
write_xlsx(validation, here("data", "validation.xlsx"))
