################################################################################
# Prep
# 
# Preps relevant data for classification
################################################################################


# Needs an Open AI API key that is integrated with your machine


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("irudnyts/openai", ref = "r6")

# jsonlite generates unnecessarily nested structures
pacman::p_load(tidyverse, here, RJSONIO, openai) 


# Import ------------------------------------------------------------------

to_classify <- read_csv(here("data", "relevant.csv")) %>% 
    filter(!is.na(nations_lookup))

# added quotation marks around desired output
system_prompt <- readLines(here("prompts", "system-prompt-adjusted.txt")) %>% 
    str_subset(".+") %>% 
    paste(collapse = "\n")


# Create formatted data ---------------------------------------------------

format_entry <- function(id, system, user) {
    output <- list(
        id = id,
        messages = list(
            list(role = "system",
                 content = system),
            list(role = "user",
                 content = user)
            )
    )
}


format_entries <- function(ids, system_prompt, user_prompts) {
    pmap(list(ids,
              rep(system_prompt, length(user_prompts)),
              user_prompts),
         format_entry
    )
}

entries <- format_entries(to_classify$id, system_prompt, to_classify$text)


# Classify ----------------------------------------------------------------

client <- OpenAI()

# exponential backoff from this template: https://platform.openai.com/docs/guides/rate-limits/error-mitigation?context=tier-free
with_backoff <- function(func, 
                         initial_delay = 1, 
                         exponential_base = 2,
                         max_retries = 10) 
    {
    
    wrapper <- function(...) {
        num_retries <- 0
        delay <- initial_delay
        
        while (TRUE) {
            tryCatch({
                return(func(...))
            },
            error = function(e) {
                num_retries <<- num_retries + 1
                
                if (num_retries > max_retries) {
                    stop(message(e))
                }
                
                delay <<- delay * exponential_base * (1 + runif(1))
                Sys.sleep(delay)
            })
        }
    }
    
    return(wrapper)
}

complete <- function(entry) {
    client$chat$completion$create(
        model = "gpt-4-0125-preview",
        messages = entry$messages,
        seed = 123,
        temperature = 0
    )
}

complete_with_backoff <- with_backoff(complete)

get_answer <- function(entry) {
    tryCatch({
        completion <- complete_with_backoff(entry)
        list(id = entry$id,
             answer = completion$choices[[1]]$message$content)
        
    }, error = function(e) {
        message(paste0("Error in entry ", entry$id, ": ", e))
        list(id = entry$id,
             answer = NA_character_)
    })
}


classify <- function(entry, path = here("data", "classified.jsonl")) {
    answer <- get_answer(entry) %>% 
        toJSON(encoding = "UTF-8", collapse = "", .na = "")
    
    # write directly to file in case sth goes wrong
    write(answer, path, append = TRUE)
}

entries %>% 
    walk(~ classify(.x), .progress = list(
            type = "iterator", 
            format = "{cli::pb_bar} {cli::pb_percent}",
            clear = TRUE
            )
    )

