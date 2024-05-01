################################################################################
# 5 Sample
# 
# Retrieves answers for different gpt models
################################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("irudnyts/openai", ref = "r6")

pacman::p_load(tidyverse, here, jsonlite, openai, httr, rlist) 


# Import Data -------------------------------------------------------------

validation_formatted <- read_lines(here("data", "validation-formatted.jsonl")) %>% 
    map(~ fromJSON(.x, simplifyVector = FALSE)) %>% 
    map(pluck("messages")) %>% 
    map(~ `[`(.x, c(1, 2)))


# Get fine tuned model ----------------------------------------------------

URL <- "https://api.openai.com/v1/fine_tuning/jobs/"
ft_job_id <- read_lines(here("data", "ft-job-id.txt"))

headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
)

ft_model <- GET(paste0(URL, ft_job_id), add_headers(.headers = headers)) %>% 
    content() %>% 
    pluck("fine_tuned_model")


# Retrieve ----------------------------------------------------------------

client <- OpenAI()

complete <- function(entry, model) {
    completion <- client$chat$completion$create(
        model = model,
        messages = entry,
        seed = 123,
        temperature = 0
    )
    
    answer <- completion$choices[[1]]$message$content
}

answers_gpt35turbo <- validation_formatted %>% 
    map(~ complete(.x, "gpt-3.5-turbo-0125"))

answers_ft <- validation_formatted %>% 
    map(~ complete(.x, ft_model))

answers_gpt4turbo <- validation_formatted %>% 
    map(~ complete(.x, "gpt-4-0125-preview"))


# Export ------------------------------------------------------------------

write_json(answers_gpt35turbo, here("data", "sample-gpt35turbo.json"))
write_json(answers_ft, here("data", "sample-ft.json"))
write_json(answers_gpt4turbo, here("data", "sample-gpt4turbo.json"))

