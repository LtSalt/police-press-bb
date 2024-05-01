################################################################################
# 4 Train
# 
# Uses prepped training/validation data to fine tune gpt
################################################################################


# The 100 cases in the training & validation samples were coded manually in a spreadsheet
# Needs an Open AI API key that is integrated with your machine


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("irudnyts/openai", ref = "r6")

# jsonlite generates unnecessarily nested structures
pacman::p_load(tidyverse, here, readxl, RJSONIO, openai, httr) 


# Import ------------------------------------------------------------------

training_prepped <- read_excel(here("data", "training-prepped.xlsx"))
validation_prepped <- read_excel(here("data", "validation-prepped.xlsx"))

system_prompt <- readLines(here("prompts", "system-prompt.txt")) %>% 
    str_subset(".+") %>% 
    paste(collapse = "\n")



# Format ------------------------------------------------------------------

# Helper function to format data according to Open AI needs
format_entry <- function(system, user, assistant) {
    output <- list(
        messages = list(
            list(role = "system",
                 content = system
            ),
            list(role = "user",
                 content = user 
            ),
            list(role = "assistant",
                 # mark an entry as class 'asIs' to prevent flattening to scalar
                 # (see RJSONIO documentation)
                 content = map(assistant, I) %>% toJSON(encoding = "UTF-8", collapse = "", .na = "")
            )
        )
    )
}

# Put helper function in loop
format_data <- function(system_prompt, user_prompts, answers) {
    pmap(list(rep(system_prompt, length(user_prompts)),
              user_prompts,
              answers),
         format_entry
         )
        
}


# Train -------------------------------------------------------------------

df_to_list <- function(df) {
    df %>% 
        select(täter, opfer, zeugen) %>%
        mutate(across(täter:zeugen, ~ str_split(.x, ",") %>% map(trimws))) %>% 
        group_by(row_number()) %>% 
        group_split(.keep = FALSE) %>% 
        map(~ as.list(map(.x, unlist)))
}

write_jsonl <- function(formatted, path) {
    
    # since we append line-by-line, remove file if it already exists
    if (file.exists(path)) unlink(path)
    
    walk(formatted, ~ write(
        toJSON(.x, encoding = "UTF-8", collapse = "", .na = ""),
        path,
        append = TRUE
    ))
}

training_formatted <- format_data(system_prompt, 
                                  training_prepped$text, 
                                  df_to_list(training_prepped))

validation_formatted <- format_data(system_prompt, 
                                  validation_prepped$text, 
                                  df_to_list(validation_prepped))


write_jsonl(training_formatted, here("data", "training-formatted.jsonl"))
write_jsonl(validation_formatted, here("data", "validation-formatted.jsonl"))


# Train -------------------------------------------------------------------

# since `create_fine_tune()` in openai R package is broken (change in API),
# we create our own http request

url <- "https://api.openai.com/v1/fine_tuning/jobs"

headers <- c(
    "Content-Type" = "application/json",
    "Authorization" = paste("Bearer", Sys.getenv("OPENAI_API_KEY"))
)


training_info <- openai::upload_file(here("data", "training-formatted.jsonl"), 
                             "fine-tune")

validation_info <- openai::upload_file(here("data", "validation-formatted.jsonl"), 
                               "fine-tune")

payload <-  list(
    training_file = training_info$id,
    validation_file = validation_info$id,
    model = "gpt-3.5-turbo-0125",
    suffix = "pbb-archive",
    hyperparameters = list(n_epochs = 4)
    ) %>% 
    toJSON()

response <- POST(url, body = payload, encode = "json", add_headers(.headers = headers))


# Save job id for later use -----------------------------------------------

write_lines(content(response)$id, here("data", "ft-job-id.txt"))

