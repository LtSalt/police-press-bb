################################################################################
# 1 Scrape
# 
# Scrapes the press archive of the police of the state of Brandenburg
################################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, here, RSelenium, rvest, furrr, progressr)


# Prepare Selenium Browser ------------------------------------------------

# define search
url <- "https://polizei.brandenburg.de/suche/typ/Meldungen/1/1?search%5Bquery%5D=&search%5Bregion%5D=&search%5Bdienststelle%5D=&search%5Bkategorien%5D=&search%5BonlineDateFrom%5D=01.01.2010&search%5BdocType%5D=Pressemeldung&search%5Btags%5D=&search%5Bzeitraum%5D=&search%5BonlineDateTo%5D=20.03.2024&search%5BsearchButton2%5D=Suchen+%C2%BB#pbb-search-result-pager"

# start selenium browser
rd <- rsDriver(browser = "firefox")
remDr <- rd[["client"]]
remDr$navigate(url)

# use max pagination (50) 
remDr$findElement(using = "css selector", "a[href='/suche/typ/Meldungen/kategorie/null/limit/50?fullResultList=1']")$clickElement()


# Collect Links -----------------------------------------------------------

collect_links <- function() {
  links <- list()
  index <- 1
  
  # loop through the pager
  while(TRUE) {
    page_content <- remDr$getPageSource() %>% 
      pluck(1) %>% 
      read_html()
    
    current_links <- page_content %>% 
      html_elements("h4") %>% 
      html_elements("a") %>% 
      html_attr("href")
    
    links[[index]] <- current_links
    
    pager <- page_content %>% 
      html_elements("ul.pbb-atoz.pbb-pagination") %>% 
      html_elements("a") %>% 
      html_attr("title") %>% 
      last()
    
    # break loop when reaching the last entry
  if (pager != "naechste" | is.na(pager)) break
  
    # else, go to next entry
  index = index + 1
  remDr$findElement(using = "css selector", "a[title='naechste']")$clickElement()
  
  # wait 2 seconds for page to load
  Sys.sleep(2)
  }
  
  paste0("https://polizei.brandenburg.de", unlist(links))
}

links <- collect_links()

# write links to csv in case some error occurs in the next step
write_csv(tibble(links = links), here("data", "links.csv"))

# stop selenium browser
rd$server$stop()


# Traverse Links ----------------------------------------------------------

parse_element <- function(html, element) {
  return(html %>% html_element(element) %>% html_text2())
}

use_link <- function(link) {
  URL <- url(link, "rb")
  ct <- read_html(URL)
  
  meldung <- tibble(
    titel = parse_element(ct, "h1"),
    ort = parse_element(ct, "p.pbb-ort"),
    kreis = parse_element(ct, "p.pbb-landkreis"),
    meta = html_elements(ct, "dd") %>% html_text2() %>% list(),
    datum = meta %>% pluck(1) %>%  last(),
    kategorie = meta %>% pluck(1) %>% first(),
    tags = ifelse(meta %>% pluck(1) %>% length() > 2,
                  meta %>% pluck(1) %>% `[`(2:(length(.) - 1)) %>% trimws() %>% paste(collapse = ", "),
                  NA),
    verantwortlich = parse_element(ct, "p[data-iw-field='adresse']") %>% str_extract("^(.*)\n", 1),
    text = parse_element(ct, "div.pbb-article-text")
  )
  
  close(URL)
  
  meldung %>% 
    select(-meta) %>% 
    mutate(across(1:last_col(), trimws))
}

# use parallel processing to speed up the program
traverse_links <- function(links) {
  plan(multisession, workers = availableCores())
  
    # implement progress bar
  with_progress({
    pb <- progressor(steps = length(links))
  
    output <- future_imap(links, function(x, i) {
      pb()
      
      output <- tryCatch(
        use_link(x),
        error = function(cond) {
          tibble(titel = NA,
                 ort = NA,
                 kreis = NA,
                 datum = NA,
                 kategorie = NA,
                 tags = NA,
                 verantwortlich = NA,
                 text = NA)
        }
        
        )
    })
  })
}

archive <- traverse_links(links)


# Save results to csv -----------------------------------------------------

archive_df <- bind_rows(archive) %>% 
  bind_cols(tibble(link = links)) %>% 
  mutate(id = row_number()) %>% 
  relocate(link, .before = text) %>% 
  relocate(id, .before = titel)

write_csv(archive_df, here("data", "press-archive.csv"))


