
library(dplyr)
library(magrittr)
library(readr)
checked0 <- read_csv("output/2022_taxonomy_checked.csv") %>% 
  dplyr::mutate(join = 1:nrow(.)) %>% 
  dplyr::select(-accepted_name) %>% 
  dplyr::mutate(survey_name0 = survey_name, 
                survey_name = gsub(pattern = " sp\\.$", 
                                      replacement = "", 
                                      x = survey_name))


checked <-  
  # joined species that have been synonymized
  dplyr::left_join(
    x = checked0, 
    y = checked0 %>% 
      dplyr::filter(suggested_name == survey_name) %>% 
      dplyr::select(suggested_name, join_suggested = join), 
    by = c("suggested_name")) %>% 
  dplyr::mutate(reason_auto = dplyr::case_when(
    reason == "invalid (misspelling)" ~ "invalid (misspelling)",
    is.na(join_suggested) ~ "invalid synonym", 
    is.na(database) & !grepl(paste0(rm_bits,"|sp\\."), survey_name) ~ "nomen nudum",
    join != join_suggested ~ "synonymized"), 
    join_suggested = ifelse(is.na(join_suggested), join, join_suggested)) %>%
  
  # dplyr::mutate(reason_auto = "blah") %>%
  
  dplyr::relocate(reason, reason_auto, join, join_suggested) %>% 
  dplyr::mutate(year_retired = 
                  ifelse(!is.na(reason_auto) | reason_auto == 	
                                        "nomen nudum", 2022, NA))


# check for new accepted species

checked_checkfornewspp <- checked[1:10, ] %>% 
  dplyr::filter(is.na(year_retired) &
                  database == "WORMS") %>% 
  dplyr::select(survey_name = suggested_name, species_code) %>%
  dplyr::add_row(survey_name = "Asterias phrygiana", species_code = 00000) %>%
  mutate(taxonomy = purrr::map(survey_name, ~ get_taxonomy(.x, "WORMS"))) %>%
  unnest(cols = taxonomy) %>%
  pivot_wider(names_from = rank, values_from = name) %>%
  dplyr::filter(survey_name != accepted_name) 
  

checked_checkfornewspp_new <- checked_checkfornewspp %>% 
  dplyr::mutate(join = (max(checked$join)+1):(max(checked$join)+nrow(checked_checkfornewspp)), 
                join_suggested = join)

# add new checked names to main table and change join_suggested to new value
checked_checkfornewspp_new %>%
  rename(suggested_name = accepted_name) %>%
  left_join(checked %>%
              dplyr::select(suggested_name, join),
            by = "suggested_name") %>% View



checked <- dplyr::bind_rows(
  checked, 
   
  )

  

