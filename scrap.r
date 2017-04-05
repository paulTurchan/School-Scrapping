library(rvest)
library(dplyr)
require(stringr)

#Scrap Association of American Universities
members <- data.frame(
  School = read_html("https://www.aau.edu/about/default.aspx?id=16710") %>%
    html_nodes("#ctl00_secaboutaauwidecph_main a") %>%
    html_text()
  ) %>% 
  filter(length(School) > 0) %>% #Drop blank td elements %>% 
  mutate(School = str_extract(School, "^[^(]+"))