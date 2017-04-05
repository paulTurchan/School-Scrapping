library(rvest)
library(dplyr)
require(stringr)

#Association of American Universities
aau_members <- data.frame(
  School = read_html("https://www.aau.edu/about/default.aspx?id=16710") %>%
    html_nodes("#ctl00_secaboutaauwidecph_main a") %>%
    html_text()
  ) %>% 
  filter(School != '') %>% #Drop blank td elements %>% 
  mutate(School = str_trim(str_extract(School, "^[^(]+"))) %>%  #Remove year and trailing space
  arrange(School)

#NCAA Members & Conferences
ncaa_members <- read_html("http://web1.ncaa.org/onlineDir/exec2/divisionListing?sortOrder=0&division=All") %>% 
  html_nodes("form table") %>% 
  html_table(fill = T, header = T)

ncaa_members <- ncaa_members[[1]] %>% #Transform scrap to data frame
  dplyr::rename(School = Institution) %>% 
  filter(!(School %in% c('1 - DI CA Test', '2 - DII CA Test', '3 - DIII CA Test'))) %>% #Drop test records
  select(-State, -`Reclass Division`) %>% 
  arrange(School)

ncaa_conferences <- ncaa_members %>%
  mutate(Division = str_trim(str_extract(Division, "^[^-]+"))) %>% #Clean divisions to remove FCS or FBS suffixes
  group_by(Conference, Division) %>% 
  summarise(Count = n())