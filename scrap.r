library(rvest)
library(dplyr)
library(stringr)
library(jsonlite)
library(httr)
library(fuzzyjoin)
library(urltools)

#IPEDS
GET("https://nces.ed.gov/ipeds/datacenter/data/HD2015.zip", write_disk("ipeds.zip", overwrite=TRUE), progress()) #Download and write IPEDS (institutional characteristics) data
ipeds <- read.csv(unzip("ipeds.zip")[1], stringsAsFactors = F) %>%
  rename(IPEDS_ID = UNITID, School = INSTNM) %>%
  select(IPEDS_ID, School)

#Association of American Universities
aau_members <- data.frame(
  School = read_html("https://www.aau.edu/about/default.aspx?id=16710") %>%
    html_nodes("#ctl00_secaboutaauwidecph_main a") %>%
    html_text()
  ) %>% 
  filter(School != "") %>% #Drop blank td elements %>% 
  mutate(School = str_trim(str_extract(School, "^[^(]+"))) #Remove year and trailing space

ipeds_aau_members <- aau_members %>%
  left_join(ipeds, by = c("School" = "School")) %>% 
  arrange(IPEDS_ID)

#Sport Affiliations

#NCAA Members & Conferences
ncaa_members <- read_html("http://web1.ncaa.org/onlineDir/exec2/divisionListing?sortOrder=0&division=All") %>% 
  html_nodes("form table") %>% 
  html_table(fill = T, header = T)

ncaa_members <- ncaa_members[[1]] %>% #Transform scrap to data frame
  dplyr::rename(School = Institution) %>% 
  filter(!(School %in% c("1 - DI CA Test", "2 - DII CA Test", "3 - DIII CA Test"))) %>% #Drop test records
  select(-State, -`Reclass Division`) %>%
  mutate(School = str_trim(str_extract(School, "^[^(]+"))) %>%  #Remove year and trailing space
  arrange(School)

ncaa_conferences <- ncaa_members %>%
  mutate(Division = str_trim(str_extract(Division, "^[^-]+"))) %>% #Clean divisions to remove FCS or FBS suffixes
  group_by(Conference, Division) %>% 
  summarise(Count = n()) %>% 
  arrange(Conference)

ipeds_ncaa_members <- ncaa_members %>%
  left_join(ipeds, by = c("School" = "School")) %>% 
  arrange(IPEDS_ID)
write.csv(ipeds_ncaa_members, file = "National Collegiate Athletic Association (NCAA) Members with IPED IDs.csv", row.names = F)

#NAIA Members
naia_translation <- data.frame(
  Conference = c("River State Conference", "Appalachian Athletic Conference", "Wolverine–Hoosier Athletic Conference", "Golden State Athletic Conference", "Sun Conference", "Heart of America Athletic Conference", "Sooner Athletic Conference", "North Star Athletic Association", "California Pacific Conference", "Kansas Collegiate Athletic Conference", "Crossroads League", "Southern States Athletic Conference", "Great Plains Athletic Conference", "Chicagoland Collegiate Athletic Conference", "Mid-South Conference", "Frontier Conference", "American Midwest Conference", "Association of Independent Institutions", "Cascade Collegiate Conference", "Gulf Coast Athletic Conference", "Red River Athletic Conference"),
  Abbreviation = c("RSC", "AAC", "WHAC", "GSAC", "TSC", "HAAC", "Sooner", "North Star", "Cal Pac", "KCAC", "Crossroads", "SSAC", "GPAC", "CCAC", "Mid-South", "Frontier", "Am. Midwest", "A.I.I.", "Cascade", "GCAC", "Red River")
)

naia_translation[] <- lapply(naia_translation, as.character)

naia_members <- read_html("http://www.playnaia.org/remote/schools.php?viewAll=") %>% 
  html_nodes("table") %>% 
  html_table(fill = T, header = T)

naia_members <- naia_members[[1]] %>% 
  mutate(School = str_trim(str_extract(School, "([^\n]*)\n"))) %>% #Retain string before new line (\n)
  rename(Abbreviation = Conference) %>% #Prepare for translation
  left_join(naia_translation, by = c("Abbreviation" = "Abbreviation")) %>% #Join in full conference names
  select(-(Location:Enrollment)) %>% 
  arrange(School)

naia_conferences <- naia_members %>%
  group_by(Conference) %>% 
  summarise(Count = n()) %>% 
  arrange(Conference)

#NJCAA
# njcaa_regions <- read_html("http://www.njcaa.org/member_colleges/Organization_of_NJCAA_Regions") %>% 
#   html_nodes(".njcaaTable") %>% 
#   html_table()
# 
# njcaa_regions <- njcaa_regions[[1]] %>% 
#   rename(Abbreviation = X1, Region = X2)

njcaa_members <- fromJSON("http://api.njcaa.org//Common/GetCollegesByAcademicYearsSportDivisionRegionAndState/13/All/All/All/All") %>%
  rename(School = Name) %>% 
  mutate(Region = paste("Region ", Region, sep = '')) %>% 
  select(School, Region) %>% 
  arrange(School)

njcaa_regions <- njcaa_members %>%
  group_by(Region) %>% 
  summarise(Count = n()) %>% 
  arrange(Region)