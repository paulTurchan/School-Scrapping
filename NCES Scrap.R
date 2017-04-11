ipeds_ncaa_members_temp <- ipeds_ncaa_members %>% filter(is.na(IPEDS_ID))

for(i in 1:nrow(ipeds_ncaa_members_temp)){
  
  #Parse current URL
  current_school <- str_replace_all(url_encode(ipeds_ncaa_members_temp$School[i]),"%20","+")
  
  #Count how many results were returned
  results <- length(
    read_html(paste("https://nces.ed.gov/globallocator/index.asp?search=1&State=&city=&zipcode=&miles=&itemname=", current_school, "&sortby=name&College=1", sep ='')) %>%
      html_nodes("#hiddenitems_college a") %>%
      html_text()
  )
  
  #Grab IPEDS ID from JS popup
  ipeds_id <- read_html(paste("https://nces.ed.gov/globallocator/index.asp?search=1&State=&city=&zipcode=&miles=&itemname=", current_school, "&sortby=name&College=1", sep ='')) %>%
    html_nodes("#hiddenitems_college tr:nth-child(1) a") %>%
    html_attr("href")
  
  #Strip out IPEDS ID
  ipeds_id <- str_sub(str_replace_all(ipeds_id,"^[^=]+=",""), 1, 6) #Needs more robust solution
  
  ipeds_ncaa_members_temp$IPEDS_ID[i] <- if(results == 1) ipeds_id else "Unknown"
  
  print(paste("Loop ", i, " of ", nrow(ipeds_ncaa_members_temp), " (", round((100 * (i / nrow(ipeds_ncaa_members_temp))), digits = 3), "% complete) - ", if(results == 1) ipeds_id else "Unknown", " (", ipeds_ncaa_members_temp$School[i],")", sep = ''))
  
}

print(paste("Finished with a ", round(100 * sum(ipeds_ncaa_members_temp$IPEDS_ID != 'Unknown') / nrow(ipeds_ncaa_members_temp), digits = 3), "% success rate.", sep = ''))