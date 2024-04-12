#This script imports xls annotation data output files collected during review of Gully 2022 ROV surveys 
# from into one table, formats GPS, 

#Laura Feyrer 2024


#LIBRARIES------------
pacman::p_load(data.table, dplyr, readxl, purrr, readr, tidyr, stringr)

here::here()
year = 2022

# # # Read multiple sheets from xlsx and combine
read_multiple_sheets<- function(file_path, sheet_names) {
  # Use map to apply read_xlsx to each sheet name and return a list of data frames
  data_list <- map(sheet_names, function(sheet_name) {
    df = read_xlsx(file_path, sheet = sheet_name, col_types = "text")
    # Add a new column for the sheet name (ID variable)
    df$Survey <- sheet_name
    return(df%>%select(Survey, everything()))
  })
  # Combine the list of data frames into a single data frame
  combined_data <- do.call(rbind, data_list)
  
  return(combined_data)
}

#Data------------
original_path <- (here::here("input/"))

    #read in xlsx files
    files_to_read = list.files(original_path, pattern="xlsx", full.names = T)
    
    # a list of sheets 
    sheets_to_read <- c("SURVEY M5", "SURVEY M6", "SURVEY M7", "SURVEY M8","SURVEY M9","SURVEY M10","SURVEY M11","SURVEY M12",
                        "SURVEY M13","SURVEY M14","SURVEY M15") # Add or remove sheet names as needed
    
   
    # For each file, read the specified sheets and combine into one data frame
    # For each file, read the specified sheets as characters and combine into one data frame
    all_data <- map_df(files_to_read, function(file_path) {
      read_multiple_sheets(file_path, sheets_to_read)
    })
    
    summary(all_data)
    
#create var for the surveys that have been annotated
    all_data = all_data%>%mutate(Debris_ID = as.factor(`Debris #`), Type = as.factor(`Litter Type (ICES)`),Reviewed = ifelse(is.na(Debris_ID), "NO", "YES"))
    
    all_data<-all_data  %>% # pipe - create a sequence of data cleaning operations 
         mutate(Latd_m = str_extract(all_data$Lat,"(?<=N|S|E|W)\\d+ \\d+\\.\\d+"))%>%
      # Split into separate degree and minute columns
      separate(Latd_m, into = c("degrees", "minutes"), sep = " ") %>%
      # Convert degrees and minutes to numeric
      mutate(across(c(degrees, minutes), as.numeric)) %>%
      # Convert minutes to decimal and calculate final decimal degrees
      mutate(Latitude = (degrees + minutes) / 60) %>%
    mutate(Longd_m = str_extract(all_data$Long,"(?<=N|S|E|W)\\d+ \\d+\\.\\d+"))    %>%
      # Split into separate degree and minute columns
      separate(Longd_m, into = c("degrees", "minutes"), sep = " ") %>%
      # Convert degrees and minutes to numeric
      mutate(across(c(degrees, minutes), as.numeric)) %>%
      # Convert minutes to decimal and calculate final decimal degrees
      mutate(Longitude = ((degrees + minutes) / 60*(-1)) %>%select(-degrees, -minutes)
    
    #########
    #data summaries of analyses to date------
    all_data%>%group_by(Reviewed)%>%dplyr::summarise(Processed = n())

    #April 11 = 36% of videos reviewed
    67/(67+110)
    
    
    #check which have debris annotations  - 27 unique items of debris observed across 35 observations, majority plastic 
    videos = all_data%>%group_by(Survey, Debris_ID, Type) %>%dplyr::summarise(Debris_unique = n( ))
    videos%>%group_by(Survey, Debris_ID) %>%dplyr::summarise(surveys = n( ))%>%na.omit()%>%summarise()

    obs = videos%>%filter(Debris_ID!= 0)
    annotations_total = sum(obs$Debris_unique)
    

#clean working variables out for writing output

    #understand hours of footage reviewed
  time =  read_xlsx(files_to_read, "Sheet1")
  time = time%>%mutate(hours = Time.End-Time.Start)
  
 time%>%group_by(Complete)%>% summarise(hrs_total = sum(hours))
 
 22.3/(22.3+54.01667)
  
    

#write csv
write.csv(all_data, file = paste0("output/gully_rov_debris", Sys.Date(), ".csv"), row.names = FALSE)
write_rds(all_data, file = paste0("output/gully_rov_debris", Sys.Date(), ".rds"))



