# Read in dive survey Nav csvs and map dive locations

#LIBRARIES------------
pacman::p_load(data.table, dplyr, readxl, purrr, readr, tidyr, stringr)

# Set the path to folder of waypoint notes containing the CSV files
path_to_csvs <- "input/waypoints/"

# List all CSV files in the directory
csv_files <- list.files(path_to_csvs, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files into a single DataFrame
# Define column names if you know the structure
column_names <- c("Sample", "Lat", "Long", "depth")
gearNoted = map(csv_files, ~read_csv(.x, col_names = column_names))%>%bind_rows()

gearNoted = gearNoted %>%
  filter(str_detect(Sample, regex("(fish|rope|trawl)", ignore_case = TRUE)))


summary(gearNoted)

#write shapefile
gearNoted_sf = gearNoted%>%st_as_sf(coords = c("Long", "Lat"), crs = 4326)
write_sf(gearNoted_sf, "output/gearNotedlive_sf.shp")

#write csv
write_csv(gearNoted_sf%>%st_drop_geometry(), "output/gearNotedlive.csv")



# Set the path to folder of nav data containing the CSV files-----
path_to_csvs <- "input/nav_files/"

# List all CSV files in the directory
csv_files <- list.files(path_to_csvs, pattern = "\\.csv$", full.names = TRUE)

# Read and combine all CSV files into a single DataFrame

clean_and_read_csv <- function(file_path) {
  read_csv(file_path, col_names = TRUE, col_types = cols(.default = col_character())) %>%
    rename_with(~str_trim(tolower(.)), .cols = everything())  # Clean up column names
}

# Map this function over CSV files and combine the results
ROV_NAV <- map(csv_files, clean_and_read_csv)%>%bind_rows()

ROV_NAV = ROV_NAV %>%select(1:4, 14:15)%>%rename(DateTime = `timestamp (utc)`, Lat = `latitude (4326)`, Long = `longitude (4326)`, 
                                                    Depth = `depth (m)`, Date = `timestamp (utc)...1`, Time = `timestamp (utc)...2` )
  
ROV_NAV = ROV_NAV%>%mutate(DateT = as.POSIXct(DateTime, TZ = "UTC", format ="%m/%d/%Y %H:%M"))

ROV_NAV = ROV_NAV%>%distinct( Date, .keep_all = TRUE)

# filter(str_detect(Sample, regex("(fish|rope|trawl)", ignore_case = TRUE)))


summary(ROV_NAV)

#write shapefile
gearNoted_sf = gearNoted%>%st_as_sf(coords = c("Long", "Lat"), crs = 4326)
write_sf(gearNoted_sf, "output/gearNotedlive_sf.shp")

#write csv
write_csv(gearNoted_sf%>%st_drop_geometry(), "output/gearNotedlive.csv")
