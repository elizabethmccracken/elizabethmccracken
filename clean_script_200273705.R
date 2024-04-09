library(tidyr)

# Read the dataset and make first row into column names
messydata <- read.delim("Spotify_Messy_200273705.txt",
                        header = TRUE)
library(tidyverse)

colnames(messydata)[2] = "track_name"

# changing title of column ZZtrack_name89 to "track_name" 
# to make it tidier

# split danceability.energy into two columns
# danceability and energy 

library(dplyr)
messydata<- messydata %>% separate(danceability.energy, 
                                   c('danceability', 'energy'),
                                                   sep= '_')
#  split column into two 

# danceability and acousticness as all the values must be 0-1

typo <- filter(messydata, energy >1 & energy <0)

# use this to search typos in following columns ^
# by changing from >1 and <0 and searching through the columns

# replace A typos in mode by removing them 

?replace

messydata$mode <- gsub("A", "", messydata$mode)

# got rid of all the "A"
# these values are a typo as the values should be between 0 and 1 

# change everything to lowercase 

messydata[] <- lapply(messydata, tolower)

# search for typos in 
# Shakira, Taylor Swift, Janis Joplin, The Four Owls, and Bad Bunny
messydata$track_artist[grep("nn", messydata$track_artist, ignore.case = TRUE)]

# no mispelling in shakira, janis joplin, four owls 
# or bad bunny 

# fixing tailor swift typo 

messydata$track_artist <- gsub("tailor swift", "taylor swift", 
                               messydata$track_artist, ignore.case = TRUE)

# checking taylor swift is spelt properly 

messydata$track_artist[grep("taylor swift", messydata$track_artist, ignore.case = TRUE)]

# making a sub genre and genre column

messydata<-pivot_longer(messydata,cols = "pop":"edm",
             names_to = "playlist_genre",
             values_to = "playlist_sub_genre")
messydata<-messydata[!is.na(messydata$playlist_sub_genre),]

# checking for duplicate data with any_dupe
any_dupe <- any(duplicated(messydata))

# output is FALSE - no duplicated rows 

# track_album_release_date 
# verify column is in date format

class(messydata$track_album_release_date)
# returns character 

library(dplyr)

library(lubridate)
# changing format from character to date in
# track album release date column

dates <-ymd(messydata$track_album_release_date)
dates

years <-year(dates)
messydata$release_year <-years
# then set as a column for years

months <-month(dates)
messydata$release_month <-months
days <- day(dates)
messydata$release_day <- days
# impute missing dates in the year column from original column
messydata$release_year[is.na(dates)] <- messydata$track_album_release_date[is.na(dates)]

# deleting original column
messydata <- select(messydata, -track_album_release_date)

# replacing all 3000 values in release_year with NA

messydata$release_year[messydata$release_year==3000]<-NA

# checking the types of values in the columns
glimpse(messydata)

# verifying columns are in the same format as the dictionary  

n_distinct(messydata)

# output gives 1800, which is no.of rows t/f
# no duplicated rows

# track_id, track_album_id and playlist_id contain the same values and
# can be merged into 1 column song_id

messydata <- mutate(messydata, song_id = paste(track_album_id, track_id, 
                                                playlist_id, sep = "_"))
# deleting the original columns 
messydata <- select(messydata, -c(track_album_id, track_id, playlist_id))

# putting columns in the same order as they appear in dictionary
messydata <- messydata %>%
  select(
    song_id,track_name,track_artist,track_popularity,
    track_album_name,release_day,release_month,
    release_year, playlist_name,playlist_genre,
    playlist_sub_genre,danceability,energy,
    key,loudness,mode,speechiness,
    acousticness,instrumentalness,liveness,
    valence,tempo, duration_ms
  )
# exporting messydata as a csv now that it has been cleaned 

write.csv(messydata, "clean_data_200273705.csv", row.names=FALSE)
