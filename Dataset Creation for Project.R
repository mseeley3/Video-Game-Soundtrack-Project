#please do not run all lines at once
#errors will occure due to the setwd()s being specific to the creator's setup


#setup
require(curl)
require(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(rvest)
library(httr)
library(stringdist)
library(R.utils)
library(ggplot2)
library(lazyeval)

#custom function to extract JSON data from youtube API
get_JSON <- function(x){fromJSON(x)}

#custom function to find the mode (the variable that occurs the most)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#create blank lists to store data generated from for-loop
all_game_titles<- c()
all_game_dates<- c()
all_game_scores<- c()
all_game_user_ratings<- c()

#decide from what years to get the games from
years<- as.character(c(2007:2016))

#the basic metacritic page to get information from, only needs the year as an input
the_page<- "http://www.metacritic.com/browse/games/score/metascore/year/pc/filtered?year_selected=%s&sort=desc"

#custom function to create scraping url by inputting the year
create_scrape_url <- function(x) {paste0(sprintf(the_page, x))}

#create list of urls to run through JSON
metcrit_url_list<- sapply(years, create_scrape_url)

#for-loop that collects data about the top 100 games from each year 
for(metcrit in metcrit_url_list){
  #set the add_header to "user-agent"
  page_data <- read_html(GET(metcrit, add_headers('user-agent' = 'r')))
  
  #get the game titles, then convert titles to html_text
  game_titles<- page_data %>% 
    html_nodes("#main .product_title") %>%
    html_text() %>%
    as.character()
  
  #store the number of games scraped
  number_of_games<- (1:length(game_titles))
  
  #use for-loop to clean up the list of titles
  for(number in number_of_games){
    number2<- number
    game_titles[number2]<- gsub(pattern = "\n", replacement = "", x = game_titles[number2]) %>%
      trim()  
  }
  
  #get the game release dates, then convert dates to html_text, then year-month-date format
  game_dates<- page_data %>% 
    html_nodes(".product_date") %>%
    html_text() %>%
    mdy()
  
  #make sure all rows are accounted for
  if(length(game_dates) != length(number_of_games)){
    print(game_titles[1])
  }
  
  #get the game score, then convert score to html_text, then numeric data
  game_scores<- page_data %>% 
    html_nodes(".small") %>%
    html_text() %>%
    as.numeric()
  
  #make sure all rows are accounted for
  if(length(game_scores) != length(number_of_games)){
    print(game_titles[1])
  }
  
  #get the game user rating, then convert rating to html_text, then numeric data
  game_user_ratings<- page_data %>% 
    html_nodes("#main .textscore") %>%
    html_text() %>%
    as.numeric()
  
  #make sure all rows are accounted for
  if(length(game_user_ratings) != length(number_of_games)){
    print(game_titles[1])
  }
  
  #add game_titles and game_dates to the bigger list
  all_game_titles<- append(all_game_titles, game_titles)
  all_game_dates <- append(all_game_dates, game_dates)
  all_game_scores <- append(all_game_scores, game_scores)
  all_game_user_ratings<- append(all_game_user_ratings, game_user_ratings)
}

#create dataframe to contain game titles and their release dates
#later add all_number_of_critics and all_number_of_users
game_title_df <-data.frame(all_game_titles, all_game_dates, all_game_scores, all_game_user_ratings)

#remove the white space in front of game titles
#create custom function to remove white space
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
#update game_title_df with new game titles
game_title_df$all_game_titles <- trim(game_title_df$all_game_titles)

View(game_title_df)





#SEARCH_API
#used youtube search API to get search results when each game title is searched along with the segment "soundtrack"


#assign the API url to yt_search
yt_search <- "https://www.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&order=relevance&publishedBefore=%s%%3A00%%3A00Z&q=%s+soundtrack+full+-cover+-radio&type=video&videoDuration=long&key=%s"

#custom function pastes elements(game titles) into the proper places within the search API url and creates a useable url
create_search_url <- function(x) {paste0(sprintf(yt_search, "2017-06-24T00", x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#ceate a list of game titles from 2016 to be the elements inserted into the search API
games <- game_title_df$all_game_titles
#replace spaces with "+" so it works within the url
games <- gsub(pattern=" ", replacement= "+", x=games)

#create a list of urls to run through the get_JSON function
search_url_list <- sapply(games, create_search_url)

#run the urls through the get_JSON data to get the JSON data from the youtube search API
search_JSON <- sapply(search_url_list, get_JSON)

#not all urls run will return the right amount of data
#find which search_JSON elements have the wrong number of components
#search_JSON has ~100 entries due to there being 100 games tested per year for 10 years
rows<-c(1:nrow(game_title_df))

#store the entry length each game return via search API
row_length_list<- c()

for(row in rows){
  
  row_length_list<- append(row_length_list, lengths(search_JSON[row]))
}

#find the entry length most games return
optimal_entry_length <-Mode(row_length_list)

bad_line <-c()

#remove games that don't return the optimal entry length
for(row in rows){
  
  row_length <- lengths(search_JSON[row])
  other_row_length<- length(search_JSON[row])
  
  if(row_length != optimal_entry_length){
    bad_line<- append(bad_line, row)
  }
  if(other_row_length != 1){
    bad_line<- append(bad_line, row)
  }
}

row2<- rows[-bad_line]

for(row in row2){
  temp <- as.data.frame(search_JSON[row])
  
  if(nrow(temp) != 50){
    bad_line<- append(bad_line, row)
  }
}


#remove inconsistent lines in search_JSON, turn search_JSON to dataframe with bad lines removed
search_JSON_df <- data.frame(search_JSON[-bad_line])

#.items.snippet column contains the titles of the returned youtube videos
#create dataframe with just the ".items.snippet" column
new_df <- subset.data.frame(select(search_JSON_df, ends_with("snippet")))

#nested within the .items.snippet column is the "title" column
#create function to specify the "title" from ".item.snippet"
get_title <- function(x){subset.data.frame(select(x, starts_with("title")))}
#store "title" information into "titles"
titles <- mapply(get_title, new_df, SIMPLIFY = FALSE)

#turn "titles" into a clean dataframe
#turn titles into dataframe
titles_df <- data.frame(titles)

#column names for titles_df is currently title, title1, title2, title3, etc...
#rename column names for titles_df with the name of the game

#create list of game titles from new_df
game_titles_list <- colnames(new_df)
#remove .items.snippet.title from the end of each element of titles_df
game_titles_list <- gsub(".items.snippet", "", x= game_titles_list)
#replace "." with " " from each row
game_titles_list <- gsub("\\.", " ", x= game_titles_list)
game_titles_list <- gsub("  ", " ", x= game_titles_list)
#apply game titles as title_df's new column names
colnames(titles_df) <- game_titles_list

View(titles_df)


#SEGMENT SEARCH
#search through each game's video titles to find a segment that is the most common amongst the 50 titles
#this segment will most likely be the game title, or a commonly used abbreviation (ie: Battlefield 1 is BF4)

#create another dataframe so the original video titles are unchanged
titles2_df<- as.data.frame(titles_df)

#create a list with the number of games that are being investigated
gg_games<-c(1:ncol(titles2_df))

#remove special characters from video titles
#remove "Soundtrack" from video titles
for(gg_g in gg_games){
  titles2_df[,gg_g] <-gsub(pattern = "\\[", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\]", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\(", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\)", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\:", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\|", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\/", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "  ", replacement = " ", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "//?", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "-", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\{", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\}", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "'", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "|", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "Soundtrack", replacement = "", x = titles2_df[,gg_g])
  titles2_df[,gg_g] <-gsub(pattern = "\\", replacement = "", x = titles2_df[,gg_g], fixed = TRUE)
  titles2_df[,gg_g] <-gsub(pattern = "  ", replacement = " ", x = titles2_df[,gg_g], fixed = TRUE)
  titles2_df[,gg_g] <-gsub(pattern = ".", replacement = " ", x = titles2_df[,gg_g], fixed = TRUE)
}

#there are 50 video titles per game
title_number <- c(1:50)
segment_lengthh <- c(8:12)
segment_startt<- c(1:20)


#create an empty list to store all the best segments into
#best_segments<-c()
best_segments_3 <- c()

for(gg_g in gg_games){
  gg2<- gg_g
  
  #create empty lists to store segments and segment frequency for each game
  segment_collection <- c()
  num_of_exact_matches<- c()
  
  #chop all video titles from a game into segments of different lengths and start points
  #store all created segments into segment_collection
  for(title in title_number){
    title2<- title
    
    min_segment_length<- c(8)
    
    #if the game title is shorter than 8, shrink the segment_length
    if(nchar(colnames(titles2_df[gg2])) < 8){
      segment_lengthh <- c(3:7)
      min_segment_length<-c(3)
    }
    
    for(lengthh in segment_lengthh){
      lengthh2<- lengthh
      
      for(startt in segment_startt){
        segment<-substr(titles2_df[,gg2][title2], start=startt, stop=(startt + lengthh2))
        if(nchar(segment) >= min_segment_length){
          segment_collection <- append(segment_collection, segment)
        }
      }
    }
  }
  
  
  #store the segments minus the repeats into another list
  #this is the list from where each segment will be pulled out from to be tested
  unique_segment_collection<- unique(segment_collection)
  
  #outline
  #the overall goal is now to put unique segments in order of how many times they appear, most to least
  #then store the 5 most frequent segments into a list
  
  #1) unique_segment_collection <-- the segment collection filtered for unique values
  
  #2) num_of_exact_matches <-- each segment from unique_segment_collection is tested for how many exact matches it has within the original segment_collection, and this number is stored
  
  #3) sorted_match_spots <-- num_of_exact_matches are sorted for unique values, then sorted from greatest to least
  #this is the key that allows the unique segments to be grouped by frequency
  
  #4) same_match_level_segments <-- isolate segments by frequency
  #group all the segments that appeared 30 times, all the segments that appeared 20 times, etc
  #from each frequency group, remove the segments that are found in other segments
  #ie: remove "ioshock" because it is found in "Bioshock"
  
  #5) sorted_segments <-- segments are stored in order of greatest frequency to least frequency
  #segments that occure over 200 times or more are not included to reduce running time
  
  #6)best_segments_2 <-- the first 5 segments from sorted_segments that don't occure more than 200 times in all the other game video titles are stored
  
  
  
  
  
  #create a list of how many unique segments exist
  num_of_segments<- c(1:length(unique_segment_collection))
  
  #for-loop to test each segment against the segment_collection
  #the more often the segment from unique_segment_collection appears in the original segment_collection, the more common the segment is
  for(num in num_of_segments){
    
    #use stringdist to get a number on how close the segments match
    match_distance <-stringdist(unique_segment_collection[num], segment_collection)
    
    #collect how many times a segment had an exact match (match_distance = 1)
    num_of_exact_matches<- append(num_of_exact_matches, table(match_distance <= 1)["TRUE"])
  }
  
  #sort the number of exact matches between segments in decending order
  sorted_match_spots_2<- unique(sort(num_of_exact_matches, decreasing = TRUE))
  
  #create empty list to store the reuslts of the for-loop into
  sorted_segments<-c()
  
  #for-loop groups segments by frequency and eliminates segments that are smaller sections of other segments
  for(sorted in sorted_match_spots_2){
    #find indicies for the level of frequency
    testtest<- grep(sorted, num_of_exact_matches)
  
    #convert indicies to the actual segmetns
    same_match_level_segments <- unique_segment_collection[testtest]
  
    #only look at frequency levels that have 200 or less segments in them
    if(length(same_match_level_segments) <= 200){
      for(same in same_match_level_segments){
        #only keep segments that only appear once
        if(table(grepl(same, same_match_level_segments, fixed= TRUE))["TRUE"] == 1){
          sorted_segments <- append(sorted_segments, same)
        }
      }
    }
  }  
  
  #create increment meter to limit best segments to 5 per game 
  n<- c(1)
  c<- c(1)
  
  #create a temporary dataframe that contains all the game and their titles minus the game that is currently being inspected
  temp_titles_df<- titles2_df[-gg2]
  
  #create a list of game titles from the temporary dataframe
  all_titles3<- unlist(temp_titles_df[,1:ncol(temp_titles_df)])
 

  #keep n at 5 or under
  #each time a suitable segment term is found, n increases by one
  while(n<= 5){
    
    segment<- sorted_segments[c]
    
    #if searching for the segment within the list off all titles from other games results TRUE
    if(is.na(table(grepl(segment, all_titles3, fixed = TRUE))["TRUE"]) == FALSE){
      
      #store segments that result in TRUE 200 or less times
      if(table(grepl(segment, all_titles3, fixed = TRUE))["TRUE"] <= 200){
        best_segments_3<- append(best_segments_3, segment)
        n <- n + 1
      }
      #if the segment is not found in other games titles they should still be added to the list
    }else{
      best_segments_3<- append(best_segments_3, segment)
      n <- n + 1
    }
    
    c <- c + 1
    if(c == (length(sorted_segments) + 1)){
      print("stop")
      print(n)
      
      if(n == 1){
        best_segments_3<- c(best_segments_3, NA, NA, NA, NA, NA)
      }
      
      if(n == 2){
        best_segments_3<- c(best_segments_3, NA, NA, NA, NA)
      }
      if(n == 3){
        best_segments_3<- c(best_segments_3, NA, NA, NA)
      }
      if(n == 4){
        best_segments_3<- c(best_segments_3, NA, NA)
      }
      if(n == 5){
        best_segments_3<- c(best_segments_3, NA)
      }
      
      n<-6
    }
  }
  print(colnames(titles2_df[gg2]))
}
















#STAT_API
#run video titles through a youtube stats API to get view count, like count, dislike count, comment count of each video

#API url to look up stats for videos
yt_stat_search <- "https://www.googleapis.com/youtube/v3/videos?part=statistics&id=%s&key=%s"

#custom function to create url for the stats API, requires etag of video (not the title like the previous API)
create_stats_url <-function(x) {paste0(sprintf(yt_stat_search, x, "AIzaSyCPOy2cpD0SOXEfGDoc2v0riFqtSQ172ow"))}

#stats API require etag of video
#find the corresponding etag for the relevent videos from the original search_JSON dataframe


#grab the section of search_JSON_df that contains the etag
new_2_df <- subset.data.frame(select(search_JSON_df, ends_with("items.id")))

#create function to further reduce dataframe and apply to above dataframe
get_videoID <- function(x){subset.data.frame(select(x, ends_with("videoID")))}

#store all etags into new dataframe
videoID<- mapply(get_videoID, new_2_df)
videoID_df<- data.frame(videoID)

#create dummy list from key_terms_list
best_segments_2_list <- as.list(best_segments_3)

#reset which_key_terms
which_key_term<-c(1:5)

#create an empty list to store dataframes created from running the stats API
list_of_data_frames<- as.list(c())

non_list_of_data_frame<- as.list(c())

soundtrack_data_df<-data.frame()

final_game_names<- as.list(final_game_title_df$all_game_titles)

num_of_useable_videos<- c()

#run for-loop that gathers stats for each relevant video
for(gg_g in gg_games){
  gg2<- gg_g
  
  #create an empty list to store video titles that contain one of the terms
  relevent_titles<-c()
  
  #focus which terms should be used as a filter for this particular game
  used_segments <- best_segments_3[which_key_term]
  #get rid of extra spaces attached to use_segments
  used_segments<- trim(used_segments)
  
  segment_valid_collection<- c() 
  used_segment_additions<-c()
  
  for(used in used_segments){
    #look for used_key_term in colname of title2_df
    #grepl will result in TRUE if segment is found
    segment_valid_collection <- append(segment_valid_collection, grepl(used, colnames(titles2_df[gg2]), ignore.case = TRUE))
  }
  
  #if searching for TRUE in segment_valid_collection results in a NA, then that means none of the segments were found within the game title
  if(is.na(table(grepl("TRUE", segment_valid_collection))["TRUE"]) == TRUE){
    
    #created segments fromg game title of varying lenght and starting location
    for(lengthh in segment_lengthh){
      lengthh2<- lengthh
      
      for(startt in segment_startt){
        segment<-substr(colnames(titles2_df[gg2]), start=startt, stop=(startt + lengthh2))
        used_segment_additions <- append(used_segment_additions, segment)
      }  
    }
    
    #remove whitespace at the begining of each segment
    
    used_segment_additions<- trim(used_segment_additions)
    
    #filter for unique used_segment_additions
    unique_additions<- unique(used_segment_additions)
    
    used_segment_additions_2<-c()
    
    for(unique in unique_additions){
      unique2<- unique
      
      #if when searching for the segment in all the other game titles results in TRUE
      if(is.na(table(grepl(unique2, all_titles3, fixed = TRUE))["TRUE"]) == FALSE){
    
        #store segments that result in TRUE 200 or less times
        if(table(grepl(unique2, all_titles3, fixed = TRUE))["TRUE"] <= 200){
          used_segment_additions_2 <- append(used_segment_additions_2, unique2)
          
        }
        #segments that don't appear in other games's titles should still be used
      }else{
        used_segment_additions_2 <- append(used_segment_additions_2, unique2)
      }
      used_segments <- append(used_segments, used_segment_additions_2) 
    }
  }
  
   
  
  #reduce the list of video titles to titles that contain the key_term
  if(length(which(is.na(used_segments))) < length(used_segments)){
    
    #narrow the list of video titles in titles_df to the ones that contain the used_segments (from the key_terms_list)
    for (used in used_segments){
      relevent_title_spots<- grep(used, titles2_df[,gg2], ignore.case = TRUE)
      relevent_titles<- append(relevent_titles, relevent_title_spots)
    }
  }
    
  if(length(relevent_titles) > 0){
    #filter the list of relevent titles indices for unique occurances
    key_title_spots <- unique(relevent_titles)
    #translate the indicies to actual video titles that will be used
    optimal_titles <- titles_df[,gg_g][key_title_spots]
      
    #turn the list of used video titles into a dataframe
    holder_data_frame<- data.frame(optimal_titles, stringsAsFactors = FALSE)
    holder_data_frame <- holder_data_frame %>% mutate(videoID=videoID_df[,gg_g][key_title_spots])
      
    #create list of urls to use from the etag column of holder_data_frame
    stats_url_list <- sapply(holder_data_frame[2], create_stats_url)
    
    #use urls that were created and JSON function to scrape Youtube Stats API for data
    stats_JSON <- sapply(stats_url_list, get_JSON)
    
    stats_JSON_df <- data.frame(stats_JSON)
    
    #manipulate stats_JSON_df 
    
    
    #transpose the stats dataframe to put video titles as rows
    stats_JSON_df<-as.data.frame(t(stats_JSON_df))
    
    #carve out section stats_JSON_df with subset that contains the stats
    stats <- subset.data.frame(select(stats_JSON_df, ends_with("items")))
    
    bahaha<-c(1:nrow(stats))
    stats_collection <- data.frame()
    
    #even after carving out "items" column from stats JSON, the results are still clumped together
    #use for-loop to separate stats JSON results into their separate components (viewCount, likeCount, etc) and store it in a matrix
    for (bah in bahaha) {
      vid_stat<-as.character(stats$items[bah])
      
      vid_stat_sep<- unlist(strsplit(vid_stat, split="\\,"))
      
      vid_stat_matrix <- matrix(data = vid_stat_sep[4:8], nrow = 5, ncol=1, byrow=TRUE)
      
      vid_df<- as.data.frame(vid_stat_matrix)
      
      stats_collection<- append(stats_collection, vid_df)
    }
    
    #store the matrix generated above into a dataframe
    stats_collection_df<- as.data.frame(stats_collection)
    colnames(stats_collection_df)<- holder_data_frame[1:nrow(holder_data_frame), 2]
    
    #when forming the matrix, sometimes the stats_JSON doesn't have anything for a certain stat for a video, and this messes up the data.
    #example: if a video returns nothing for "favoriteCount", commentCount" will be moved up a row in the matrix where "favoriteCount" should be
    #and the last spot on the matrix where "CommentCount" was supposed to be has an NA
    #we need the fourth row to contain NA, and the fifth row to contain "CommentCount"
    
    #rearrange stats_collection so that stats are in their appropriate rows
    
    #find the columns of stats_collection_df that contain NA
    #sum up the number of NAs per column
    NA_per_column<- colSums(is.na(stats_collection_df))
    
    #find all the columns that don't have any NAs
    non_empty_spots<-grep(pattern = 0, NA_per_column)
    
    #find columns with NAs by subtracting all the columns that don't have NAs
    lacking_columns<- c(1:ncol(stats_collection_df))[-non_empty_spots]  
    
    #create a list (go_test_row) of the objects in the lacking column
    #replace the NAs in go_test_row with term "placeholder" to prevent errors with using function: startsWith
    #replace every row in the lacking column with NA
    #re-fill the column by matching what the object in go_test_row list is with the appropriate row
    
    #only deal with lacking_columns that have something in them, otherwise the entire column should be NAs
    if(sum(lacking_columns) > 0){
      for (lacking in lacking_columns) {
        
        go_test_row<- as.character(stats_collection_df[,lacking])
        #replace the NAs in go_test_row with term "placeholder" to prevent errors with using function: startsWith
        go_test_row[grep(TRUE, is.na(go_test_row))] <- "placeholder"
        #ceate a blank slate, replace every row in the lacking column with NA, original values are stored in go_test_row
        stats_collection_df[,lacking]<- NA
        
        for(go_test in go_test_row){
          #if the object in go_test_row starts with "statistics", it should be placed in the first row of the column
          if(startsWith(go_test, " statistics") == TRUE){
            stats_collection_df[1,lacking]<- as.character(go_test)
          }
          #if the object in go_test_row starts with "likeCount", it should be placed in the second row of the column, etc, etc
          if(startsWith(go_test, " likeCount") == TRUE){
            stats_collection_df[2,lacking]<- as.character(go_test)
          }
          if(startsWith(go_test, " dislikeCount") == TRUE){
            stats_collection_df[3,lacking]<- as.character(go_test)
          }
          if(startsWith(go_test, " favoriteCount") == TRUE){
            stats_collection_df[4,lacking]<- as.character(go_test)
          }
          if(startsWith(go_test, " commentCount") == TRUE){
            stats_collection_df[5,lacking]<- as.character(go_test)
          }
        }
      }
    }
    
    #collect all the stats from test_data_fram_df into their appropriate lists (viewCount, likeCount, etc) to mass remove extra symbols
    number_of_videos <- c(1:ncol(stats_collection_df))
    
    viewCount_list<- c()
    likeCount_list<- c()
    dislikeCount_list<-c()
    favoriteCount_list<- c()
    commentCount_list<-c()
    
    for(num in number_of_videos){
      viewCount_list<- append(viewCount_list, as.character(stats_collection_df[1, num]))
      likeCount_list<- append(likeCount_list, as.character(stats_collection_df[2, num]))
      dislikeCount_list<- append(dislikeCount_list, as.character(stats_collection_df[3, num]))
      favoriteCount_list<- append(favoriteCount_list, as.character(stats_collection_df[4, num]))
      commentCount_list<- append(commentCount_list, as.character(stats_collection_df[5, num]))
    }
    
    #clean up the data
    #remove " statistics = list(viewCount = \"" and replace with ""
    viewCount_list<- gsub(pattern =" statistics = list(viewCount = \"", "", viewCount_list, fixed = TRUE)
    viewCount_list<- gsub(pattern ="\"", "", viewCount_list, fixed = TRUE)
    
    likeCount_list<- gsub(pattern =" likeCount = \"", "", likeCount_list, fixed = TRUE)
    likeCount_list<- gsub(pattern ="\"", "", likeCount_list, fixed = TRUE)
    
    dislikeCount_list<- gsub(pattern =" dislikeCount = \"", "", dislikeCount_list, fixed = TRUE)
    dislikeCount_list<- gsub(pattern ="\"", "", dislikeCount_list, fixed = TRUE)
    
    favoriteCount_list<- gsub(pattern =" favoriteCount = \"", "", favoriteCount_list, fixed = TRUE)
    favoriteCount_list<- gsub(pattern ="\"", "", favoriteCount_list, fixed = TRUE)
    
    commentCount_list<- gsub(pattern =" commentCount = \"", "", commentCount_list, fixed = TRUE)
    commentCount_list<- gsub(pattern ="))", "", commentCount_list, fixed = TRUE)
    commentCount_list<- gsub(pattern ="\"", "", commentCount_list, fixed = TRUE)
    
    
    
    #add stat data from the lists to the holder_data_frame
    holder_data_frame<- holder_data_frame %>% mutate(viewCount = viewCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(likeCount = likeCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(dislikeCount = dislikeCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(favoriteCount = favoriteCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(commentCount = commentCount_list)
    holder_data_frame<- holder_data_frame %>% mutate(game_name = colnames(titles2_df[gg_g]))
    
    #create a title for the holder_data_frame that reflects what game it has data of
    stats_df_lable <- paste0(gsub(pattern=" ", replacement= "_", x=colnames(titles_df[
      gg_g])), "_stats_df")
    
    #use this line to keep track of which game is being done as the for-loop is run
    print(stats_df_lable)
    
    #rename the column names the represent what the data in the column is
    colnames(holder_data_frame)<- c("video_titles", "etag", "viewCount", "likeCount", "dislikeCount", "favoriteCount", "commentCount", "game_name")
    
    #fill the empty list with the dataframe to be used later for manipulation and analysis
    list_of_data_frames[[gg_g]]<- data.frame(holder_data_frame)
    
    #name each object in the list_of_data_frames so we know which dataframe is which game
    names(list_of_data_frames[gg_g]) <- stats_df_lable 
    
    #store holder_data_frame into a catch-all dataframe
    soundtrack_data_df<-rbind(soundtrack_data_df, holder_data_frame) 
    
    num_of_useable_videos<- append(num_of_useable_videos, nrow(holder_data_frame))
    
    #increase which_key_term by 5 so the next 5 terms in the key_terms_list can be used with the next game
    which_key_term<- which_key_term +5      


    }else{
    
    non_list_of_data_frame<- append(non_list_of_data_frame, colnames(titles2_df[gg2]))
        
    #increase which_key_term by 5 so the next 5 terms in the key_terms_list can be used with the next game
    which_key_term<- which_key_term + 5
    
    #a game that the youtube search API can't return related videos is a game that unfortunately has to be thrown out of the study
    #add the game's number to bad_line so we can update the original game_title_df and remove the game from it
    bad_line<- c(bad_line, gg2)
  }
}
#there are empty spots in list_of_data_frames due to some games not returning anything cohesive from the search API
#get ride of these spots by using the non_list_of_data_frames that was created in the previous for-loop
#create list to store the indicies of the empty spots
bad_line_3<-c()

#for-loop finds empty spots by finding the indicies of the game name from non_list_of_data_frame from the column names of titles2_df

num_non<- c(1: length(non_list_of_data_frame))

for(num in num_non ){
  removal<- stringdist(as.character(non_list_of_data_frame[num]), colnames(titles2_df))
  removal <- paste0(" ", removal, " ")
  removal_spot<- grep(" 0 ", removal)
  bad_line_3<- append(bad_line_3, removal_spot)
}
       
#remove the games that had incomplete results from the search API                                                                                                                                                                                                                                                                                                                                                                                                                             #create a dataframe with the final list of games to be analyzed 
final_game_title_df <- data.frame(game_title_df[-bad_line_3,])

#dataframe row indexes are removed when row is removed
#if two games were removed, this results in the 98th game still being labeled the 100th index
#reassign the dataframes index appropriately 
rownames(final_game_title_df) <- 1:nrow(final_game_title_df)

game_title_df[4,]
colnames(titles2_df[4])
final_game_title_df[4,]












#add the average of the stats found through the youtube stats API to the original games_titles_df dataframe

#make sure the gg_games is updated to represent any changes made in the list of games
gg_games_2<- c(1: nrow(final_game_title_df))

#create a dataframe about the stats overall
#create an empty dataframe to store averaged stats into
game_stats_df<- data.frame()

#run for loop the get the total views, average views, like, dislikes, and comment count of all the videos for each game
for(gg_g in gg_games_2){
  game_data_frame <- data.frame(list_of_data_frames[gg_g])
  
  best_match<- sort(stringdist(final_game_title_df$all_game_titles[gg_g], soundtrack_data_df$game_name))[1]
  
  video_num<- length(grep(best_match, stringdist(final_game_title_df$all_game_titles[gg_g], soundtrack_data_df$game_name)))
  video_num<- nrow(game_data_frame)
  total_views<- sum(as.numeric(game_data_frame$viewCount), na.rm = TRUE) 
  total_likes<- sum(as.numeric(game_data_frame$likeCount), na.rm = TRUE)
  total_dislikes<- sum(as.numeric(game_data_frame$dislikeCount), na.rm = TRUE)
  total_favorites<- sum(as.numeric(game_data_frame$favoriteCount), na.rm = TRUE)
  total_comments<- sum(as.numeric(game_data_frame$commentCount), na.rm = TRUE)
  
  #game_data_line<- list(video_num, total_views, total_likes, total_dislikes, total_favorites, total_comments)
  
  game_stats_df[gg_g, 1]<- video_num
  game_stats_df[gg_g, 2]<- total_views
  game_stats_df[gg_g, 3]<- total_likes
  game_stats_df[gg_g, 4]<- total_dislikes
  game_stats_df[gg_g, 5]<- total_favorites
  game_stats_df[gg_g, 6]<- total_comments
  
  print(colnames(titles_df[gg_g]))
}

#label game_stats_df's columns appropriately
colnames(game_stats_df)<- c("video_num", "total_views", "total_likes", "total_dislikes", "total_favorites", "total_comments")

#merge dataframes so that game titles, game release dates, and average stats are all under one dataframe
final_game_title_df<- cbind(final_game_title_df, game_stats_df) 

View(final_game_title_df)

#set working directory
setwd("/Users/monicaseeley/Documents/R Files/Youtube API/")

#load final_game_title_df
final_game_title_df<- read.csv("final_game_title_df.csv")

#change data type in each final_game_title_df column as needed

#column "game_title" is Factor
str(final_game_title_df$game_title)
#turn "game_title" into character
final_game_title_df$game_title<- as.character(final_game_title_df$game_title)

#set working directory
setwd("/Users/monicaseeley/Documents/R Files/Youtube API/metacritic_links/")

#list of file names
meta_link_files <- list.files("/Users/monicaseeley/Documents/R Files/Youtube API/metacritic_links/")

link_num<- c(1:length(meta_link_files))

#create an empty dataframe to store the data from the metacritic csv files
links_df<- data.frame()

#open csv into R, then combined them all together in one dataframe
for(link in link_num){
  temp_file<- read.csv(meta_link_files[link])
  
  links_df<- rbind(links_df, temp_file)
  
}

#set working directory to original directory
setwd("/Users/monicaseeley/Documents/R Files/Youtube API/")

#game links in links_df are in the order they were imported into R
#need to reorder them to match the order of games in final_game_title_df 

#create a range for how many games are from game_title_df (probably 1000)
num_of_game_title <- c(1:nrow(game_title_df))

#create empty list to store links in correct order
list_of_links<-c()

#reorder links from links_df to match the order of games in game_title_df
#do this by comparing the game title in game_title_df$all_game_titles vs the game title in links_df$Anchor.Text
for(num in num_of_game_title){
  
  if(is.na(table(grepl(game_title_df$all_game_titles[num], links_df$Anchor.Text, fixed = TRUE))["TRUE"]) == FALSE){
    spot <-grep(game_title_df$all_game_titles[num], links_df$Anchor.Text, fixed = TRUE)
    
    for(sp in spot){
      if(nchar(as.character(game_title_df$all_game_titles[num])) + 1 == nchar(as.character(links_df$Anchor.Text[sp]))){
        list_of_links <-append(list_of_links, as.character(links_df$Link[sp]))
      }
    }
    #if it doesn't match up the game was in the links_df but not in the final_game_title_df
  }else{
    list_of_links <- append(list_of_links, NA)
  }
}

#final_game_title_df is reduced from game_title_df, list_of_links needs to reflect this

num_of_games <- c(1:nrow(game_title_df))

#create empty list to store bad terms into
bad_bad<- c()

#create counter
n2<-c(1)

#for-loop to compare each line of game_title_df to final_game_title_df
#if there is a game in game_title that is missing from final_game_title_df
#the row of final_game that needs to be looked at stays the same
for(num in num_of_games){
  if(stringdist(game_title_df$all_game_titles[num], as.character(final_game_title_df$game_title[n2])) > 1){
    bad_bad <- append(bad_bad, num)
    n2 <- n2 - 1
  }
  n2<- n2 + 1
}

#removed links that belong to games that are missing from final_game_titles_df
list_of_links <- list_of_links[-bad_bad]

#create a dataframe to contain games and their metacritic link
game_type_df<- data.frame(game_title_df$all_game_titles[-bad_bad], list_of_links)

#name the dataframe columns
colnames(game_type_df) <- c("game_title", "meta_link")

#create a range of games in game_type_df
num_final_games<- c(1:nrow(game_type_df))

#create empty list to store data scraped from each link in game_type_df
all_critic_num <- c()
all_user_num <-c()
all_game_types <- c()
all_ratings<- c()

#for-loop to scrape each page for data
for(num in num_final_games){
  
  if(is.na(game_type_df$meta_link[num])){
    all_critic_num<- append(all_critic_num, NA)
    
    #scraping webpage for the number of user reviews always returns two elements, one useful, one useless
    all_user_num <- append(all_user_num, NA)
    all_game_types<- append(all_game_types, NA)
    all_ratings <- append(all_ratings, NA)
  }else{
    
    #set the add_header to "user-agent"
    page_data <- read_html(GET(as.character(game_type_df$meta_link[num]), add_headers('user-agent' = 'r')))
    
    #get the number of critics, then convert to numeric
    critic_num<- page_data %>% 
      html_nodes(".count a span") %>%
      html_text() %>%
      as.numeric()
    
    #get the number of users, then convert to numeric
    user_num<- page_data %>% 
      html_nodes(".feature_userscore a") %>%
      html_text()
    
    #some games to not have a user rating because none were submitted
    #replace the data scraped with "placeholder" "placeholder"
    if(length(user_num) < 2){
      user_num<- c(NA)
    }
    
    #get the genre of the game, then convert to character
    game_type<- page_data %>% 
      html_nodes(".product_genre .data") %>%
      html_text() %>%
      as.character()
    
    #get the game rating, then convert to character
    game_rating<- page_data %>% 
      html_nodes(".product_rating .data") %>%
      html_text() %>%
      as.character()
    
    if(length(game_rating) < 1){
      game_rating<- NA
    }
    
    all_critic_num<- append(all_critic_num, critic_num)  
    all_user_num <- append(all_user_num, user_num)
    all_game_types<- append(all_game_types, game_type)
    all_ratings <- append(all_ratings, game_rating)
  }
  print(as.character(game_type_df$game_title[num]))
}


#format all_user_num 


#find where in all_user_num has the actual "Ratings"
user_rating_spots<- grep("Ratings", all_user_num)

#find where in all_user_num has NA
#that is where NAs should be because the metacritic link was missing
user_rating_empty_spots<- which(is.na(all_user_num))

#only keep every other NA because two NAs were placed per metacritic link that was NA
#user_rating_empty_spots_2<- user_rating_empty_spots[seq(1, length(user_rating_empty_spots), 2)]

#combine user_rating_spots and user_rating_empty_spots_2 to get the relevent data and the NAs
best_user_rating_spots<- sort(c(user_rating_spots, user_rating_empty_spots))

#translate the spots to actual user review numbers
all_user_num_2<- all_user_num[best_user_rating_spots]

#take the "Ratings" part out of all_user_num_2
num_of_all_user_num_2<- c(1: length(all_user_num_2))

for(num in num_of_all_user_num_2){
  if(is.na(grepl("Ratings", all_user_num_2[num])) == FALSE){
    
    temp <- unlist(strsplit(all_user_num_2[num], split=" "))
    
    all_user_num_2[num]<- as.numeric(temp[1]) 
  }
}


#create dataframe combining all the information
additional_data<- data.frame(all_critic_num, all_user_num_2, all_game_types, all_ratings)

#create and store critic and user review links
create_critic_url<- function(x){paste0(x, "/critic-reviews", collapse = "")}

all_critic_urls<- sapply(game_type_df$meta_link, create_critic_url)

create_user_url<- function(x){paste0(x, "/user-reviews", collapse = "")}

all_user_urls<- sapply(game_type_df$meta_link, create_user_url)

#collect the breakdowns of critic and user reviews
all_breakdown<- c()

for(num in num_final_games){
  
  if(is.na(game_type_df$meta_link[num])){
    all_breakdown<-append(all_breakdown, c(NA, NA, NA, NA, NA, NA))
  }else{
    
    #set the add_header to "user-agent"
    page_data <- read_html(GET(as.character(all_critic_urls[num]), add_headers('user-agent' = 'r')))
    
    #get the number of positive, mixed, and negative critic and user reviews,  then convert to numeric
    breakdown<- page_data %>% 
      html_nodes(".total .count") %>%
      html_text() %>%
      as.numeric()
    
    #if the scraping results turn out weird, replace everything with NAs
    if(length(breakdown) != 6 ){
      breakdown<- c(NA, NA, NA, NA, NA, NA)
    }
    
    all_breakdown <- append(all_breakdown, breakdown)
  }
  print(as.character(game_type_df$game_title[num]))
}

#separate all_breakdown into their respective types
all_critic_positive<- all_breakdown[seq(1, length(all_breakdown)-5, 6)]
all_critic_mixed<- all_breakdown[seq(2, length(all_breakdown)-4, 6)]
all_critic_negative<- all_breakdown[seq(3, length(all_breakdown)-3, 6)]

all_user_positive<- all_breakdown[seq(4, length(all_breakdown)-2, 6)]
all_user_mixed<- all_breakdown[seq(5, length(all_breakdown)-1, 6)]
all_user_negative<-all_breakdown[seq(5, length(all_breakdown), 6)] 

#collect data into dataframe
critic_user_reviews_df<- data.frame(all_critic_positive, all_critic_mixed, all_critic_negative, all_user_positive, all_user_mixed, all_user_negative)

#combine original dataframe with all the last two dataframes generated
final_game_title_df<- cbind(final_game_title_df, additional_data, critic_user_reviews_df)

#get column names from additional_data and critic_user_reviews_df
additional_data_colnames<- colnames(additional_data)
critic_user_reviews_colnames<- colnames(critic_user_reviews_df)

#change final_game_title_df column names for clarity and consistency
colnames(final_game_title_df)<- c("game_title", "game_release_date", "game_critic_score", "game_user_score", "video_number", "video_total_views", "video_total_likes", "video_total_dislikes", "video_total_favorites", "video_total_comments", "game_total_critic_reviews", "game_total_user_reviews", "game_type", "game_rating", "game_critic_positive_reviews", "game_critic_mixed_reviews", "game_critic_negative_reviews", "game_user_positive_reviews", "game_user_mixed_reviews", "game_user_negative_reviews", "video_average_views", additional_data_colnames, critic_user_reviews_colnames)




#import csv of links from vgchartz page of data from PC games: http://www.vgchartz.com/platform/48/microsoft-windows/
vgchartz<-read.csv("vgchartz.csv") 

#filter out rows that are missing Links or Anchor.Text
vgchartz_df<-vgchartz %>% filter(nchar(as.character(Link)) > 0) %>% filter(nchar(as.character(Anchor.Text)) > 0)

#vgchartz_df ends with "Bus Terminal Amusements"
#import vgchartz_2.cvs to get the rest of the database
vgchartz_2<- read.csv("vgchartz_2.csv")

#filter out rows that are missing Links or Anchor.Text
vgchartz_2_df<-vgchartz_2 %>% filter(nchar(as.character(Link)) > 0) %>% filter(nchar(as.character(Anchor.Text)) > 0)

#merge vgchartz_df and vgchartz_2_df together
vgchartz_raw_df<- rbind(vgchartz_df, vgchartz_2_df)

#change everything to as.character
vgchartz_raw_df$Link<- as.character(vgchartz_raw_df$Link)
vgchartz_raw_df$Anchor.Text<- as.character(vgchartz_raw_df$Anchor.Text)

num_final_games<- c(1: nrow(final_game_title_df))

vgchartz_links<-c()
vgchartz_game_name<-c()

#find the game title from final_game_title_df in the Anchor.Text in vgchartz_raw_df
#store the result into vgchartz_links
#game titles in final_game_title_df al usuaslly longer and more specific than the game titles from vgchartz dataframe
for(num in num_final_games){
  #create two counters
  n<- c(1)
  c<- c(1)
  
  #control where the game segment ends
  #create a number range from the max number of characters in the game title to 5
  end_segment<- (nchar(final_game_title_df$game_title[num]): 1)
  
  #when a match if found n will increase to 2 and end while loop
  while(n < 2){
    #carve out a segment from the game title
    game_title_subset<- substr(final_game_title_df$game_title[num], start = 1, stop = end_segment[c])
    
    #if there is an exact match between the segment and the game_title from final_game_title_df
    if(sort(stringdist(game_title_subset, vgchartz_raw_df$Anchor.Text))[1] == 0){
      
      match_spot<- grep(" 0 ", paste0(" ", stringdist(game_title_subset, vgchartz_raw_df$Anchor.Text), " "))[1]
      
      #store the Anchor.Text from vgchartz_raw_df to vgchartz_game_name
      vgchartz_game_name<- append(vgchartz_game_name, vgchartz_raw_df$Anchor.Text[match_spot])
      
      #store the Link to vgchartz_links
      vgchartz_links<- append(vgchartz_links, vgchartz_raw_df$Link[match_spot])
      
      n<- n + 1
    }
    
    #if there is an almost exact match between the segment and the game_title from final_game_title_df
    if(sort(stringdist(game_title_subset, vgchartz_raw_df$Anchor.Text))[1] == 1){
      
      match_spot<- grep(" 1 ", paste0(" ", stringdist(game_title_subset, vgchartz_raw_df$Anchor.Text), " "))[1]
      
      #store the Anchor.Text from vgchartz_raw_df to vgchartz_game_name
      vgchartz_game_name<- append(vgchartz_game_name, vgchartz_raw_df$Anchor.Text[match_spot])
      
      #store the Link to vgchartz_links
      vgchartz_links<- append(vgchartz_links, vgchartz_raw_df$Link[grep(" 1 ", paste0(" ", stringdist(game_title_subset, vgchartz_raw_df$Anchor.Text), " "))[1]])
      
      n<- n + 1
      
    }
    if(sort(stringdist(game_title_subset, vgchartz_raw_df$Anchor.Text))[1] > 1){
      #if the game_title_subset doesn't match anything in vgchartz_raw_df$Link increase c by 1
      c<- c+1
    }
    
    #each time game_title_subset lacks a match it will shrink until it has the character length of 5
    #eventually c will reach the maximum size, and there is no Link for the game
    if(c == length(end_segment)){
      
      #store the orginal game_title from final_game_title_df to vgchartz_game_name
      vgchartz_game_name<- append(vgchartz_game_name, final_game_title_df$game_title[num])
      
      #store NA value to vgchartz_links
      vgchartz_links<- append(vgchartz_links, NA)
      n<- 2
    }
  }
  #keep track of which game's links is being searched for
  print(final_game_title_df$game_title[num])
}

#create dataframe from vgchartz_game_name and vgchartz_links
vgchartz_df<- data.frame(vgchartz_game_name, vgchartz_links)

#simplify column names of vgchartz_df
vgchartz_colnames<- c("game_name", "links")

colnames(vgchartz_df) <- vgchartz_colnames

vgchartz_df$game_name<- as.character(vgchartz_df$game_name)
vgchartz_df$links<- as.character(vgchartz_df$links)












#get the global sales from the links in vgchartz_df

num_vgchartz_df<- c(1: nrow(vgchartz_df))

vgchartz_global_sales<-c()

for(num in num_vgchartz_df){
  
  #if there's not a link associated with the game then add NA to vgchartz_global_sales
  if(is.na(vgchartz_df$links[num])){
    vgchartz_global_sales<- append(vgchartz_global_sales, NA)
    
  }else{
    url<- vgchartz_df$links[num]
    
    #format url
    page_data <- read_html(GET(as.character(url), add_headers('user-agent' = 'r')))
    
    #get the global sale from the page
    global_sale<- page_data %>% 
      html_nodes("#total_units td+ td b") %>%
      html_text() %>%
      strsplit(split = "m") %>%
      as.numeric()
    
    #store the value if it exists
    if(length(global_sale) < 1){
      vgchartz_global_sales<- append(vgchartz_global_sales, NA)
    }else{
      vgchartz_global_sales<- append(vgchartz_global_sales, global_sale)
    }
  }
  
  #keep track of which game is being searched for 
  print(as.character(vgchartz_df$game_name[num]))
}

#change vgchartz_global_sales into a data.frame
vgchartz_global_sales_df<- data.frame(vgchartz_global_sales)

#add vgchartz_global_sales to vgchartz_df
vgchartz_df<- cbind(vgchartz_df, vgchartz_global_sales_df)

#add "global_sales" as last column name for vgchartz_df
vgchartz_colnames<- append(vgchartz_colnames, "global_sales")

#update colnames of vgchartz_df
colnames(vgchartz_df)<- vgchartz_colnames

#change data type vgchartz_df
vgchartz_df$global_sales<- as.numeric(as.character(vgchartz_df$global_sales))


#find where links in vgchartz_df are missing
NA_vgchartz<- which(is.na(vgchartz_df$global_sales))

num_NA_vchartz<- c(1: length(NA_vgchartz))

vgchartz_game_name_3<- c()
vgchartz_links_3<- c()
vgchartz_global_sales_3<- c()


for(num in num_NA_vchartz){
  
  #remove and add to game_name as needed
  key_term <- vgchartz_df$game_name[NA_vgchartz[num]]
  key_term <- gsub(pattern = " ", replacement = "+", key_term)
  key_term <- gsub(pattern = ":", replacement = "%3A", key_term)
  key_term <- gsub(pattern = ",", replacement = "%2C", key_term)
  
  
  url<- paste0("http://www.vgchartz.com/gamedb/?name=", key_term, collapse = "")
  
  #format url
  page_data <- read_html(GET(as.character(url), add_headers('user-agent' = 'r')))
  
  #store all data on the page
  game_chart<- page_data %>% 
    html_nodes(".chart td") %>%
    html_text()
  
  
  #some urls return no resuts at all
  if(length(game_chart) == 0){
    vgchartz_game_name_3<- append(vgchartz_game_name_3, vgchartz_df$game_name[NA_vgchartz[num]])
    vgchartz_links_3<- append(vgchartz_links_3, url)
    vgchartz_global_sales_3<- append(vgchartz_global_sales_3, NA)
  }
  
  #if "PC" doesn't appear on the page, store NA instead of sales data
  else if(is.na(table(grepl("PC", game_chart))["TRUE"]) ==TRUE){
    vgchartz_game_name_3<- append(vgchartz_game_name_3, vgchartz_df$game_name[NA_vgchartz[num]])
    vgchartz_links_3<- append(vgchartz_links_3, url)
    vgchartz_global_sales_3<- append(vgchartz_global_sales_3, NA)
    
  }else if(is.na(table(grepl("PC", game_chart))["TRUE"]) == FALSE) {
    
    #find where PC appears in game_chart
    PC_spots<- grep("PC", game_chart)
    
    if(length(PC_spots) > 1){
      num_PC_results<- c(1: length(PC_spots))
      
      #pick out the game name and link and global sales for each PC game on the page and store the values
      for(num_PC in num_PC_results){
        vgchartz_game_name_3<- append(vgchartz_game_name_3, game_chart[(PC_spots[num_PC] - 1)])
        vgchartz_links_3<- append(vgchartz_links_3, url)
        vgchartz_global_sales_3<- append(vgchartz_global_sales_3, game_chart[(PC_spots[num_PC] + 8)])
      }
    }else{
      vgchartz_game_name_3<- append(vgchartz_game_name_3, game_chart[(PC_spots - 1)])
      vgchartz_links_3<- append(vgchartz_links_3, url)
      vgchartz_global_sales_3<- append(vgchartz_global_sales_3, game_chart[(PC_spots + 8)])
    }
  }
  
  #check to make sure nothing is being dropped by comparing lengths of vectors
  if(length(vgchartz_game_name_3) != length(vgchartz_links_3)){
    print("num_NA_vgchartz is")
    print(num)
    break
  }
  
  #add a delay to ensure page information is being downloaded fully  
  Sys.sleep(4)
  
  #keep track of which game is being searched for 
  print(as.character(vgchartz_df$game_name[NA_vgchartz[num]]))
}

#join vgchartz_game_name_2 and vgchartz_global_sales_2 together in a dataframe
vgchartz_df_2 <- data.frame(vgchartz_game_name_3, vgchartz_links_3, vgchartz_global_sales_3)

vgchartz_df_2_colnames<- c("game_name", "link", "global_sales")

colnames(vgchartz_df_2) <- vgchartz_df_2_colnames


#change data type in vgchartz_df_2
vgchartz_df_2$game_name<- as.character(vgchartz_df_2$game_name)
vgchartz_df_2$link<- as.character(vgchartz_df_2$link)
vgchartz_df_2$global_sales<- as.numeric(as.character(vgchartz_df_2$global_sales))







#match games titles from vgchartz_df_2 to vgchartz_df
#in order to fill in empty links in vgchartz_df


for(num in num_NA_vchartz){
  
  #if there is an exact match between the segment and the game_name from vgchartz_df_2
  if(sort(stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name))[1] == 0){
    
    match_spot<- grep(" 0 ", paste0(" ", stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name), " "))[1]
    
    #put global_sales value from vgchartz_df_2 into vgchartz_df
    vgchartz_df$global_sales[NA_vgchartz[num]] <- vgchartz_df_2$global_sales[match_spot]
    
    #put the link from vgchartz_df_2 into vgchartz_df
    vgchartz_df$links[NA_vgchartz[num]] <- vgchartz_df_2$link[match_spot]
    
    n<- n + 1
  }
  
  #if there is an almost exact match between the segment and the game_name from vgchartz_df_2
  else if(sort(stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name ))[1] == 1){
    
    match_spot<- grep(" 1 ", paste0(" ", stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name), " "))[1]
    
    #put global_sales value from vgchartz_df_2 into vgchartz_df
    vgchartz_df$global_sales[NA_vgchartz[num]] <- vgchartz_df_2$global_sales[match_spot]
    
    #put the link from vgchartz_df_2 into vgchartz_df
    vgchartz_df$links[NA_vgchartz[num]] <- vgchartz_df_2$link[match_spot]
    
    n<- n + 1
  }
  #if there is an almost exact match between the segment and the game_name from vgchartz_df_2
  else if(sort(stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name ))[1] == 2){
    
    match_spot<- grep(" 2 ", paste0(" ", stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name), " "))[1]
    
    #put global_sales value from vgchartz_df_2 into vgchartz_df
    vgchartz_df$global_sales[NA_vgchartz[num]] <- vgchartz_df_2$global_sales[match_spot]
    
    #put the link from vgchartz_df_2 into vgchartz_df
    vgchartz_df$links[NA_vgchartz[num]] <- vgchartz_df_2$link[match_spot]
    
    n<- n + 1
  }
  
  else if(sort(stringdist(vgchartz_df$game_name[NA_vgchartz[num]], vgchartz_df_2$game_name ))[1] > 2){
    #if the game_title_subset doesn't match anything in vgchartz_df_2$game_name
    #don't do anything to vgchartz_df
  }
  
  #keep track of which game's links is being searched for
  print(as.character(vgchartz_df$game_name[NA_vgchartz[num]]))
}


#give the games that sales cannot be for one more chance

still_NA_spots<- which(is.na(vgchartz_df$global_sales))
length(still_NA_spots)

still_NA_games<- vgchartz_df$game_name[still_NA_spots]

num_still_NA_games<-c(1:length(still_NA_spots))


#compare game titles to all the game titles collected from individually searching game titles in vgchartz_df_2
for(num in num_still_NA_games){
  
  best_match <- sort(stringdist(still_NA_games[num], vgchartz_df_2$game_name))[1]
  
  best_match_spot<- grep(best_match, stringdist(still_NA_games[num], vgchartz_df_2$game_name))
  
  if(length(best_match_spot) <= 5){
    print("vgchartz_df game name")
    print(vgchartz_df$game_name[still_NA_spots[num]])
    print("game name contenders")
    print(vgchartz_df_2$game_name[best_match_spot])
    print("still_NA_spots")
    print(still_NA_spots[num])
  }
}



#add still_NA_spots of games that have a viable name
useable_games<- c(14, 124, 382, 458)

#store which of the resulting game titles should be used
useable_title_spots<-c(1, 1, 1, 1)

num_useable_games<-c(1: length(useable_games))

for(num in num_useable_games){
  
  best_match <- sort(stringdist(vgchartz_df$game_name[useable_games[num]], vgchartz_df_2$game_name))[1]
  
  best_match_spot<- grep(best_match, stringdist(vgchartz_df$game_name[useable_games[num]], vgchartz_df_2$game_name))[useable_title_spots[num]]
  
  vgchartz_df$global_sales[useable_games[num]]<- vgchartz_df_2$global_sales[best_match_spot]
}


#store dataframe
write_csv(vgchartz_df, "vgchartz_df_final.csv")

#add vgchartz_df to final_game_title_df
final_game_title_df<- cbind(final_game_title_df, vgchartz_df)

#store final dataframe
write_csv(final_game_title_df, "game_soundtrack_df.csv")

