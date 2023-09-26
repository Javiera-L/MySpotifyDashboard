library(dplyr)
library(httr2)   
library(jsonlite)


get_spotify_access_token <- function(client_id, client_secret) {
  library(httr)
  
  # Prepare the authentication configuration
  auth <- httr::authenticate(user = client_id, password = client_secret)
  
  # Prepare the request body
  body <- list(grant_type = "client_credentials")
  
  # Make the POST request to obtain the access token
  response <- httr::POST(
    url = "https://accounts.spotify.com/api/token",
    config = auth,
    body = body,
    encode = "form"
  )
  
  # Check for a successful HTTP response
  if (httr::http_status(response)$category != "Success") {
    cat("Error: Unable to obtain the access token.\n")
    return(NULL)
  }
  
  # Extract and return the access token
  token_data <- httr::content(response)
  access_token <- token_data$access_token
  
  return(access_token)
}

get_artists <- function(artist_name, token) {
  # Search Spotify API for artist name
  res <- httr::GET(
    url = 'https://api.spotify.com/v1/search',
    query = list(q = artist_name, type = 'artist'),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  
  # Check for a successful HTTP response
  if (httr::http_status(res)$category != "Success") {
    cat("Error: Unable to retrieve data from the Spotify API.\n")
    return(NULL)
  }
  
  # Parse the JSON response
  content <- httr::content(res, as = "parsed")
  
  # Extract artist information
  artist_list <- content$artists$items
  
  # Check if any artists were found
  if (length(artist_list) == 0) {
    cat("No artists found for the given query.\n")
    return(NULL)
  }
  
  # Create a data frame with artist information
  artists <- data.frame(
    artist_name = sapply(artist_list, function(x) x$name),
    artist_uri = sub('spotify:artist:', '', sapply(artist_list, function(x) x$uri)),
    artist_img = sapply(artist_list, function(x) ifelse(length(x$images) > 0, x$images[[1]]$url, NA))
  )
  
  return(artists)
}

get_top_tracks_with_features <- function(artist_id, access_token, country = "US") {
  library(httr)
  
  # Validate input parameters
  if (is.null(artist_id) || is.null(access_token)) {
    cat("Error: Artist ID and access token are required.\n")
    return(NULL)
  }
  
  # Construct the URL for the top tracks endpoint
  url <- paste0("https://api.spotify.com/v1/artists/", artist_id, "/top-tracks")
  
  # Make the GET request to fetch the top tracks
  response <- GET(
    url = url,
    limit = 50,  # Limit the number of tracks for demonstration purposes
    query = list(country = country),
    add_headers(Authorization = paste("Bearer", access_token))
  )
  
  # Check for a successful HTTP response
  if (http_status(response)$category != "Success") {
    cat("Error: Unable to fetch top tracks.\n")
    return(NULL)
  }
  
  # Parse the response content to extract top tracks
  song_data <- content(response)
  songs <- song_data$tracks
  
  # Initialize an empty data frame to store track information
  track_df <- data.frame(
    track_name = character(0),
    album_name = character(0),
    track_uri = character(0),
    Popularity = integer(0),
    Danceability = numeric(0),
    Energy = numeric(0),
    Speechiness = numeric(0),
    Instrumentalness = numeric(0),
    Valence = numeric(0),  # Added Valence column
    Tempo = numeric(0),
    stringsAsFactors = FALSE
  )
  
  # Fetch audio features and other track information for each track
  for (i in 1:length(songs)) {
    track <- songs[[i]]
    track_id <- track$id
    
    audio_response <- GET(
      url = paste0("https://api.spotify.com/v1/audio-features/", track_id),
      add_headers(Authorization = paste("Bearer", access_token))
    )
    
    if (http_status(audio_response)$category != "Success") {
      cat(paste("Error: Unable to fetch audio features for track ", track_id, ".\n", sep = ""))
      # You can choose to continue or break here based on your requirements
      next  # Skip to the next track
    }
    
    audio_data <- content(audio_response)
    
    # Create a new row with track information and audio features
    new_row <- data.frame(
      track_name = track$name,
      album_name = track$album$name,
      track_uri = track$uri,
      popularity = track$popularity,
      danceability = audio_data$danceability,
      energy = audio_data$energy,
      speechiness = audio_data$speechiness,
      instrumentalness = audio_data$instrumentalness,
      valence = audio_data$valence,  # Added Valence column
      tempo = audio_data$tempo,
      stringsAsFactors = FALSE
    )
    
    # Append the new row to the data frame
    track_df <- rbind(track_df, new_row)
  }
  
  return(track_df)
}



get_albums <- function(artist_uri, token) {
  # Make the API request to get albums
  albums <- httr::GET(
    url = paste0('https://api.spotify.com/v1/artists/', artist_uri, '/albums'),
    query = list(access_token = token, market = "US", limit = 50, album_type = "album")
  )
  
  # Check for a successful HTTP response
  if (httr::http_status(albums)$category != "Success") {
    cat("Error: Unable to retrieve albums from the Spotify API.\n")
    return(NULL)
  }
  
  # Parse the JSON response
  content <- httr::content(albums, as = "parsed")
  
  # Check if any albums were found
  if (length(content$items) == 0) {
    cat("Sorry. No albums were found for this artist.\n")
    return(NULL)
  }
  
  # Extract relevant album information
  album_info <- lapply(content$items, function(tmp) {
    if (tmp$album_type == 'album') {
      album_uri <- sub('spotify:album:', '', tmp$uri)
      album_name <- gsub("'", '', tmp$name)
      album_img <- tmp$images[[1]]$url
      
      # Get album release date
      album_release_info <- httr::GET(
        url = paste0('https://api.spotify.com/v1/albums/', album_uri),
        query = list(access_token = token)
      )
      
      if (httr::http_status(album_release_info)$category == "Success") {
        release_date <- httr::content(album_release_info, as = "parsed")$release_date
        album_release_year <- ifelse(nchar(release_date) == 4, as.integer(substr(release_date, 1, 4)), NA)
      } else {
        release_date <- NA
        album_release_year <- NA
      }
      
      return(data.frame(
        album_uri = album_uri,
        album_name = album_name,
        album_img = album_img,
        album_release_date = release_date,
        album_release_year = album_release_year,
        stringsAsFactors = FALSE
      ))
    } else {
      return(NULL)
    }
  })
  
  # Filter and arrange the album information
  album_df <- do.call(rbind, album_info)
  album_df <- album_df[!duplicated(tolower(album_df$album_name)), ]  # Remove duplicates based on album name
  album_df <- album_df[order(album_df$album_release_year), ]  # Arrange by release year
  
  return(album_df)
}


get_tracks <- function(artist_info, album_info, token) {
  track_info <- data.frame()  # Initialize an empty data frame to store track information
  
  # Loop through each album in album_info
  for (x in album_info$album_uri) {
    # Make the API request to get tracks for the album
    tracks <- httr::GET(
      url = paste0('https://api.spotify.com/v1/albums/', x, '/tracks'),
      query = list(access_token = token)
    )
    
    # Check for a successful HTTP response
    if (httr::http_status(tracks)$category == "Success") {
      # Parse the JSON response
      tracks_content <- httr::content(tracks, as = "parsed")
      
      # Create a data frame with track information
      df <- data.frame(
        album_uri = x,
        track_uri = sapply(tracks_content$items, function(track) gsub('spotify:track:', '', track$uri)),
        track_name = sapply(tracks_content$items, function(track) track$name),
        track_number = 1:length(tracks_content$items),  # Assign track numbers based on position
        stringsAsFactors = FALSE
      )
      
      # Join with album_info and artist_info
      df <- dplyr::left_join(df, album_info, by = 'album_uri') %>%
        dplyr::mutate(
          artist_img = artist_info$artist_img,
          artist_name = artist_info$artist_name
        )
      
      # Append the track information to track_info
      track_info <- dplyr::bind_rows(track_info, df)
    }
  }
  
  # Clean and format the resulting data frame
  track_info <- track_info %>%
    dplyr::mutate_at(
      c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'),
      as.character
    ) %>%
    dplyr::mutate_at(
      c('track_number'),
      function(x) as.numeric(x)
    )
  
  return(track_info)
}



# Function to get playlists
get_playlists <- function(playlist_name, token) {
  # Search Spotify API for playlists
  res <- httr::GET(
    url = 'https://api.spotify.com/v1/search',
    query = list(q = playlist_name, type = 'playlist'),
    httr::add_headers(Authorization = paste("Bearer", token))
  )
  
  # Check for a successful HTTP response
  if (httr::http_status(res)$category != "Success") {
    cat("Error: Unable to retrieve playlists from the Spotify API.\n")
    return(NULL)
  }
  
  # Parse the JSON response
  content <- httr::content(res, as = "parsed")
  print(content)
  # Extract playlist information
  playlists <- lapply(content$playlists$items, function(item) {
    list(
      playlist_id = item$id,
      user_id = sub('spotify:user:', '', item$owner$id),  # Remove meta info from the uri string
      playlist_name = item$name
    )
  })
  
  # Convert the list of playlists to a data frame
  playlists_df <- do.call(rbind.data.frame, playlists)
  return(playlists_df)
}

# Function to get tracks from playlists
get_playlists_tracks <- function(playlist_list, token) {
  # Initialize an empty data frame to store track information
  tracks_info <- data.frame()
  
  # Loop through each playlist in playlist_list
  for (i in seq_len(nrow(playlist_list))) {
    # Make the API request to get tracks for the playlist
    tracks <- httr::GET(
      url = paste0('https://api.spotify.com/v1/users/', playlist_list[i, 2], '/playlists/', playlist_list[i, 1], '/tracks'),
      query = list(access_token = token)
    )
    
    # Check for a successful HTTP response
    if (httr::http_status(tracks)$category == "Success") {
      # Parse the JSON response
      tracks_content <- httr::content(tracks, as = "parsed")
      
      # Extract track information
      track_info <- lapply(tracks_content$items, function(item) {
        list(
          track_name = item$track$name,
          artist_name = item$track$artists[[1]]$name,
          track_id = item$track$id
        )
      })
      
      # Convert the list of track information to a data frame
      track_info_df <- do.call(rbind.data.frame, track_info)
      
      # Append the track information to tracks_info
      tracks_info <- dplyr::bind_rows(tracks_info, track_info_df)
    }
  }
  
  return(tracks_info)
}
library(future)
library(promises)
# Function to get audio features for a batch of track IDs
get_audio_features_batch <- function(batch_track_ids, token) {
  # Make the API request to get audio features for tracks in the batch
  features <- httr::GET(
    url = paste0('https://api.spotify.com/v1/audio-features/?ids=', paste(batch_track_ids, collapse = ",")),
    query = list(access_token = token)
  )
  
  # Check for a successful HTTP response
  if (httr::http_status(features)$category != "Success") {
    cat("Error: Unable to retrieve audio features from the Spotify API.\n")
    return(NULL)
  }
  
  # Parse the JSON response
  features_content <- httr::content(features, as = "parsed")
  
  # Extract audio features and clean the data
  features_df <- do.call(rbind.data.frame, lapply(features_content$audio_features, function(feature) {
    if (!is.null(feature)) {
      feature[is.na(feature)] <- NA  # Replace null values with NA
      as.data.frame(t(feature), stringsAsFactors = FALSE)
    } else {
      NULL
    }
  }))
  
  return(features_df)
}

# Function to get features for tracks
get_tracks_features <- function(tracks, token) {
  # Extract track IDs from the tracks data frame
  track_ids <- tracks$track_uri
  
  # Define batch size (adjust as needed)
  batch_size <- 50
  
  # Split track IDs into batches
  track_id_batches <- split(track_ids, ceiling(seq_along(track_ids) / batch_size))
  
  # Initialize a list to store the results
  audio_features_list <- list()
  
  for (batch_track_ids in track_id_batches) {
    audio_features_list[[length(audio_features_list) + 1]] <- get_audio_features_batch(batch_track_ids, token)
  }
  
  print(tracks)
  # Combine the audio features from all batches
  audio_features_df <- do.call(rbind.data.frame, audio_features_list)
  # Convert all numerical columns to type numeric
  numeric_columns <- sapply(audio_features_df, is.numeric)
  audio_features_df[numeric_columns] <- lapply(audio_features_df[numeric_columns], as.numeric)
  audio_features_df$energy <- as.numeric(audio_features_df$energy)
  audio_features_df$valence <- as.numeric(audio_features_df$valence)
  audio_features_df$danceability <- as.numeric(audio_features_df$danceability)
  #audio_features_df$id <- as.numeric(audio_features_df$id)
  #tracks$track_uri <- as.numeric(tracks$track_uri)
  print(audio_features_df)
  result <- merge(tracks, audio_features_df, 
                  by.x = "track_uri", by.y = "id", all = FALSE)
  return(result)
}
