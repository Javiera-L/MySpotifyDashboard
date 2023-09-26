library(shiny)
library(spotifyr)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
source("spotify_functions.R")
source("config.R") # fetch the client_id and secret

ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Spotify Music Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("artist_input", "Search Artist:"),
                    actionButton("search_button", "Search"),
                    DTOutput("artist_table"),  # Display artist names in the sidebar
                    h2("Artist's Tracks"),
                    DTOutput("album_tracks_table")
                  ),
                  mainPanel(
                    tags$style(HTML("
                         .dataTables_wrapper {
                    background-color: #ffffff; /* Background color of the entire DataTable */
                    color: #000000; /* Text color */
                  }
                  
                  /* Target the DataTable header */
                  .dataTables_scrollHead {
                    background-color: #007BFF; /* Header background color */
                    color: #ffffff; /* Header text color */
                  }
                  
                  /* Target the DataTable rows */
                  .dataTables_scrollBody tr {
                    background-color: #f2f2f2; /* Row background color */
                    color: #000000; /* Row text color */
                  }

                   "
                                    
                                    
                    )),
                    
                    
                    tabsetPanel(
                      tabPanel("About"),
                      tabPanel("Valence vs. Energy", 
                               plotlyOutput(outputId = "valence_energy_plot")),
                      tabPanel("Danceability Distribution",
                               plotlyOutput(outputId = "danceability_density_plot")
                      )
                    )
                  )
                )
)



server <- function(input, output, session) {
  # Define the token variable outside of observeEvent
  token <- NULL  # Initialize it to NULL
  token <- get_spotify_access_token(client_id, client_secret)
  # Create reactive values to store selected artist's information, albums, and tracks
  selected_artist <- reactiveVal(NULL)
  artist_albums <- reactiveVal(NULL)
  artist_tracks <- reactiveVal(NULL)
  audio_features <- reactiveVal(NULL)
  
  observeEvent(input$search_button, {
    artist_input <- input$artist_input
    # Search for artists
    artists <- get_artists(artist_input, token)[1:3, ]
    
    # Display the list of artists as an interactive DataTable without search bar
    output$artist_table <- renderDT({
      datatable(
        data.frame(Artist = artists$artist_name),
        selection = "single",  # Allow single-row selection
        rownames = FALSE,  # Hide row numbers
        options = list(
          pageLength = 5,
          searching = FALSE,  # Disable the search bar
          info = FALSE,
          paging = FALSE
        )
      )
    })
    
    # Observe clicks on an artist in the table
    observeEvent(input$artist_table_rows_selected, {
      selected_row <- input$artist_table_rows_selected
      if (length(selected_row) == 0) {
        return()  # No selection, nothing to do
      }
      
      # Update the selected artist's information when an artist is clicked
      selected_artist(artists[selected_row, , drop = FALSE])
      
      # Fetch and display albums for the selected artist, if available
      albums <- tryCatch(
        {
          get_albums(selected_artist()$artist_uri, token)
        },
        error = function(e) {
          cat("Error fetching albums:", conditionMessage(e), "\n")
          NULL  # Handle the error by returning NULL
        }
      )
      
      if (!is.null(albums) && nrow(albums) > 0) {
        # Fetch and display tracks for the selected artist's albums
        tracks <- get_tracks(selected_artist(), albums, token) 
        
        # Check if tracks were found and fetch their audio features
        if (!is.null(tracks) && nrow(tracks) > 0) {
          audio_features <- get_tracks_features(tracks, token)
          artist_tracks(audio_features)
        } else {
          # If no tracks are found, set artist_tracks to NULL
          artist_tracks(NULL)
        }
      } else {
        # Fetch and display top tracks with features by the selected artist
        top_tracks <- get_top_tracks_with_features(selected_artist()$artist_uri, token)
        artist_tracks(top_tracks)
      }
    })
  })
  # Display all the tracks for the selected artist's albums
  output$album_tracks_table <- renderDT({
    if (!is.null(artist_tracks())) {
      datatable(
        artist_tracks() %>% select(track_name, album_name),
        rownames = FALSE,
        options = list(
          searching = FALSE,  # Disable the search bar
          info = FALSE,
          lengthChange = FALSE,
          pageLength = 15
        ),
        colnames = c("Track", "Album")
      )
    }
  })
  
  # # Create a reactive expression to get audio features for selected tracks
  # selected_tracks_audio_features <- reactive({
  #   if (!is.null(selected_artist()) && !is.null(token)) {
  #     # Get audio features for the selected tracks
  #     audio_features <- get_tracks_features(artist_tracks(), token)
  #     return(audio_features)
  #   }
  # })
  
  
  # Create the valence vs. energy plot
  output$valence_energy_plot <- renderPlotly({
    audio_features <- artist_tracks()
    print(audio_features)
    audio_features$energy <- as.numeric(audio_features$energy)
    audio_features$valence <- as.numeric(audio_features$valence)
    print(audio_features)
    if (!is.null(audio_features)) {
      # Create a scatter plot using plotly
      plot <- ggplot(audio_features, aes(x = valence, y = energy)) +
        geom_point(aes(text = paste("Song: ", track_name,
                                    "<br>Album: ", album_name,
                                    "<br>Valence: ", valence,
                                    "<br>Energy: ", energy
                                    ))) +
        labs(x = "Valence", y = "Energy") 
      ggplotly(plot, tooltip = "text")
   
    } else {
      return(NULL)  # Return NULL if there is no data
    }
  })
  
  output$danceability_density_plot <- renderPlotly({
    audio_features <- artist_tracks()
    audio_features$danceability <- as.numeric(audio_features$danceability)
    if (!is.null(audio_features)) {
      gg_density <- ggplot(audio_features, aes(x = danceability)) +
        geom_density() +
        geom_point(aes(y = 0, text = paste("Song: ", track_name, 
                                           "<br>Album: ", album_name,
                                           "<br>Danceability: ", danceability
                                           )), 
                   color = "green", size = 2) +
        labs(x = "Danceability", y = "Density") +
        theme_minimal()
      ggplotly(gg_density, tooltip = "text")
    } else {
      return(NULL)
    }
  })
  
  
}

shinyApp(ui, server)
