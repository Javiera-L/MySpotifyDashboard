library(shiny)
library(spotifyr)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
source("spotify_functions.R")

client_id <- "8fea6e24930c4aebbca86a6c887b61e9"
client_secret <- "c7df85ba9d2145f18a761b291537b111"

ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel("Spotify Music Analysis"),
                sidebarLayout(
                  sidebarPanel(
                    textInput("artist_input", "Search Artist:"),
                    actionButton("search_button", "Search"),
                    DTOutput("artist_table")  # Display artist names in the sidebar
                  ),
                  mainPanel(
                    tags$style(HTML("
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                     tbody {
                    color: #000000;
                    }

                   "
                                    
                                    
                    )),
                    
                    
                    tabsetPanel(
                      tabPanel("Albums & Tracks", DTOutput("album_tracks_table")),
                      tabPanel("Valence vs. Energy", 
                               plotlyOutput(outputId = "valence_energy_plot"),
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
  
  observeEvent(input$search_button, {
    artist_input <- input$artist_input
    # Search for artists
    artists <- get_artists(artist_input, token)
    
    # Display the list of artists as an interactive DataTable without search bar
    output$artist_table <- renderDT({
      datatable(
        data.frame(Artist = artists$artist_name),
        selection = "single",  # Allow single-row selection
        rownames = FALSE,  # Hide row numbers
        options = list(
          searching = FALSE  # Disable the search bar
        )
      )
    })
    
    # Observe clicks on an artist in the table
    observeEvent(input$artist_table_rows_selected, {
      selected_row <- input$artist_table_rows_selected
      if (length(selected_row) > 0) {
        # Update the selected artist's information when an artist is clicked
        selected_artist(artists[selected_row, , drop = FALSE])
        
        # Fetch and display albums for the selected artist
        albums <- get_albums(selected_artist()$artist_uri, token)
        artist_albums(albums)
        
        # Fetch and display tracks for the selected artist's albums
        tracks <- get_tracks(selected_artist(), albums, token)
        artist_tracks(tracks)
      }
    })
  })
  
  # Display all the tracks for the selected artist's albums
  output$album_tracks_table <- renderDT({
    if (!is.null(artist_tracks())) {
      datatable(
        artist_tracks(),
        rownames = FALSE,
        options = list(
          searching = FALSE  # Disable the search bar
        )
      )
    }
  })
  
  # Create a reactive expression to get audio features for selected tracks
  selected_tracks_audio_features <- reactive({
    cat(' enter audio features')
    print(token)
    print(!is.null(token))
    if (!is.null(selected_artist()) && !is.null(token)) {
      # Get audio features for the selected tracks
      audio_features <- get_tracks_features(artist_tracks(), token)
      return(audio_features)
    }
  })
  
  # Create the valence vs. energy plot
  output$valence_energy_plot <- renderPlotly({
    audio_features <- selected_tracks_audio_features()
    audio_features$energy <- as.numeric(audio_features$energy)
    audio_features$valence <- as.numeric(audio_features$valence)
    print(audio_features)
    print('fsfasfsafsasa')
    if (!is.null(audio_features)) {
      # Create a scatter plot using plotly
      print('hiiiiiii')
      plot <- ggplot(audio_features, aes(x = energy, y = valence)) +
        geom_point() +
        labs(x = "Energy", y = "Valence") 
      
      # Convert the ggplot to a plotly plot
      p <- ggplotly(plot)
      print(p)
    
    }
  })
  
  output$danceability_density_plot <- renderPlotly({
    gg_density <- ggplot(track_features, aes(x = danceability)) +
      geom_density() +
      geom_point(aes(y = 0), color = "blue", size = 2) +
      labs(x = "X Variable", y = "Density") +
      theme_minimal()
    
    # Convert ggplot to plotly
    ggplotly(gg_density)
  })
  
  
}

shinyApp(ui, server)
