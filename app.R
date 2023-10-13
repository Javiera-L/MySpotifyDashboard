library(shiny)
library(spotifyr)
library(dplyr)
library(shinythemes)
library(DT)
library(plotly)
source("spotify_functions.R")
source("config.R") # fetch the client_id and secret

shiny::devmode(TRUE)

about_content <- readLines("about_page.txt", warn = FALSE)

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
                      tabPanel("About", 
                               HTML(about_content)),
                      tabPanel("Valence vs. Energy", 
                               conditionalPanel(
                                 condition = "input.artist_selected !== null",
                                 plotlyOutput(outputId = "valence_energy_plot", width="100%")
                               ),
                               conditionalPanel(
                                 condition = "input.artist_selected == null",
                                 "Please select an artist to view this content."
                               )
                      ),
                      tabPanel("Danceability Distribution",
                               conditionalPanel(
                                 condition = "input.artist_selected !== null",
                                 plotlyOutput(outputId = "danceability_density_plot")
                               ),
                               conditionalPanel(
                                 condition = "input.artist_selected == null",
                                 "Please select an artist to view this content."
                               )
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
  selected_track <- reactiveVal(NULL)
  
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
      
      selected_track(NULL)
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
          pageLength = 10
        ),
        colnames = c("Track", "Album")
      )
    }
  })
  
  observeEvent(input$album_tracks_table_rows_selected, {
    # Check if any rows are selected
    #print(isTRUE(nrow(input$album_tracks_table_rows_selected) == 0))
    print(input$album_tracks_table_rows_selected)
    if (length(selected_track()) == 1 &&
        isTRUE(nrow(input$album_tracks_table_rows_selected) == 0)
        ) {
      # Reset the selected track if no rows are selected
      selected_track(NULL)
    } else {
      # Update the selected track when a user clicks on a row in the DataTable
      selected_row <- input$album_tracks_table_rows_selected
      # Get the selected track's data
      selected_track_data <- artist_tracks()[selected_row, ]
      selected_track(selected_track_data)
    }

  })
  
  
  # Create the valence vs. energy plot
  output$valence_energy_plot <- renderPlotly({
    audio_features <- artist_tracks()
    audio_features$energy <- as.numeric(audio_features$energy)
    audio_features$valence <- as.numeric(audio_features$valence)
    if (!is.null(audio_features)) {
      # Create a scatter plot using plotly
      plot <- ggplot(audio_features, aes(x = valence, y = energy)) +
        labs(x = "Valence", y = "Energy") +
        scale_x_continuous(limits = c(0, 1)) +  # Set x-axis limits
        scale_y_continuous(limits = c(0, 1)) +  # Set y-axis limits
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        theme(panel.border = element_blank()) +
  
        # Add vertical and horizontal lines at x = 0.5 and y = 0.5
        annotate("segment", x = 0.5, xend = 0.5, y = 0, yend = 1, color = "black") +
        annotate("segment", x = 0, xend = 1, y = 0.5, yend = 0.5, color = "black") +
        
        # Color the quadrants with legend
        geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0, ymax = 0.5, fill = "Sad"), alpha = 0.2) +
        geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0, ymax = 0.5, fill = "Chill"), alpha = 0.2) +
        geom_rect(aes(xmin = 0, xmax = 0.5, ymin = 0.5, ymax = 1, fill = "Angry"), alpha = 0.2) +
        geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.5, ymax = 1, fill = "Happy"), alpha = 0.2) +
        
        scale_fill_manual(values = c("orange","purple", "darkgreen", "blue")) +  # Define fill colors
        guides(fill = guide_legend(title = "Musical Moods")) + # Customize legend title
        geom_point(aes(text = paste("Song: ", track_name,
                                    "<br>Album: ", album_name,
                                    "<br>Valence: ", valence,
                                    "<br>Energy: ", energy
        ))) 
      
      if (!is.null(selected_track()) && !is.null(selected_track()$track_name)) {
        # Add a point to highlight the selected track
        plot <- plot + geom_point(data = selected_track(), aes(text = paste("Song: ", track_name,
                                                                            "<br>Album: ", album_name,
                                                                            "<br>Valence: ", valence,
                                                                            "<br>Energy: ", energy)), 
                                  color = "red", size = 3)
        
      } else {
        plot <- plot
      }
      
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
        color = "darkgreen", size = 2) +
        labs(x = "Danceability", y = "Density") +
        scale_x_continuous(limits = c(0, 1)) +
        theme_minimal() + 
        theme(panel.grid = element_blank()) +
        theme(panel.border = element_blank())
      
      if (!is.null(selected_track()) && !is.null(selected_track()$track_name)) {
        gg_density <- gg_density +
          geom_point(data = selected_track(),
                     aes(x = selected_track()$danceability, 
                         y = 0, 
                         text = paste("Song: ", selected_track()$track_name, 
                                      "<br>Album: ", selected_track()$album_name,
                                      "<br>Danceability: ", selected_track()$danceability
          )), 
          color = "red", size = 2)
      }
      else {
        gg_density <- gg_density
      }
      ggplotly(gg_density, tooltip = "text")
    } else {
      return(NULL)
    }
  })

  
  
}

shinyApp(ui, server)
