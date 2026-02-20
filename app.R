library(shiny)
library(DBI)
library(RSQLite)

DB_FILE <- "songs.sqlite"

# Initialize DB if it doesn't exist
initDB <- function() {
  con <- dbConnect(SQLite(), DB_FILE)
  if (!dbExistsTable(con, "songs")) {
    dbExecute(con, "
      CREATE TABLE songs (
        id INTEGER PRIMARY KEY,
        Song TEXT,
        BPM INTEGER,
        LastPlayed DATE
      )
    ")
    dbExecute(con, "
      INSERT INTO songs (Song, BPM, LastPlayed) VALUES
      ('Let It Be', 72, NULL),
      ('Bohemian Rhapsody', 144, NULL),
      ('Hotel California', 75, NULL),
      ('Smells Like Teen Spirit', 117, NULL),
      ('Billie Jean', 117, NULL)
    ")
  }
  dbDisconnect(con)
}

# Load all songs
loadSongs <- function() {
  con <- dbConnect(SQLite(), DB_FILE)
  df <- dbGetQuery(con, "SELECT * FROM songs ORDER BY id")
  dbDisconnect(con)
  df$LastPlayed <- as.Date(df$LastPlayed)
  df
}

# Update LastPlayed
updateLastPlayed <- function(id) {
  con <- dbConnect(SQLite(), DB_FILE)
  dbExecute(con,
            "UPDATE songs SET LastPlayed = ? WHERE id = ?",
            params = list(Sys.Date(), id))
  dbDisconnect(con)
}

# Add a new song
addSong <- function(name, bpm) {
  con <- dbConnect(SQLite(), DB_FILE)
  dbExecute(con,
            "INSERT INTO songs (Song, BPM, LastPlayed) VALUES (?, ?, NULL)",
            params = list(name, bpm))
  dbDisconnect(con)
}

initDB()

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #121212; color: white; font-family: sans-serif; }
      .song-row { display: flex; padding: 12px; border-bottom: 1px solid #2a2a2a; align-items: center; }
      .song-name { flex: 3; }
      .bpm { flex: 1; color: #aaa; text-align: center; }
      .date { flex: 2; text-align: center; }
      .play-btn { flex: 1; background-color: #1DB954; border: none; color: white; border-radius: 20px; padding: 6px; cursor: pointer; }
      .play-btn:hover { background-color: #1ed760; }
      .input-row { display: flex; margin-bottom: 10px; align-items: center; }
      .input-row input { margin-right: 10px; padding: 5px; border-radius: 5px; border: none; width: 150px; }
      .add-btn { background-color: #1DB954; color: white; border: none; padding: 6px 12px; border-radius: 20px; cursor: pointer; }
      .add-btn:hover { background-color: #1ed760; }
    "))
  ),
  
  h3("Spotify Song Tracker"),
  
  # Input for adding new songs
  div(class = "input-row",
      textInput("new_song", "Song Name", ""),
      numericInput("new_bpm", "BPM", value = 120, min = 30, max = 300),
      actionButton("add_song_btn", "Add Song", class = "add-btn")
  ),
  
  uiOutput("songs")
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = loadSongs())
  
  # Color gradient function
  getColor <- function(date, all_dates) {
    if (is.na(date)) return("#777777")
    valid <- all_dates[!is.na(all_dates)]
    if (length(valid) <= 1) return("#1DB954")
    min_date <- min(valid)
    max_date <- max(valid)
    if (min_date == max_date) return("#1DB954")
    ratio <- as.numeric(date - min_date) / as.numeric(max_date - min_date)
    ratio <- max(0, min(1, ratio))
    red <- round(255 * (1 - ratio))
    green <- round(185 * ratio + 70)
    rgb(red, green, 80, maxColorValue = 255)
  }
  
  # Render songs
  output$songs <- renderUI({
    rv$data <- loadSongs()
    lapply(seq_len(nrow(rv$data)), function(i) {
      row <- rv$data[i,]
      color <- getColor(row$LastPlayed, rv$data$LastPlayed)
      div(class = "song-row",
          div(class = "song-name", row$Song),
          div(class = "bpm", row$BPM),
          div(class = "date",
              style = paste0("color:", color),
              ifelse(is.na(row$LastPlayed), "Never", as.character(row$LastPlayed))
          ),
          actionButton(paste0("btn_", row$id), "Played Today", class = "play-btn")
      )
    })
  })
  
  # Observe Played Today buttons
  observe({
    lapply(rv$data$id, function(id) {
      observeEvent(input[[paste0("btn_", id)]], {
        updateLastPlayed(id)
        rv$data <- loadSongs()
      }, ignoreInit = TRUE)
    })
  })
  
  # Observe Add Song button
  observeEvent(input$add_song_btn, {
    req(input$new_song)
    req(input$new_bpm)
    addSong(input$new_song, input$new_bpm)
    rv$data <- loadSongs()
    updateTextInput(session, "new_song", value = "")
    updateNumericInput(session, "new_bpm", value = 120)
  })
}

shinyApp(ui, server)
