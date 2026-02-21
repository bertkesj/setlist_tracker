library(shiny)
library(DBI)
library(RPostgres)

# Connect helper
con <- dbConnect(
  RPostgres::Postgres(),
  host = 'ep-divine-sunset-ai4cvzw0-pooler.c-4.us-east-1.aws.neon.tech',
  dbname = 'neondb',
  user = 'neondb_owner',
  password = 'npg_ngHOAKX7o9Bq',
  sslmode = "require"
)


# Initialize table
initDB <- function() {
  #con <- get_con()
  
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS songs (
      id SERIAL PRIMARY KEY,
      song TEXT NOT NULL,
      bpm INTEGER NOT NULL,
      lastplayed DATE
    )
  ")
  
  # Seed only if empty
  count <- dbGetQuery(con, "SELECT COUNT(*) FROM songs")[1,1]
  if (count == 0) {
    dbExecute(con, "
      INSERT INTO songs (song, bpm)
      VALUES
      ('Let It Be', 72),
      ('Bohemian Rhapsody', 144),
      ('Hotel California', 75),
      ('Smells Like Teen Spirit', 117),
      ('Billie Jean', 117)
    ")
  }
  
  #dbDisconnect(con)
}

# Load alphabetically
loadSongs <- function() {
  #con <- get_con()
  df <- dbGetQuery(con, "
    SELECT * FROM songs
    ORDER BY LOWER(song) ASC
  ")
  #dbDisconnect(con)
  df$lastplayed <- as.Date(df$lastplayed)
  df
}

updateLastPlayed <- function(song) {
  #con <- get_con()
  
  dbExecute(con,
            "UPDATE songs
     SET lastplayed = CURRENT_DATE
     WHERE song = $1",
            params = list(song))
  
  #dbDisconnect(con)
}

addSong <- function(song, bpm) {
  #con <- get_con()
  
  dbExecute(con,
            "INSERT INTO songs (song, bpm)
     VALUES ($1, $2)
     ON CONFLICT (song) DO NOTHING",
            params = list(song, bpm))
  
  #dbDisconnect(con)
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
      .add-section { margin-top: 25px; padding-top: 15px; border-top: 1px solid #333; }
      .add-section input { margin-right: 10px; padding: 5px; border-radius: 5px; border: none; }
      .add-btn { background-color: #1DB954; color: white; border: none; padding: 6px 12px; border-radius: 20px; cursor: pointer; }
      .add-btn:hover { background-color: #1ed760; }
    "))
  ),
  
  h3("Spotify Song Tracker"),
  
  uiOutput("songs"),
  
  # ⬇️ Add Song section moved to bottom
  div(class = "add-section",
      h4("Add Song"),
      textInput("new_song", NULL, placeholder = "Song name"),
      numericInput("new_bpm", NULL, value = 120, min = 30, max = 300),
      actionButton("add_song_btn", "Add Song", class = "add-btn")
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = loadSongs())
  
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
  
  output$songs <- renderUI({
    rv$data <- loadSongs()
    
    lapply(seq_len(nrow(rv$data)), function(i) {
      row <- rv$data[i,]
      color <- getColor(row$lastplayed, rv$data$lastplayed)
      
      div(class = "song-row",
          div(class = "song-name", row$song),
          div(class = "bpm", row$bpm),
          div(class = "date",
              style = paste0("color:", color),
              ifelse(is.na(row$lastplayed),
                     "Never",
                     as.character(row$lastplayed))
          ),
          actionButton(
            paste0("btn_", gsub("[^A-Za-z0-9]", "_", row$song)),
            "Played Today",
            class = "play-btn"
          )
      )
    })
  })
  
  observe({
    lapply(rv$data$song, function(song) {
      
      id <- paste0("btn_", gsub("[^A-Za-z0-9]", "_", song))
      
      observeEvent(input[[id]], {
        updateLastPlayed(song)
        rv$data <- loadSongs()
      }, ignoreInit = TRUE)
      
    })
  })
  
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
