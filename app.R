library(shiny)
library(DBI)
library(RSQLite)

DB_FILE <- "songs.sqlite"

# Initialize database if needed
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

# Load songs
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
  
  dbExecute(
    con,
    "UPDATE songs SET LastPlayed = ? WHERE id = ?",
    params = list(Sys.Date(), id)
  )
  
  dbDisconnect(con)
}

initDB()

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body { background-color: #121212; color: white; font-family: sans-serif; }

      .song-row {
        display: flex;
        padding: 12px;
        border-bottom: 1px solid #2a2a2a;
        align-items: center;
      }

      .song-name { flex: 3; }
      .bpm { flex: 1; color: #aaa; text-align: center; }
      .date { flex: 2; text-align: center; }

      .play-btn {
        flex: 1;
        background-color: #1DB954;
        border: none;
        color: white;
        border-radius: 20px;
        padding: 6px;
        cursor: pointer;
      }

      .play-btn:hover {
        background-color: #1ed760;
      }
    "))
  ),
  
  h3("Song Tracker"),
  
  uiOutput("songs")
  
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(data = loadSongs())
  
  getColor <- function(date, all_dates) {
    
    # If the date is NA → gray
    if (is.na(date)) return("#777777")
    
    valid_dates <- all_dates[!is.na(all_dates)]
    
    # Only one valid date → green
    if (length(valid_dates) <= 1) return("#1DB954")
    
    min_date <- min(valid_dates)
    max_date <- max(valid_dates)
    
    # If all dates equal → green
    if (min_date == max_date) return("#1DB954")
    
    # Compute ratio safely
    ratio <- as.numeric(date - min_date) / as.numeric(max_date - min_date)
    
    # Clamp ratio between 0 and 1 just in case
    ratio <- max(0, min(1, ratio))
    
    red   <- round(255 * (1 - ratio))
    green <- round(185 * ratio + 70)
    
    # Ensure red/green are numeric and in 0-255
    red   <- max(0, min(255, red))
    green <- max(0, min(255, green))
    blue  <- 80
    
    rgb(red, green, blue, maxColorValue = 255)
  }
  
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
              ifelse(is.na(row$LastPlayed),
                     "Never",
                     as.character(row$LastPlayed))
          ),
          
          actionButton(
            paste0("btn_", row$id),
            "Played Today",
            class = "play-btn"
          )
      )
    })
    
  })
  
  observe({
    
    lapply(rv$data$id, function(id) {
      
      observeEvent(input[[paste0("btn_", id)]], {
        
        updateLastPlayed(id)
        
        rv$data <- loadSongs()
        
      }, ignoreInit = TRUE)
      
    })
    
  })
  
}

shinyApp(ui, server)