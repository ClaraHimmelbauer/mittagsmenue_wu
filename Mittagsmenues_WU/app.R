# Load necessary packages
library(shiny)
library(readxl)
library(tidyverse)
library(flextable)
library(gh)

recode_umlaut2 <- function(x){
  y <- gsub("\xe4", "ä", x, useBytes = TRUE)
  y <- gsub("\xf6", "ö", y, useBytes = TRUE)
  y <- gsub("\xfc", "ü", y, useBytes = TRUE)
  y <- gsub("\xdf", "ß", y, useBytes = TRUE)
  y <- gsub("\xc4", "Ä", y, useBytes = TRUE)
  y <- gsub("\xd6", "Ö", y, useBytes = TRUE)
  y <- gsub("\xdc", "Ü", y, useBytes = TRUE)
  y <- gsub("\xc9", "E", y, useBytes = TRUE)
  y <- gsub("\xe9", "é", y, useBytes = TRUE)
  return(y)
}

# Load data
data <- read.csv2("https://raw.githubusercontent.com/ClaraHimmelbauer/mittagsmenue_wu/main/data/menues.csv")
data <- as.data.frame(sapply(data, recode_umlaut2))

# Get metadata: when was file last updated
commits <- gh::gh("/repos/:owner/:repo/commits", owner = "ClaraHimmelbauer", repo = "mittagsmenue_wu", path = "data/menues.csv")
last_modified <- commits[[1]]$commit$committer$date
last_modified <- as.Date(last_modified)

# Mutate data to create the flextable
df <- data %>%
  group_by(Restaurant, Wochentag) %>%
  mutate(id = row_number()) %>%
  ungroup() %>% 
  select(Restaurant, Wochentag, Gericht, id) %>% 
  pivot_wider(names_from = Wochentag, values_from = Gericht) %>% 
  select(Restaurant, Täglich, Montag, Dienstag, Mittwoch, Dönerstag, Freitag)

# Identify vegetarian and vegan cells
veggie <- data %>%
  group_by(Restaurant, Wochentag) %>%
  mutate(id = row_number()) %>%
  ungroup() %>% 
  select(Restaurant, Wochentag, `Vegetarisch.Vegan`, id) %>% 
  pivot_wider(names_from = Wochentag, values_from = `Vegetarisch.Vegan`) %>% 
  select(Restaurant, Täglich, Montag, Dienstag, Mittwoch, Dönerstag, Freitag)

vegetarisch <- which(veggie == "vegetarisch")
vegan <- which(veggie == "vegan")

vegetarisch_j <- ceiling(vegetarisch / nrow(veggie))
vegetarisch_i <- vegetarisch - (vegetarisch_j - 1) * nrow(veggie)
vegan_j <- ceiling(vegan / nrow(veggie))
vegan_i <- vegan - (vegan_j - 1) * nrow(veggie)

# Create flextable
# make flextable
flex <- flextable::flextable(df) %>% 
  flextable::theme_box()

# color vegetarian and vegan cells
for(k in 1:length(vegetarisch_i)){
  flex <- flex %>% 
    color(i = vegetarisch_i[k], j = vegetarisch_j[k], color = "#72bf6a")
}
for(k in 1:length(vegan_i)){
  flex <- flex %>% 
    color(i = vegan_i[k], j = vegan_j[k], color = "#46923c")
}

# borders
x <- cumsum(table(df$Restaurant)[rank(unique(df$Restaurant))])
flex <- flex %>% 
  bold(j = 1) %>% 
  bg(j = 1, bg = "grey") %>% 
  bg(i = 1, part = "header", bg = "grey50") %>% 
  border(border = officer::fp_border(color = "grey"), part = "body") %>% 
  border(border = officer::fp_border(color = "grey50"), part = "header") %>% 
  border(i = x, border.bottom = officer::fp_border(color = "black"), part = "body")

flex <- flex %>% 
  merge_v(j = ~Restaurant + Täglich + Montag + Dienstag+ Mittwoch + Dönerstag + Freitag) %>% 
  fix_border_issues()

# Define UI for the Shiny app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .center-text {
        text-align: center;
      }
      .center-table {
        display: flex;
        justify-content: center;
      }
    "))
  ),
  div(class = "center-text",
      h1("Mittagsmenüs am WU-Campus"),
      p(paste("Zuletzt aktualisiert:", last_modified))
  ),
  div(class = "center-table",
      uiOutput("menue_table")
  ),
  div(class = "center-text",
      a("View the project on github.", href="https://github.com/ClaraHimmelbauer/mittagsmenue_wu/")
  )
)

# Define server logic required to render the flextable
server <- function(input, output) {
  output$menue_table <- renderUI({
    flex  %>% 
      autofit() %>%
      htmltools_value()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
