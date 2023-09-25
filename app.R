library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(dplyr)
library(dbplyr)
library(RPostgres)
# pak::pkg_install("Lchiffon/wordcloud2")
library(wordcloud2)
# library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header = dashboardHeader(
    title = "Portal Berita Indonesia Scraper", 
    titleWidth = "100%", 
    disable = FALSE
  ), 
  sidebar = dashboardSidebar(
    disable = FALSE, 
    pickerInput(
      inputId = "portal",
      # label = "Pilih Portal Berita",
      choices = c("Kompas", "Detik", "Tempo"),
      options = list(title = "Pilih Portal Berita", `actions-box` = TRUE),
      multiple = TRUE,
      width = "100%"
    )
    # actionGroupButtons(inputIds = c("select", "deselect"), 
    #                    labels = c("Pilih Semua", "Hapus Semua"), 
    #                    direction = "horizontal", fullwidth = FALSE),
    # checkboxGroupButtons(
    #   inputId = "portal",
    #   label = "Pilih Portal Berita",
    #   choices = c("Kompas", "Detik", "Tempo"),
    #   checkIcon = list(
    #     yes = tags$i(class = "fa fa-check-square", 
    #                  style = "color: steelblue"),
    #     no = tags$i(class = "fa fa-square-o", 
    #                 style = "color: steelblue")), 
    #   direction = "vertical", justified = FALSE, width = "100%", 
    # )
  ), 
  body = dashboardBody(
    tags$head(
      tags$style('
                 .content-wrapper {
                      background-color: #37475c;
                 }
                 
                 body {
                 background-color: #303c4c;
                 }
                 ')
    ),
    div(style="margin: 10px 15px;", 
        img(src = "https://stat.ipb.ac.id/staf/wp-content/uploads/2021/02/1.png", width = "100%")), 
    # column(5, 
    #        DTOutput("title_table")
    # ), 
    # column(7,
           # wordcloud2Output("wordPlot", height = 700),
    # )
    fluidRow(uiOutput("wordPlotUI"))
  ), 
  title = "Portal Berita Indonesia Scraper", 
  skin = "blue"
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  prh <- eventReactive(input$portal, {
    if(is.null(input$portal)){
      shinyalert(type = "error", text = "Silahkan pilih dahulu portal berita")
      list(prc = NULL)
    } else {
      dbcon <- dbConnect(
        drv = Postgres(), 
        host = "rosie.db.elephantsql.com", 
        port = 5432, 
        dbname = "puxgpqon", 
        user = "puxgpqon", 
        password = "Uzz11fMtsa6FF7HiVlgbsoEMQY7aCU1l"
      )
      
      proc_headlines <- dbcon |> 
        tbl("proc_headlines") |> 
        filter(portal %in% !!input$portal) |> 
        select(portal, word) |> 
        collect()
      
      # headlines <- dbcon |> 
      #   tbl("headlines") |> 
      #   filter(portal %in% !!input$portal) |> 
      #   select(title) |> 
      #   collect()
      
      dbDisconnect(dbcon)
      # list(prc = proc_headlines, ph = headlines)
      list(prc = proc_headlines)
    }
    
  })
  
  
  # output$title_table <- renderDT(
  #   prh()$ph, options = list(lengthChange = FALSE)
  # )
  
  output$wordPlotAll <- renderWordcloud2({
    isolate(input$portal)
    if(is.null(prh()$prc)){
      return(NULL)
    } else {
      prh()$prc |>
        count(word, name = "freq", sort = TRUE) |>
        filter(freq >= 10) |>
        wordcloud2::wordcloud2(shape = 'circle', size = 1/(length(input$portal) + 3))
      
    }
    
  })
  
  output$wordPlotKompas <- renderWordcloud2({
    isolate(input$portal)
    if(is.null(prh()$prc)){
      return(NULL)
    } else {
      prh()$prc |> 
        filter(portal == "Kompas") |> 
        count(word, name = "freq", sort = TRUE) |>
        # filter(freq >= 10) |>
        wordcloud2::wordcloud2(shape = 'circle', size = 1/(length(input$portal)))
    }
  })
  
  output$wordPlotDetik <- renderWordcloud2({
    isolate(input$portal)
    if(is.null(prh()$prc)){
      return(NULL)
    } else {
      prh()$prc |> 
        filter(portal == "Detik") |> 
        count(word, name = "freq", sort = TRUE) |>
        # filter(freq >= 10) |>
        wordcloud2::wordcloud2(shape = 'circle', size = 1/length(input$portal))
    }
  })
  
  output$wordPlotTempo <- renderWordcloud2({
    isolate(input$portal)
    if(is.null(prh()$prc)){
      return(NULL)
    } else {
      prh()$prc |> 
        filter(portal == "Tempo") |> 
        count(word, name = "freq", sort = TRUE) |>
        # filter(freq >= 10) |>
        wordcloud2::wordcloud2(shape = 'circle', size = 1/(length(input$portal)))
    }
  })
  # 
  # output$wordPlotUI <- renderUI({
  #   if(is.null(prh()$prc)){
  #     return(NULL)
  #   } else {
  #     if(length(input$portal) == 1){
  #       tagList(
  #         column(12, 
  #                sprintf("wordPlot%s", input$portal) |> 
  #                  wordcloud2::wordcloud2Output() |> 
  #                  box(title = input$portal, width = "100%")
  #                )
  #       )
  #     } else if(length(input$portal) == 2){
  #       input.portal <- input$portal
  #       tagList(
  #         column(12, 
  #                sprintf("wordPlot%s", input.portal[1]) |> 
  #                  wordcloud2::wordcloud2Output() |> 
  #                  box(title = input.portal[1], width = "100%")
  #         ), 
  #         column(12, 
  #                sprintf("wordPlot%s", input.portal[2]) |> 
  #                  wordcloud2::wordcloud2Output() |> 
  #                  box(title = input.portal[2], width = "100%")
  #         )
  #       )
  #     } else if(length(input$portal) == 3){
  #       input.portal <- input$portal
  #       tagList(
  #         column(12, 
  #                sprintf("wordPlot%s", input.portal[1]) |> 
  #                  wordcloud2::wordcloud2Output() |> 
  #                  box(title = input.portal[1], 
  #                      width = "100%", status = "danger")
  #         ), 
  #         column(12, 
  #                sprintf("wordPlot%s", input.portal[2]) |> 
  #                  wordcloud2::wordcloud2Output() |> 
  #                  box(title = input.portal[2], width = "100%")
  #         ), 
  #         column(12, 
  #                sprintf("wordPlot%s", input.portal[3]) |> 
  #                  wordcloud2::wordcloud2Output() |> 
  #                  box(title = input.portal[3], width = "100%")
  #         )
  #       )
  #     } 
  #   }
  # })
  
  output$wordPlotUI <- renderUI({
    if(is.null(prh()$prc)){
      return(NULL)
    } else {
      if(length(input$portal) == 1){
        tagList(
          column(12, 
                 sprintf("wordPlot%s", input$portal) |> 
                   wordcloud2::wordcloud2Output() |> 
                   box(title = input$portal, width = "100%", 
                       status = "warning", solidHeader = TRUE)
          )
        )
      } else if(length(input$portal) == 2){
        input.portal <- input$portal
        tagList(
          column(6, 
                 sprintf("wordPlot%s", input.portal[1]) |> 
                   wordcloud2::wordcloud2Output() |> 
                   box(title = input.portal[1], width = "100%", 
                       status = "warning", solidHeader = TRUE)
          ), 
          column(6, 
                 sprintf("wordPlot%s", input.portal[2]) |> 
                   wordcloud2::wordcloud2Output() |> 
                   box(title = input.portal[2], width = "100%", 
                       status = "warning", solidHeader = TRUE)
          )
        )
      } else if(length(input$portal) == 3){
        input.portal <- input$portal
        tagList(
          column(4, 
                 sprintf("wordPlot%s", input.portal[1]) |> 
                   wordcloud2::wordcloud2Output() |> 
                   box(title = input.portal[1], width = "100%", 
                       status = "warning", solidHeader = TRUE)
          ), 
          column(4, 
                 sprintf("wordPlot%s", input.portal[2]) |> 
                   wordcloud2::wordcloud2Output() |> 
                   box(title = input.portal[2], width = "100%", 
                       status = "warning", solidHeader = TRUE)
          ), 
          column(4, 
                 sprintf("wordPlot%s", input.portal[3]) |> 
                   wordcloud2::wordcloud2Output() |> 
                   box(title = input.portal[3], width = "100%", 
                       status = "warning", solidHeader = TRUE)
          )
        )
      } 
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
