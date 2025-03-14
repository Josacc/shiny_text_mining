library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(scales)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyFeedback)

function(input, output, session) {
  observeEvent(input$info_button_top_ten, {
    show_alert(
      session = session,
      title   = "",
      text    = tags$div(
        tags$h3("Information",
                style = "color: #0076C8; font-weight: bold; text-align: center"),
        tags$br(),
        tags$br(),
        style = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
        tags$b('Top 10 preguntas más observadas', style = "font-weight: bold"),
        'is an application that uses text mining techniques to detect, quantify,
          order and graph statements with the highest incidence that are part of
          the activities in the Censo Nacional de Gobierno INEGI.',
        tags$br()
      ),
      html  = TRUE,
      width = "55%"
    )
  })
  
  
  observeEvent(input$id_top_ten_question, {
    updateTabsetPanel(session, "id_modulo_select", input$id_top_ten_question)
  })
  
  reactive_top_ten_questions <- reactive({
    switch(input$id_top_ten_question,
           CNGE   = switch(input$id_CNGE,
                           `Módulo 1` = top_ten_questions("CNGE", "M1"),
                           `Módulo 2` = top_ten_questions("CNGE", "M2"),
                           `Módulo 3` = top_ten_questions("CNGE", "M3"),
                           `Módulo 4` = top_ten_questions("CNGE", "M4"),
                           `Módulo 5` = top_ten_questions("CNGE", "M5")
           ),
           CNSPE   = switch(input$id_CNSPE,
                            `Módulo 1` = top_ten_questions("CNSPE", "M1"),
                            `Módulo 2` = top_ten_questions("CNSPE", "M2")
           ),
           CNSIPEE = switch(input$id_CNSIPEE,
                            `Módulo 1` = top_ten_questions("CNSIPEE", "M1"),
                            `Módulo 2` = top_ten_questions("CNSIPEE", "M2")
           ),
           CNPJE   = switch(input$id_CNPJE,
                            `Módulo 1` = top_ten_questions("CNPJE", "M1"),
                            `Módulo 2` = top_ten_questions("CNPJE", "M2"),
                            `Módulo 3` = top_ten_questions("CNPJE", "M3"),
                            `Módulo 4` = top_ten_questions("CNPJE", "M4"),
                            `Módulo 5` = top_ten_questions("CNPJE", "M5")
           ),
           CNIJE   = switch(input$id_CNIJE,
                            `Módulo 1` = top_ten_questions("CNIJE", "M1"),
                            `Módulo 2` = top_ten_questions("CNIJE", "M2"),
                            `Módulo 3` = top_ten_questions("CNIJE", "M3"),
                            `Módulo 4` = top_ten_questions("CNIJE", "M4"),
                            `Módulo 5` = top_ten_questions("CNIJE", "M5"),
                            `Módulo 6` = top_ten_questions("CNIJE", "M6"),
                            `Módulo 7` = top_ten_questions("CNIJE", "M7")
           ),
           CNPLE = switch(input$id_CNPLE,
                          `Módulo 1` = top_ten_questions("CNPLE", "M1"),
                          `Módulo 2` = top_ten_questions("CNPLE", "M2")
           ),
           CNDHE = switch(input$id_CNDHE,
                          `Módulo 1` = top_ten_questions("CNDHE", "M1"),
                          `Módulo 2` = top_ten_questions("CNDHE", "M2")
           ),
           CNTAIPPDPE = switch(input$id_CNTAIPPDPE,
                               `Módulo 1` = top_ten_questions("CNTAIPPDPE", "M1"),
                               `Módulo 2` = top_ten_questions("CNTAIPPDPE", "M2"),
                               `Módulo 3` = top_ten_questions("CNTAIPPDPE", "M3")
           )
    )
  })
  
  output$table_top_ten_questions <- renderDataTable({
    validate(need(reactive_top_ten_questions(), ""))
    datatable(
      reactive_top_ten_questions()$dataframe,
      rownames = FALSE,
      filter   = list(position = "top"),
      options  = list(
        pageLength = 5,
        dom        = "ltipr",
        language   = list(
          url = "https://cdn.datatables.net/plug-ins/2.1.8/i18n/es-MX.json"
        )
      )
    )
  })
  
  output$plot_top_ten_questions <- renderPlotly({
    validate(need(reactive_top_ten_questions(), "Sin observaciones"))
    reactive_top_ten_questions()$plot
  })
}
