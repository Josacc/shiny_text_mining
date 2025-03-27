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

# Auxiliary function
tm_tabPanels <- function(id, censo_name) {
  
  tabPanel(
    title = censo_name,
    selectInput(
      inputId = str_c("id_", censo_name),
      label   = "Módulo",
      choices = str_c("Módulo ", seq(module_count[module_count$Censo == censo_name, ][[2]]))
    )
  )
}

fluidPage(
  titlePanel("Top 10 preguntas más observadas"),
  fluidRow(
    column(
      width = 4,
      h4(
        p(strong("Text mining")),
        style = "color: #3c8dbc"
      )
    ),
    column(
      width = 1,
      actionBttn(
        inputId = "info_button_top_ten",
        label   = "",
        icon    = icon("info-circle"),
        style   = "jelly"
      )
    )
  ),
  br(),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 6,
          selectInput(
            "id_top_ten_question",
            label = "Censo",
            choices = module_count[[1]]
          )
        ),
        column(
          width = 6,
          do.call(
            tabsetPanel,
            c(list(id = "id_modulo_select", type = "hidden"), map(module_count[[1]], ~tm_tabPanels(id, .x)))
          )
          
        )
      )
    ),
    mainPanel(
      width = 12,
      fluidRow(
        column(width = 5,
               plotlyOutput("plot_top_ten_questions")
        ),
        column(width = 7,
               dataTableOutput("table_top_ten_questions")
        )
      )
    )
  )
)
