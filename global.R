# Text mining ----------------------------------------------------
library(tidyverse)
library(readxl)
library(plotly)
library(scales)
library(DT)

# Database on questionnaires (update every year!).
questionnaires <- tibble(
  Cuestionarios = c(
    "1101", "1102", "1103", "1104", "1105", "1106", "1107",
    "1108", "1109", "1110", "1111", "1201", "1301", "1401",
    "2101", "2201",
    "3101", "3201",
    "4101", "4201", "4301", "4401", "4501",
    "5101", "5201", "5301", "5401", "5501", "5601", "5701",
    "6101", "6201",
    "7101", "7201",
    "8101", "8201",  "8301"
  )
)

# Module count by census.
module_count <- questionnaires %>%
  transmute(Cuestionario = str_sub(Cuestionarios, 1, 2)) %>%
  transmute(Cuestionario = as.double(Cuestionario)) %>%
  count(Cuestionario) %>%
  transmute(Censo = Cuestionario %/% 10) %>%
  count(Censo, name = "n_modulos") %>%
  mutate(Censo = str_replace_all(Censo,
                                 c("1" = "CNGE",
                                   "2" = "CNSPE",
                                   "3" = "CNSIPEE",
                                   "4" = "CNPJE",
                                   "5" = "CNIJE",
                                   "6" = "CNPLE",
                                   "7" = "CNDHE",
                                   "8" = "CNTAIPPDPE")))

# Plot function top ten questions by project and module
top_ten_questions <- function(project, module) {
  
  filter_observations <- function(x) {
    
    d <- .data %>%
      filter(str_detect(Observación, x)) %>%
      transmute(Observaciones = str_extract_all(Observación, regex(str_c(x, ".*"), multiline = TRUE))) %>%
      map(unlist) %>%
      as_tibble() %>%
      mutate(first_word = word(Observaciones, 1)) %>%
      filter(!str_detect(first_word, str_c(x, "\\d"))) %>%
      select(1)
    
    return(d)
  }
  
  .data <- read_xlsx("www/historial.xlsx") %>%
    filter(`Cantidad de obs` > 0) %>%
    filter(Censo == project) %>%
    mutate(Módulo = str_replace_all(str_sub(Folio, 4, 4),
                                    c("1" = "M1",
                                      "2" = "M2",
                                      "3" = "M3",
                                      "4" = "M4",
                                      "5" = "M5",
                                      "6" = "M6",
                                      "7" = "M7"))) %>%
    filter(Módulo == module) %>%
    transmute(Observación, Pregunta = str_extract_all(Observación, "(P\\d+\\.\\d+)|(Complemento\\s\\d+)|(Anexo\\s\\d+)"))
  
  if (nrow(.data) == 0) {
    return(NULL)
  }
  
  topten <- .data %>%
    select(2) %>%
    map(unlist) %>%
    as_tibble() %>%
    count(Pregunta) %>%
    rename(`Cantidad de obs` = names(.)[2]) %>%
    arrange(desc(`Cantidad de obs`)) %>%
    head(10)
  
  topten_observations <- map(topten[[1]], filter_observations) %>%
    reduce(full_join, "Observaciones")
  
  .plot <- topten %>%
    mutate(Pregunta = fct_inorder(factor(Pregunta))) %>%
    ggplot(aes(Pregunta, `Cantidad de obs`)) +
    geom_col(width = .7, fill = "#a71106") +
    ggtitle(label = str_c(module, project, sep = " ")) +
    labs(y = "Cantidad de observaciones") +
    theme_classic() +
    theme(
      axis.text.x  = element_text(angle = 30, hjust = 1, size = 8),
      axis.title.x = element_blank()
    )
  
  return(list(dataframe = topten_observations, plot = ggplotly(.plot)))
}
