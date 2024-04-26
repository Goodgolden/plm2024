
ui <- fluidPage(
  # theme = bslib::bs_theme(version = 5),
  titlePanel("People-like-me methods (PLM)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("individual",
                  "The Patient ID:",
                  choices = unique(tsa_test1$id),
                  width = "100%"),
      numericInput("num",
                   "Matching number for PLM:",
                   10, min = 5, max = 100,
                   step = 1),
      numericInput("tmax",
                   "The max days for prediction:",
                   180, min = 5, max = 180,
                   step = 5),
      textInput("bsk_knots",
                "Brokenstick model knots:",
                value = "10, 20, 30, 40, 50, 60, 80, 100",
                placeholder = "Enter values here and separated by a comma"),
      textInput("anchor_time",
                "Matching anchor time points",
                value = "14, 30, 60, 90, 120",
                placeholder = "Enter values here and separated by a comma"),
      actionButton("run",
                   "Run!",
                   class = "btn-lg btn-success"),
      width = 4),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Euclidean Plot", plotlyOutput("eld_plot")),
        tabPanel("Mahalanobis Plot", plotlyOutput("mhl_plot"))),
      width = 8)
  )
)
