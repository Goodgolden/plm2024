server <- function(input, output) {

  ## the input parameters from the ui.R ----------------------------------------
  individual <- reactive({individual <- input$individual})
  num <- reactive({num <- input$num})
  anchor_time <- reactive({anchor_time <- input$anchor_time})
  bsk_knots <- reactive({bsk_knots <- input$bsk_knots})
  tmax <- reactive({tmax <- input$tmax})

  observeEvent(input$run, {  })
  val <- reactiveValues()
  
  observeEvent(input$run,{
    val$bsk_knots <- as.numeric(unlist(strsplit(input$bsk_knots, ",")))
    val$anchor_time <- as.numeric(unlist(strsplit(input$anchor_time, ",")))
    val$num <- as.numeric(input$num)
    val$tmax <- as.numeric(input$tmax)
  })
  
  test_data <- reactive({tsa_test1 %>%
    dplyr::filter(id == individual())})
  
  mlmf <- "outcome_score ~ adi_value + adi_value:outcome0 + adi_value:t0 +
                      bmi + bmi:outcome0 + bmi:t0 +
                      patient_age + patient_age:outcome0 + patient_age:t0 +
                      patient_gender + patient_gender:outcome0 + patient_gender:t0 +
                      primary_payer + primary_payer:outcome0 + primary_payer:t0 +
                      surgery_type + surgery_type:outcome0 + surgery_type:t0 +
                      outcome0 + t0"
  
  gf <- "outcome_score ~ cs(time, df = 3)"
  gs <- "~ cs(time, df = 1)"
  
  lmf <- "outcome_score ~ as.factor(time) + 
                      adi_value + adi_value:outcome0 + adi_value:t0 + 
                      bmi + bmi:outcome0 + bmi:t0 +
                      patient_age + patient_age:outcome0 + patient_age:t0 +
                      patient_gender + patient_gender:outcome0 + patient_gender:t0 +
                      primary_payer + primary_payer:outcome0 + primary_payer:t0 +
                      surgery_type + surgery_type:outcome0 + surgery_type:t0 +
                      outcome0 + t0"
  

  ## euclidean fixed matching number -------------------------------------
  output$eld_plot <- renderPlotly({
    validate(need(val$num, ""))
    validate(need(val$anchor_time, ""))
    validate(need(val$bsk_knots, ""))
    validate(need(val$tmax, ""))
    eld_n <- people_like_me(train_data = tsa_train1,
                            test_data= test_data(),
                            outcome_var = "outcome_score",
                            time_var = "time",
                            id_var = "id",
                            tmin = 0,
                            tmax = val$tmax,
                            brokenstick_knots = val$bsk_knots,
                            anchor_time = val$anchor_time,
                            linear_formula = lmf,
                            gamlss_formula = gf,
                            gamlss_sigma = gs,
                            match_methods = "euclidean", 
                            match_number = val$num,
                            weight = FALSE,
                            match_plot = TRUE,
                            predict_plot = TRUE)
    p1 <- eld_n$plot +
      ggtitle("Euclidean Predictive Intervals") +
      xlab("Time") +
      ylab("Outcome Score") +
      xlim(0, val$tmax) +
      ylim(-10, 110)
    p2 <- eld_n$matches +
      xlab("Time") +
      ylab("Outcome Score") +
      ggtitle("Euclidean Matching") +
      xlim(0, val$tmax) +
      ylim(-10, 110)
    
    p <- subplot(p1, p2, nrows = 1,
                 shareY = TRUE) %>%
      layout(xaxis = list(title = "Time"), 
             xaxis2 = list(title = "Time"),
             yaxis = list(title = "Outcome Score"))
    p
  })



  ## mahalanobis fixed matching number -------------------------------------------
  output$mhl_plot <- renderPlotly({
    validate(need(val$num, ""))
    validate(need(val$anchor_time, ""))
    validate(need(val$bsk_knots, ""))
    validate(need(val$tmax, ""))
    mhl_n <- people_like_me(train_data = tsa_train1,
                            test_data = test_data(),
                            outcome_var = "outcome_score",
                            time_var = "time",
                            id_var = "id",
                            tmin = 0,
                            tmax = val$tmax,
                            brokenstick_knots = val$bsk_knots,
                            anchor_time = val$anchor_time,
                            linear_formula = lmf,
                            gamlss_formula = gf,
                            gamlss_sigma = gs,
                            match_methods = "mahalanobis", 
                            match_number = val$num,
                            weight = FALSE,
                            match_plot = TRUE,
                            predict_plot = TRUE)
    p1 <- mhl_n$plot +
      ggtitle("Mahalanobis Predictive Intervals") +
      xlab("Time") +
      ylab("Outcome Score") +
      xlim(0, val$tmax) +
      ylim(-10, 110)
    p2 <- mhl_n$matches +
      xlab("Time") +
      ylab("Outcome Score") +
      ggtitle("Mahalanobis Matching") +
      xlim(0, val$tmax) +
      ylim(-10, 110)
    
    p <- subplot(p1, p2, nrows = 1, 
                 shareY = TRUE)  %>%
      layout(xaxis = list(title = "Time"),
             xaxis2 = list(title = "Time"),
             yaxis = list(title = "Outcome Score"))
    
    p
  })

  
  }

