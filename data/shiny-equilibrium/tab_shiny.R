## More Detailed Shiny adapted from Camilla Novaglio's example ----

tab_shiny<-function(sim, df_param=groups, theta=inter, kappa,r_pp){
  
  library(shiny)
  library(rhandsontable)
  
  
  # Specify the df_param columns you want to show
  df_param<-merge(df_param, sim@params@species_params)
  df_param<-df_param[order(df_param$w_inf),]
  
  # species 
  toKeep_sp<-c("species","w_inf","w_mat","beta","sigma","k_vb","r_max", "e_repro")
  empty_dat_spp = df_param[,which(colnames(df_param) %in% toKeep_sp)]
  rownames(empty_dat_spp)<-seq(1:12)
  empty_dat_spp<-empty_dat_spp[,toKeep_sp]
  
  # flee/effort to be transformed  
  # toKeep_fl<-c("species", "gear","catchability") # "sel_func","l25","l50","a","b", 
  # empty_dat_fl = df_param[,which(colnames(df_param) %in% toKeep_fl)]
  # # need to match these changes below 
  # colnames(empty_dat_fl)<-c("target","fleet","catchability") # "selectivity","l25","l50","a","b",
  # rownames(empty_dat_fl)<-seq(1:12)
  
  # ### ui function
  
  # check for formats https://shiny.rstudio.com/tutorial/written-tutorial/lesson2/
  ui = navbarPage("Southern Ocean Mizer",
                  tabPanel("Parameters",
                           # titlePanel("Instructions"),
                           sidebarLayout(
                             sidebarPanel(
                               strong("Instructions"),
                               p("1) click on",em("Update run"), "in Parameters tab; 2) click on",em("Community plots"),"tab; 3) wait!"),
                               strong("Parameters description:"),
                               p(em("w_inf"), "= max weight"),
                               p(em("w_mat"), "= weight at maturity"),
                               p(em("beta"),"= predator-pray mass ratio"),
                               p(em("sigma"), "= width of the prey selection function"),
                               p(em("k_vb"), "= von Bertalanffy K"),
                               p(em("r_max"), "= max recruitment"),
                               p(em("e_repro"), "= max recruitment"),
                                 strong("Calibration"), 
                               p("1) change kappa to scale feeding level; 2)change r_max to scale species abundance; 3) refer to comparison of observed biomass in biomass plots to see if changes are working"),
                               actionButton("go", "Update run"),
                               downloadButton('downloadData', 'Download data')),
                             mainPanel(
                               rHandsontableOutput('contents')))), # data format to be dynamic
                  # rHandsontableOutput('contents_fl')))), # do the same as above byt for effort 
                  tabPanel("Community plots", 
                           fluidRow(
                             column(10,
                                    p("Community plot"),
                                    plotOutput("plot1"))),
                        tabPanel("Biomass plots",
                           fluidRow(
                             column(10,
                                    p("Biomass by species"),
                                    plotOutput("plot2")))
                         ))
                  
  )
  
  
  # server function
  server = function(input, output) {
    
    # add this code if you'd like to upload df_param 
    # indat <- reactive({
    #   inFile <- input$file1
    #   if (is.null(inFile))
    #     return(rhandsontable(empty_dat_spp)) 
    #   raw_input = read.csv(inFile$datapath, header=T)
    #   return(rhandsontable(raw_input))
    # })
    # output$contents <- renderRHandsontable({
    #   indat()  
    # })
    
    output$contents <- renderRHandsontable({
      rhandsontable(empty_dat_spp) # call the dataset you need 
    })
    
    # this becomes for effort... 
    # output$contents_fl <- renderRHandsontable({
    #   rhandsontable(empty_dat_fl) # call the dataset you need 
    # })
    
    portfoliovals <- eventReactive(input$go, { # reactive run only if go/update plot button 
      live_data = hot_to_r(input$contents) # transform reactive data back to R data 
      # live_data_fl = hot_to_r(input$contents_fl) # this is for effort 
      
      live_data<-cbind(live_data, df_param)
      # live_data_fl .... cbind with effort 
      
      # run the model 
      params <- newMultispeciesParams(live_data, interaction = theta, kappa = kappa)
      
      sim <- project(params, effort = 0, t_max = 100,dt=0.1)
      
      return(list(sim = sim, df_param = live_data)) 
      
    })
    
    
    # define output plot
    output$plot1 <- renderPlot({
      plot(portfoliovals()$sim)
    })
    
    # define output plot
    output$plot3 <- renderPlot({
      plotBioData(portfoliovals()$sim,dat)
    })
    
    # download updated dataset 
    # need to add a button to downloaed effort data 
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste("df_param_new", ".csv", sep="") # Sys.Date(),
      },
      content = function(file) {
        write.csv(portfoliovals()$df_param, file)
      })
    
  }
  
  shinyApp(ui = ui, server = server)
  
}

