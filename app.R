#' Shiny RCI Main File
   

#library(devtools)
#install_github("philchalmers/mirt")
#packageVersion("mirt")

# So that it's using the CRAN version not the GitHub version 
#remove.packages("mirt")
#install.packages("mirt") 


# Test Run Locally 
# shinylive::assets_info()
# shinylive::assets_ensure()
# shinylive::export(".", "local_export") 

# remotes::install_github("philchalmers/mirt@v1.44.0")
# packageDescription("mirt")[c("RemoteType","RemoteRef","RemoteSha","Version")]
# remotes::install_github("philchalmers/SimDesign@v2.20.0")
# packageDescription("SimDesign")[c("RemoteType","RemoteRef","RemoteSha","Version")]


setwd("~/Desktop/Shiny_RCI")

# Load R packages
library(shiny)
library(shinythemes)
library(mirt)
library(ggplot2)
source("test_model.R")




## Store models & descriptions in a named list
models <- list(
  "BDI-II" = list(
    model = BDI2mod,
    description = "Beck Depression Inventory-II (BDI-II): A 21-item, self-report inventory measuring the severity of depression. 
    Each item is a 4-point scale ranging from 0 to 3. Total sum scores range from 0 to 63. 
    The item paramters for model calibraton were based on de Sá Junior et al. (2019)'s paper.
    The simulated model used 5000 response vectors, estimated item parameters were measured by RMSD,
     where all item parameters RMSD were <0.1 (except for the 3rd threshold parameter, RMSD = 0.12).",
    item_names = BDI2item_names,
    max_score = 21*3,
    jitems = 21
  ),
  "PHQ-9" = list(
    model = PHQ9mod,
    description = "Patient Health Questionnaire-9 (PHQ-9): A 9-item depression severity measure. 
    Each item is a 4-point scale ranging from 0 to 3. Total sum scores range from 0 to 27.
    The item paramters for model calibraton were based on LaLonde et al. (2025)'s paper.
    The simulated model used 5000 response vectors, estimated item parameters were measured by RMSD, 
    where all item parameters RMSD were <0.06.",
    item_names = PHQ9item_names,
    max_score = 9*3,
    jitems = 9
  ),
  "GAD-7" = list(
    model = GAD7mod,
    description = "Generalized Anxiety Disorder-7 (GAD-7): A 7-item screening tool for generalized anxiety disorder.
    Each item is a 4-point scale ranging from 0 to 3. Total sum scores range from 0 to 21.
    The item paramters for model calibraton were based on Schalet et al. (2015)'s paper.
    The simulated model used 5000 response vectors, estimated item parameters were measured by RMSD, 
    where all item parameters RMSD were <0.08).",
    item_names = GAD7item_names,
    max_score = 7*3,
    jitems = 7
  )
)


# ui
ui <- fluidPage(
  navbarPage("RCI Calculator",
             
             tabPanel("Results",
                      sidebarLayout(
                        
                        sidebarPanel(
                          p("To calculate the Reliable Change Index (RCI), please provide pretest and posttest information."),
                          
                          # Dropdown menu for model type: JT or IRT
                          selectInput(inputId = "model", label = "Model Type",
                                      choices = c("CTT-RCI" = "JTRCI", "IRT-RCI" = "IRTRCI")
                                      ),
                          
                          
                          # Conditional panel for JT model - shows a box for SEM only when CTT-RCI is selected
                          conditionalPanel(
                            condition = "input.model == 'JTRCI'",
                            hr(),
                            numericInput(inputId = "rxx", "Reliability", value = 0.9),
                            numericInput(inputId = "SD", "Standard deviation (SD)", value = 2.5) # placeholder
                            # selectInput(inputId = "alpha", label = "Critical Value",
                                        # choices = c("α = 0.05" = "0.05", "α = 0.10" = "0.10"))
                          ), # conditionalPanel()
                          
                          
                          
                          # Conditional panel for IRT model - shows extra dropdown only when IRT-RCI is selected
                          conditionalPanel(
                            condition = "input.model == 'IRTRCI'",
                            hr(),
                            selectInput(inputId = "model_choice", label = "Choose Calibrated Model",
                                        choices = names(models)),
                            
                            selectInput(inputId = "estmethod", label = "Theta Estimation method",
                                        choices = c("EAP for sum scores" = "EAPsum", 
                                                    "EAP" = "EAP", 
                                                    "MAP" = "MAP", 
                                                    "WML" = "WLE", 
                                                    "ML" = "ML"))
                          ), # conditionalPanel()
                          
                          # Fisher info 
                          conditionalPanel(condition = "input.estmethod != 'EAPsum'",
                                                  checkboxInput(inputId = "fisher", 'Fisher Information for SE?', value = FALSE)
                          ), # conditionalPanel()
                          hr(),
                          
                          # For pretest or prevec values 
                          conditionalPanel(
                            condition = "input.model == 'JTRCI' || input.estmethod == 'EAPsum'",  # JavaScript uses || for "OR" (not R’s |)
                            numericInput('pretest', 'Pretest sum-score:', value=NA),
                            numericInput('posttest', 'Posttest sum-score:', value=NA)
                          ), # conditionalPanel() 
                          
                          conditionalPanel(condition = "input.model != 'JTRCI' && input.estmethod != 'EAPsum'",
                                           textInput('prevec', 'Pretest response vector (e.g., 1011 ...):',
                                                            value=""),
                                           textInput('postvec', 'Posttest response vector (e.g., 1111 ...):',
                                                            value="")
                          ), # conditionalPanel() 
                          
                          
                          
                          
                          actionButton(inputId = "computebutton",
                                       label = "Compute RCI",
                                       class = "btn btn-primary")
                          
                        ),  # sidebarPanel()
                        
                        
                        
                        mainPanel(
                          
                          conditionalPanel(condition = "input.computebutton == 0",
                                           div(imageOutput("meme"),
                                           align = "center",
                                           h4("Enter pre & post scores to see whether the individual actually changed!"))),
                          
                          conditionalPanel(condition = "input.computebutton > 0",
                                           
                          # output 
                          uiOutput(outputId = "model_des"),    ## For calibrated model description
                          
                          uiOutput(outputId = "itemNames"),       ## Display collapsible item names
                          hr(),
                          
                          titlePanel("RCI Output Information"),
                          textOutput(outputId = "RCstatus"),        ## Output RC status (ie. reliable change, no change)
                          textOutput(outputId = "results"),         ## summary text
                          hr(),
                          tableOutput(outputId = "RCtable"),        ## for indv. numeric output
                          uiOutput(outputId = "LookUpTitle"),      ## For lookup table's title
                          tableOutput(outputId = "LookUp"),        ## For lookup table (CTT & EApsum)
                          plotOutput(outputId = "rci_plot")       ## CTT plot
                          
                          ) # conditionalPanel()
                     
                        ) # mainPanel()
                        
                      ) # sidebarLayout()
                      ), # tabPanel() — "Results"
             
             
             
             tabPanel("Results Explained",
                      div(includeMarkdown("results.md"))),
  
             
             tabPanel("Computation Explanation",
                      withMathJax(includeMarkdown("comp.md"))),
             
             tabPanel("Background Info",
                      withMathJax(includeMarkdown("backinfo.md"))),
             
             tabPanel("Citation",
                      div(includeMarkdown("citation.md")))
             # can also adjust the style in the text by -> p(" ", style = "color: blue; text-align: center;"))
                      
                      
             ) # navbarPage()
) # fluidPage()









# server 
server <- function(input, output){
  
  RCIresults <- reactiveValues(status = NULL, summary_text = NULL, lookup = NULL, table_data = NULL, rciplot = NULL)
  
  SEM <- reactive({(input$SD * sqrt(1 - input$rxx))}) # SEM is now a reactive function, not a number, so need to call it as SEM()
  scores <- reactive({RCI(predat = input$pretest, postdat = input$posttest, SEM.pre= SEM())})
  
  
  # This part responds instantly to the dropdown choice, without waiting for "Compute" button
  output$model_des <- renderUI({
    req(input$model == "IRTRCI", input$model_choice) # Only runs for IRT
    
    # Get the description from our list
    description <- models[[input$model_choice]]$description
    
    # Format it with HTML tags
    tagList(
      h5(paste("Description for:", input$model_choice)), # the header
      p(description)    # the description below
    )
  }) # renderUI()
  
  
  # Display Collapsible Item Name 
  output$itemNames <- renderUI({
    # Only show this for IRT models
    req(input$model == "IRTRCI", input$model_choice)
    
    item_names <- models[[input$model_choice]]$item_names
    
    tags$details(
      # Style the summary to look like a button using bootstrap classes btn
      tags$summary("Show item names", class = "btn btn-info btn-sm"),
      
      # Use a styled <ol> to remove default list numbering and padding
      tags$ol(
        style = "list-style-type: none; padding-left: 0;", 
        lapply(item_names, tags$li))
    )
  }) # renderUI()

  
  # The meme
  output$meme <- renderImage({
    list(src = "www/meme.png", contentType = "image/png", width = "95%")
  }, deleteFile = FALSE)
  
  
  
 # Makes sure to clear the output the moment model type is switched  
  observeEvent(c(input$model, input$model_choice, input$estmethod), {
    # This event triggers whenever the model type (CTT/IRT) is changed.
    # Clear all previous results immediately.
    RCIresults$status <- NULL
    RCIresults$summary_text <- NULL
    RCIresults$lookup <- NULL
    RCIresults$table_data <- NULL
    RCIresults$rciplot <- NULL
  })
  

  # Compute when button is clicked - compute the things in the {} only when the input ID "computebutton" is triggered
  observeEvent(input$computebutton, {
    
    # Reset previous results
    RCIresults$status <- NULL
    RCIresults$summary_text <- NULL
    RCIresults$lookup <- NULL
    RCIresults$table_data <- NULL
    RCIresults$rciplot <- NULL
    
    # --- CTT-based RCI ---
    if (input$model == "JTRCI"){
      
      req(input$pretest, input$posttest, is.numeric(input$pretest), is.numeric(input$posttest))
      
      SEM <- SEM()
      scores <- scores()
      
      # 1. Status: chnaged or not
      RCIresults$status <- if (scores$p <.05){"Change Status: Reliably Changed (p < .05)"}
      else {"Change Status: Did Not Reliably Change (p >= .05)"}
      
      # 2. Summary text w/ RCI cutoff & its RCI score
      #results <- scores$z
      results <- abs((input$posttest - input$pretest))/(sqrt(2)*SEM) # the same as the z score
      RCcutoff <- 1.96*sqrt(2)*SEM
      RCIresults$summary_text <- paste0(
        "The individual's RCI value is ±", round(results, 2), 
        ". The individual must improve/decline by at least ", round(RCcutoff, 2), 
        " points for the change to be considered statistically significant, with α =.05."
      ) # paste0()
      
      # 4. Look-up Table
      diffsc <- -10:0
      presc <- 0
      postsc <- presc - diffsc
      # For each value x in post_scores, apply this function, and return a list of results.
      results_table <- lapply(postsc, \(x) {
        R <- RCI(predat = presc, postdat = x, SEM.pre = SEM)
        data.frame(Difference = x - presc,
                   Z = round(R$z, 2),
                   P = signif(R$p, 3))
      })
      
      # Since "results" is a list of 10 dataframes (one per post score),
      #   this combines all dataframes into one table (a single dataframe with 10 rows)
      RCIresults$lookup <- do.call(rbind, results_table)
      
      
      # 5. Plot
        
        # Need to feed dataframe for ggplot 
        df <- data.frame(pre = input$pretest, post = input$posttest)
        
        # Find the min/max of the actual scores to establish a base range
        min_score <- min(input$pretest, input$posttest)
        max_score <- max(input$pretest, input$posttest)
        
        # Decide to add 2.5 SD around the data points as a buffer
        plot_range <- 2.5 * input$SD
        min_axis <- floor(min_score - plot_range)
        max_axis <- ceiling(max_score + plot_range)
        
        # Allows the label to be placed a quarter of the total range 
        offset <- (max_axis - min_axis) * 0.25   # (max-min) is the total width/height of the plot
        
        # Coordinates for "Significant Deterioration" (top-left triangle)
        x_det <- min_axis + offset
        y_det <- max_axis - offset
        
        # Coordinates for "Significant Improvement" (bottom-right triangle)
        x_imp <- max_axis - offset
        y_imp <- min_axis + offset
        
        
        RCIresults$rciplot <- ggplot(df, aes(x = pre, y = post)) +
          
          # Add the confidence band using geom_ribbon
          # create a dummy data frame for the ribbon's x-coordinates
          geom_ribbon(
            data = data.frame(x = c(min_axis, max_axis)),
            aes(x = x,
                ymin = x - RCcutoff,
                ymax = x + RCcutoff),
            inherit.aes = FALSE, # If FALSE, overrides the default aesthetics, rather than combining with them. 
            fill = "skyblue",  # just change the color
            alpha = 0.5) +   # the opacity of a geom (range from 0 to 1), lower values are more transparent colors
          
          # Add the diagonal 'no change' line
          geom_abline(
            intercept = 0, 
            slope = 1, 
            color = "black", 
            linetype = "solid") +
          
          # Add the individual's score as a point
          geom_point(size = 5, shape = 20) + 
          
          # Annotation for the confidence band
          annotate("text", x = min_axis + (max_axis - min_axis)*0.1, y = max_axis, 
                   label = paste0(95, "% Confidence Band\n(No Significant Change)"), 
                   hjust = 0, vjust = 1, color = "steelblue4") +
          # hjust = 0: The left edge of the text is placed at the x coordinate
          # vjust = 1: The top of the text is placed at the y coordinate
          
          # Annotation for sig deterioration/improvement"
          annotate("text", x = x_det, y = y_det,
                   label = "Significant\nDeterioration",
                   color = "coral4", fontface = "bold") + 
          
          annotate("text", x = x_imp, y = y_imp,
                   label = "Significant\nImprovement",
                   color = "darkolivegreen", fontface = "bold") + 
          
          # Set plot limits and ensure the aspect ratio is 1:1
          coord_fixed(ratio = 1, xlim = c(min_axis, max_axis), ylim = c(min_axis, max_axis)) +
          
          # Add labels and title
          labs(
            title = "Individual Pre- vs. Post-Test Score",
            x = "Pre-Test Score", y = "Post-Test Score",
            caption = "Individual’s pre- and post-test scores that land on the diagonal line 
      and within the confidence band indicates no significant change.") +
          
          # Use a clean theme
          theme_bw(base_size = 14) # The classic dark-on-light ggplot2 theme
      
      
      
    } # if ("JTRCI")
    
    
    
    # --- IRT-based RCI ---
    # else if: adds additional conditions to check only if the previous condition was false;
    # else: looks at all other cases when the if conditions are not fulfilled.
    #   bcz we have sub-category, estmethod, if IRT-RCI is selected, so use else if. 
    else if(input$model == "IRTRCI"){
      
      
      model_choice <- models[[input$model_choice]]
      IRTmodel <- model_choice$model  # model = BDI2mod, PHQ9mod, GAD7mod
      max_score <- model_choice$max_score
      jitems <- model_choice$jitems

      
      
      
      # --- For EAPsum only ---
      if (input$estmethod == 'EAPsum'){
        # For sum scores
        req(input$pretest, is.numeric(input$pretest))
        req(input$posttest, is.numeric(input$posttest)) 
        
        # Make sure scores are within the possible range for the test
        if(input$pretest < 0 || input$pretest > max_score || 
           input$posttest < 0 || input$posttest > max_score) {
          RCIresults$status <- "Error: Scores are outside the possible range for this test."
          return() # Stop execution
        } 
        
        # 4. Look up table for EAPsum
        results_table <- fscores(IRTmodel, method = 'EAPsum', 
                                 full.scores=FALSE, verbose=FALSE)
        tab <- data.frame(results_table[,1:3])
        lookuptab <- tab
        colnames(lookuptab) <- c("Sum Score", "Estimated Theta", "SE")
        RCIresults$lookup <- lookuptab
        
        # Get individual EAPsum estimates & SE values
        # pre row
        pre_row <- tab[tab$Sum.Scores == input$pretest, ]
        # post row
        post_row <- tab[tab$Sum.Scores == input$posttest, ]
        
        theta_pre <- pre_row$F1
        sem_pre   <- pre_row$SE_F1
        theta_post <- post_row$F1
        sem_post   <- post_row$SE_F1
        
        
        
        # Manually calculate RCI for EAPsum
        sem <- sqrt(sem_pre^2 + sem_post^2)
        rci <- (theta_post - theta_pre)/sem
        p_val <- 2 * pnorm(abs(rci), lower.tail = FALSE)
        
        # 3. Results Table
        RCIresults$table_data <- data.frame(
          "Theta Pre" = theta_pre, "Theta Post" = theta_post,
          Difference = theta_post - theta_pre,
          "SE" = sem,
          RCI = rci, "P-value" = p_val,
          check.names = FALSE # Preserves how the colum names are displayed (if took out would have "P.value" instead of P-value")
        )

        # 1. Status: Changed or not
        RCIresults$status <- if (p_val < 0.05) {
          "Change Status: Reliably Changed (p < .05)"
        } else {
          "Change Status: Did Not Reliably Change (p >= .05)"
        }
        
        # 2. RCI cutoff
        RCcutoff <- 1.96*sqrt(2)*sem
        RCIresults$summary_text <- paste0(
          "The individual must improve/decline by at least ", round(RCcutoff, 2), 
          " points for the change to be considered statistically significant, with α =.05."
        ) # paste0()
        
        
        
        # --- For all other IRT methods (EAP, MAP, WLE, ML) --- 
      } else { 
        # For vector scores
        req(input$prevec, input$postvec)

        preS <- as.numeric(strsplit(input$prevec, "")[[1]])
        postS <- as.numeric(strsplit(input$postvec, "")[[1]])
         
         if (anyNA(preS) || anyNA(postS) || 
             length(preS) != jitems || length(postS) != jitems){
           RCIresults$status <- paste("Error: Vector scores need to be the same length as the test items.
                                      This test has", jitems, "items.")
           return()
         }
        
        
        # RCI for IRT vector scores only 
        IRTdat <- RCI(IRTmodel, predat = preS, postdat = postS, 
                      method = input$estmethod, Fisher = input$fisher)
        
        # 3. Results Table
        IRTtable <- data.frame(subset(IRTdat, select = -converged))
        colnames(IRTtable) <- c('Theta Pre', 'Theta Post',
                                'Difference', 'SE', 'RCI', 'P-value')
        RCIresults$table_data <- IRTtable
        
        # 1. Status: Changed or not
        RCIresults$status <- if (IRTdat$p < 0.05) {
          "Change Status: Reliably Changed (p < .05)"
        } else {
          "Change Status: Did Not Reliably Change (p >= .05)"
        }
      
        
        
  
      } # else() for all other IRT methods
      
  
    } # else if ("IRTRCI")
    
      
  }) # observeEvent() (aka submitbutton portion)
    
  
  output$LookUpTitle <- renderUI({
    # Only proceed if the lookup table data actually exists
    req(!is.null(RCIresults$lookup))
    tagList(
      h4("Lookup Table"))
  })
  
  # --- Outputs from reactiveValues() (RCIresults)
  output$RCstatus <- renderText({RCIresults$status})
  output$results <- renderText({RCIresults$summary_text})
  output$RCtable <- renderTable({RCIresults$table_data})
  output$LookUp <- renderTable({RCIresults$lookup})
  output$rci_plot <- renderPlot({RCIresults$rciplot})

  
} # server() 
  






# create Shiny app
shinyApp(ui = ui, server = server)


