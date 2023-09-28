library(shiny)
library(readxl)
library(writexl)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title {
        text-align: center;
      }
      .instructions {
        margin: 30px;
        font-size: 16px;
      }
      .footer {
        padding: 20px;
        background-color: #f1f1f1;
        text-align: center;
        margin-top: 20px;
        font-size: 12px;
      }
    ")),
    tags$script(HTML("
      $(document).ready(function(){
        function adjustHeight() {
          var panelWidth = $('#mainPanel').width();
          var newHeight = panelWidth * 0.7;
          $('#scatterPlot').height(newHeight);
        }
        adjustHeight();
        $(window).resize(adjustHeight);
      });
    "))
  ),
  div(class = "title", 
      titlePanel(
        title = HTML("Training Response Simulator: Challenging the Responder Counting Approach")
      )
  ),
  fluidRow(
    column(12,
           div(class = "instructions", 
               HTML("This simulator is designed to explore maximum oxygen uptake (V̇O<sub>2max</sub>) responses to training. The purpose is to challenge the widespread use of the 'responder counting approach' by allowing users to manipulate various parameters and observe the resulting distributions. <br><br>
                    The slider panel consists of: <br>
                    - Sample Size, allows users to select a sample size between 0 and 1000. <br>
                    - V̇O<sub>2max</sub> Mean Gain, allows users to set the mean gain in V̇O<sub>2max</sub> after training, ranging from -10.5 to 20.5. <br>
                    - V̇O<sub>2max</sub> Gain SD, allows users to set the standard deviation of V̇O<sub>2max</sub> responses, ranging from 0 to 5.5. <br>
                    - Responder Threshold, allows users to set the threshold for classifying individuals as 'responders' or 'non-responders', ranging from -10.5 to 10.5. <br><br>
                    This simulator assumes a baseline dataset with a mean of 43.8 and standard deviation of 6.6 ml&middot;kg<sup>-1</sup>&middot;min<sup>-1</sup>. Note how, regardless of parameter choices, the number of 'responders' and 'non-responders' inevitably reflect the group mean V̇O<sub>2max</sub> gain. While the importance of reporting individual responses is indisputable,
                    as illustrated by one of the simulator’s plots, researchers must avoid statements like 'training protocol X yielded a greater number of responders compared with protocol Y.' Such assertions are misleading, given that the vast majority of training interventions are methodologically structured to enable inferences at the population rather than the individual level. 
                    Therefore, scientists are strongly recommended to base their conclusions on means and standard deviations, which are the statistical measures designed to support population-level inferences.
                    ")
           )
    )
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs1", "Sample Size (n)", min = 0, max = 1000, value = 500),
      sliderInput("obs2", HTML("<b>V̇O<sub>2max</sub> Mean Gain (ml&middot;kg<sup>-1</sup>&middot;min<sup>-1</sup>)</b>"), min = -10.5, max = 20.5, value = 0),
      sliderInput("obs3", HTML("<b>V̇O<sub>2max</sub> Gain SD (ml&middot;kg<sup>-1</sup>&middot;min<sup>-1</sup>)</b>"), min = 0, max = 5.5, value = 2.5),
      sliderInput("threshold", HTML("<b>Responder Threshold (ml&middot;kg<sup>-1</sup>&middot;min<sup>-1</sup>)</b>"), min = -10.5, max = 10.5, value = 3.5),
      downloadButton('downloadData', 'Download Data')
    ),
    mainPanel(
      plotOutput("hist1"),
      plotOutput("hist2"),
      plotOutput("hist3"),
      plotOutput("scatterPlot", height = "800px")
    )
  ),

  div(class = "footer",
      HTML("This simulator has been conceptualised by <strong>Dr Arthur Henrique Bossi</strong>."),
      br(),
      HTML("Find me on:"),
      a(href = "https://twitter.com/ahbossi", "Twitter"),
      " | ",
      a(href = "https://www.researchgate.net/profile/Arthur-Bossi", "ResearchGate"),
      " | ",
      a(href = "https://www.linkedin.com/in/arthur-henrique-bossi", "LinkedIn"),
      br(),
      br(),
      a(href = "https://raw.githubusercontent.com/ahbossi/Bossi_Simulator/7c5ece767bddd1f2235f8a904d434649fb2df488/LICENSE", "MIT License"),
      " | ",
      a(href = "https://github.com/ahbossi/Bossi_Simulator", "Code"),
      br(),
      br(),
      HTML('<a href="https://zenodo.org/badge/latestdoi/697859921"><img src="https://zenodo.org/badge/697859921.svg" alt="DOI"></a>'),
      br()
  )
)


server <- function(input, output) {
  
  data1_reactive <- reactive({
    rnorm(input$obs1, mean = 34.8, sd = 6.6)
  })
  
  data2_reactive <- reactive({
    data1_reactive() + rnorm(input$obs1, mean = input$obs2, sd = input$obs3)
  })
  
  range_reactive <- reactive({
    all_data <- c(data1_reactive(), data2_reactive())
    c(min(all_data), max(all_data))
  })
  
  output$hist1 <- renderPlot({
    par(mar=c(5.1, 6.1, 4.1, 2.1))
    hist(data1_reactive(), main = "Baseline Data",
         xlim = range_reactive(),
         xlab = expression(bold(dot(V)*O[2*max]~("ml" * "\U00B7kg"^{"-1"} * "\U00B7min"^{"-1"}))),
         ylab = "Individuals",
         col = "lightblue",
         border = "black")
  })
  
  output$hist2 <- renderPlot({
    par(mar=c(5.1, 6.1, 4.1, 2.1))
    hist(data2_reactive(), main = "Post-Training Data",
         xlim = range_reactive(),
         xlab = expression(bold(dot(V)*O[2*max]~("ml" * "\U00B7kg"^{"-1"} * "\U00B7min"^{"-1"}))),
         ylab = "Individuals",
         col = "lightgreen",
         border = "black")
  })
  
  output$hist3 <- renderPlot({
    par(mar=c(5.1, 6.1, 4.1, 2.1))
    data_delta <- data2_reactive() - data1_reactive()
    
    if (input$obs3 == 0) {
      breaks = c(min(data_delta) - 0.1, max(data_delta) + 0.1)
      col_data = ifelse(min(data_delta) > input$threshold, "yellow", "red")
    } else {
      breaks = seq(floor(min(data_delta)), ceiling(max(data_delta)), by = 0.5)
      hist_data <- hist(data_delta, breaks = breaks, plot = FALSE)
      col_data <- ifelse(hist_data$breaks[-length(hist_data$breaks)] + diff(hist_data$breaks)/2 > input$threshold, "yellow", "red")
    }
    
    hist(data_delta, breaks = breaks, col = col_data, main = "Delta of Observations",
         xlab = expression(bold(Delta~dot(V)*O[2*max]~("ml" * "\U00B7kg"^{"-1"} * "\U00B7min"^{"-1"}))),
         ylab = "Individuals",
         border = "black")
    
    abline(v = input$threshold, col = "blue", lty = 2, lwd = 3)
    
    legend("topright", legend = c("Responder", "Non-Responder"), fill = c("yellow", "red"))
  })
  
  output$scatterPlot <- renderPlot({
    par(mar=c(5.1, 6.1, 4.1, 2.1), xpd=NA, bty="l")
    baseline_data <- data1_reactive()
    post_training_data <- data2_reactive()
    
    plot(c(0.9, 2.1), range(c(baseline_data, post_training_data)), type="n",
         xaxt="n", xlab="", ylab=expression(dot(V)*O[2*max]~("ml" * "\U00B7kg"^{"-1"} * "\U00B7min"^{"-1"})),
         main="Individual Responses")
    
    offset <- 0.1
    axis(1, at=c(1 + offset, 2 - offset), labels=c("Baseline", "Post-Training"))
    
    points(rep(1 + offset, length(baseline_data)), baseline_data, pch=15, col="lightblue")
    points(rep(2 - offset, length(post_training_data)), post_training_data, pch=15, col="lightgreen")
    
    segments(rep(1 + offset, length(baseline_data)), baseline_data,
             rep(2 - offset, length(post_training_data)), post_training_data,
             col="black")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Simulator Data by Bossi_", format(Sys.Date(), "%d-%m-%Y"),".xlsx", sep="")
    },
    content = function(file) {
      baseline_data <- data1_reactive()
      post_training_data <- data2_reactive()
      data_delta <- post_training_data - baseline_data
      
      combined_df <- data.frame(
        "Baseline Data" = baseline_data,
        "Post-Training Data" = post_training_data,
        "Delta of Observations" = data_delta
      )
      
      write_xlsx(list("Data" = combined_df), file)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)
