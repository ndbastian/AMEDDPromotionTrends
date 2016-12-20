# Promotion Trend Analysis Live
.libPaths("Z:/RLibrary")
library(rsconnect)
rsconnect::setAccountInfo(name='ndbastian', 
                          token='2AE15B3CDA087EDB3D3AC80E640CD80F', 
                          secret='yetI9O6I/wx5gm85Z6Pn19GB8dCgtVQVI4wKZ9Z7')
library(shiny)

# Load the data
promTrend <- read.csv("PromotionTrends0116.csv", header = T)
promTrendInv <- matrix()
fy16prom <- read.csv("FY16PromotionResults.csv", header = T)
fy16promSub <- matrix()

# Define the UI
ui <- pageWithSidebar(
  headerPanel('AMEDD AC Promotion Selection Board Analyses'),
  sidebarPanel(
    tags$style(".well {background-color:#ffffff;}"),
    width = 2,
    plotOutput('logo'),
    hr(),
    selectInput('board', 'Board', c("CPT", "MAJ", "LTC", "COL")),
    selectInput('branch', 'Branch', c("MS", "AN", "SP", "MC", "DC", "VC")),
    selectInput('skill', 'AOC', fy16prom[c(1:93),2]),
    sliderInput('range', 'Years:', min = 2001, max = 2016, value = c(2001, 2016), sep = ""),
    hr(),
    h6("Disclaimer: The information contained in this communication is intended for the sole use of the granted recipients, in their conduct of official business of the United States Government. This communication may contain information that is exempt from disclosure under the Freedom of Information Act, 5 U.S.C. 552 and the Privacy Act, 5 U.S.C. 552a. Recipients are not to disseminate this communication to individuals other than those who have an official need to know the information in the course of their official government duties. If you received this communication in error, please do not examine, review, print, copy, forward, disseminate, or otherwise use the information.")),

  mainPanel(
    h3("Selection Rate Trends by Promotion Zone", align = "center"),
    h4("By Selected Board and Branch", align = "center"),
    plotOutput('plot1'),
    dataTableOutput('table1'),
    h3("Overall Eligibility and Selection Trends", align = "center"),
    h4("By Selected Board and Branch", align = "center"),
    dataTableOutput('table2'),
    h3("FY16 Eligibility and Selection Details by AOC", align = "center"),
    h4("By Selected AOC", align = "center"),
    plotOutput('plot2'),
    dataTableOutput('table3'),
    h4("Alls AOCs by Selected Branch", align = "center"),
    dataTableOutput('table4')
  )
)

# Define the server code
server <- function(input, output) {
  
  output$logo <- renderImage({
    list(src = './AMEDDLogo.jpg', width = 260, height = 300)
  }, deleteFile = FALSE)

  output$plot1 <- renderPlot({
    promTrend <- subset(promTrend, promTrend$FY >= input$range[1] & promTrend$FY <= input$range[2])
    plot(promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$FY, 
         promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$Perc_PZ_Selects, 
         pch = 20, type = "b", col = "red",
        ylab = "Selection Rate (%)", xlab = "Fiscal Year", xaxt = "n", ylim = c(0, 100))
    axis(side = 1, at = promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$FY)
    lines(promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$FY, 
          promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$Perc_AZ_Selects, 
          pch = 20, type = "b", col = "brown")
    lines(promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$FY, 
          promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), ]$Perc_BZ_Selects, 
          pch = 20, type = "b", col = "green")
    legend("topleft", inset = 0.01, 
           legend = c("% PZ Select", "% AZ Select", "% BZ Select"), 
           pch = 20, col = c("red", "brown", "green"), horiz = T)
  })
  
  output$table1 <- renderDataTable({
    promTrendInv <- t(as.matrix(promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch & 
                                                 promTrend$FY >= input$range[1] & promTrend$FY <= input$range[2]), c(1,6, 9, 12)]))
    promTrendInv <- as.data.frame(promTrendInv, rownames = c("Year", "% AZ Select", "% PZ Select", "% BZ Select"))
    promTrendInv <- data.frame(c("Year", "% AZ Select", "% PZ Select", "% BZ Select"), promTrendInv)
    promTrendInv <- promTrendInv[c(2:4), ]
    colnames(promTrendInv) <- c("Zone/FY", as.character(input$range[1]:input$range[2]))
    promTrendInv <- promTrendInv
    
  }, options = list(aLengthMenu = c(3), iDisplayLength = 3))
  
  output$table2 <- renderDataTable({
    promTrend <- subset(promTrend, promTrend$FY >= input$range[1] & promTrend$FY <= input$range[2])
    promTrend <- promTrend[with(promTrend, promTrend$Board  == input$board & promTrend$Branch == input$branch), c(1,4:12)]
    colnames(promTrend) <- c("FY", "AZ Eligible", "AZ Select", "% AZ Select", "PZ Eligible", "PZ Select", "% PZ Select",
                             "BZ Eligible", "BZ Select", "% BZ Select")
    promTrend <- promTrend
  }, options = list(aLengthMenu = c(5, 10, 15, 20), iDisplayLength = input$range[2] - input$range[1]))
  
  output$table3 <- renderDataTable({
    fy16prom <- fy16prom[with(fy16prom, fy16prom$board  == input$board & fy16prom$branch == input$branch & fy16prom$skill == input$skill), c(2, 4:12)]
    colnames(fy16prom) <- c("AOC", "AZ Select", "AZ Eligible", "% AZ Select", "BZ Select", "BZ Eligible", "% BZ Select",
                            "PZ Select", "PZ Eligible", "% PZ Select")
    fy16prom <- fy16prom
  }, options = list(aLengthMenu = c(1), iDisplayLength = 1))
  
  output$plot2 <- renderPlot({
    fy16promSub <- as.matrix(fy16prom[with(fy16prom, fy16prom$board  == input$board & fy16prom$branch == input$branch & 
                                   fy16prom$skill == input$skill), c(4:5, 7:8, 10:11)])
    fy16promSub <- rbind(fy16promSub[1, c(1:2)], fy16promSub[1, c(3:4)], fy16promSub[1, c(5:6)])
    fy16promSub <- cbind(fy16promSub[, 1], matrix(c(fy16promSub[1,2] - fy16promSub[1,1], 
                               fy16promSub[2,2] - fy16promSub[2,1], 
                               fy16promSub[3,2] - fy16promSub[3,1]), nrow = 3, ncol = 1)) 
    rownames(fy16promSub) <- c("AZ", "BZ", "PZ")
    colnames(fy16promSub) <- c("Select", "Non Select")
    
    bplt <- barplot(t(fy16promSub), col = c("green", "brown"),
            ylab = "Selection Zone", xlab = "Number of Officers", beside = T, horiz = T)
    legend("bottomright", inset = 0.05, pch = 15,
           legend = c("Select", "Non Select"), col = c("green", "brown"),
           horiz = T, title = "Selected?")
    text(x = t(fy16promSub) + 0.8, y = bplt, labels = as.character(t(fy16promSub)), xpd=T)
  })
  
  
  output$table4 <- renderDataTable({
    fy16prom <- fy16prom[with(fy16prom, fy16prom$board  == input$board & fy16prom$branch == input$branch), c(2, 4:12)]
    colnames(fy16prom) <- c("AOC", "AZ Select", "AZ Eligible", "% AZ Select", "BZ Select", "BZ Eligible", "% BZ Select",
                            "PZ Select", "PZ Eligible", "% PZ Select")
    fy16prom <- fy16prom
  }, options = list(aLengthMenu = c(5, 10, 15, 20, 25, 30), iDisplayLength = 10))
  
}

 # Return a Shiny app object
shinyApp(ui = ui, server = server)
