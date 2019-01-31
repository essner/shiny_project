library(tidyverse)
library(dendextend)
library(shinythemes)
library(corrplot)
library(shiny)
library(magrittr)
library(circlize)
library(RColorBrewer)
library(shinyWidgets)
library(gridExtra)

dist_data = read.csv(file = "./shiny_data_2.csv")
dend_data = dist_data %>% select(., -contains("NBA"))
scatter_data = dist_data %>% select(., -player, -position)
scatter_labels = colnames(scatter_data)

ui <- fluidPage(navbarPage(
  theme = shinytheme("flatly"),
  "NBA",
  tabPanel("Plots",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               pickerInput(
                 inputId = "y_input",
                 label = "Choose Y:",
                 choices =  scatter_labels,
                 selected = c("weight"),
                 options = list(
                   `actions-box` = TRUE,
                   size = 10,
                   `selected-text-format` = "count > 3"
                 )
               ),
               br(),
               pickerInput(
                 inputId = "x_input",
                 label = "Choose X:",
                 choices =  scatter_labels,
                 selected = c("height"),
                 options = list(
                   `actions-box` = TRUE,
                   size = 10,
                   `selected-text-format` = "count > 3"
                 )
               )
             ),
             mainPanel(plotOutput("scatter", width = "auto", height = "600px"))
           ),
           br(),
           br()),
  tabPanel("Correlation",
           tabsetPanel(
             type = "tabs",
             tabPanel(
               "NBA-NCAA",
               br(),
               br(),
               plotOutput("correl_play", width = "auto", height = "600px"),
               br(),
               br()
             ),
             tabPanel(
               "Combine-Performance",
               br(),
               br(),
               splitLayout(
                 cellWidths = c("50%", "50%"),
                 plotOutput("correl_anthro_1", width = "100%", height = "700px"),
                 plotOutput("correl_anthro_2", width = "100%", height = "700px")
               )
             )
             
           ),
           br(),
           br(),
           br()),
  tabPanel(
    "Player Comparisons",
    tabsetPanel(
      type = "tabs",
      tabPanel("By Position",
               sidebarLayout(
                 sidebarPanel(
                   width = 4,
                   helpText(
                     "Dendogram to analyze players based on measures of athleticism and performance"
                   ),
                   pickerInput(
                     inputId = "positionInput",
                     label = "Choose Position:",
                     choices = c("PG", "SG", "SF", "PF", "C"),
                     selected = c("PG"),
                     options = list(
                       `actions-box` = TRUE,
                       size = 10,
                       `selected-text-format` = "count > 3"
                     )
                   )
                 ),
                 mainPanel(plotOutput(
                   "distPlot", width = "auto", height = "700px"
                 ))
               )),
      tabPanel("By Branch",
               sidebarLayout(
                 sidebarPanel(
                   width = 4,
                   helpText(
                     "Choose a brach of the dendogram to explore in greater detail."
                   ),
                   pickerInput(
                     inputId = "mySliderText",
                     label = "Choose Branch:",
                     choices =  c("Branch 1", "Branch 2-1-1", "Branch 2-1-2", "Branch 2-2"),
                     selected = c("Branch 1"),
                     options = list(
                       `actions-box` = TRUE,
                       size = 10,
                       `selected-text-format` = "count > 3"
                     )
                   )
                 ),
                 mainPanel(plotOutput(
                   "distPlot_all", width = "auto", height = "700px"
                 ))
               ))))
))

server = function(input, output) {
  # Create dend by player
  output$distPlot <- renderPlot({
    # Filter Data
    filtered = dend_data %>%
      filter(., position == input$positionInput) %>%
      select(.,-position)
    
    # clean data
    rownames(filtered) = filtered[, 1]
    filtered = filtered %>% select(.,-player)
    
    # create a dendrogram
    hc = hclust(dist(filtered))
    dend = as.dendrogram(hc)
    
    # Plot
    circlize_dendrogram(dend,
                        labels_track_height = .3,
                        dend_track_height = .4)
    
  })
  
  output$distPlot_all = renderPlot({
    # Filter Data
    filtered = dend_data %>%
      select(.,-position)
    
    # clean data
    rownames(filtered) = filtered[, 1]
    filtered = filtered %>% select(.,-player)
    
    # create a dendrogram
    hc = hclust(dist(filtered))
    dend = as.dendrogram(hc)
    
    # Plot
    if (input$mySliderText == "Branch 1") {
      circlize_dendrogram(dend[[1]],
                          labels_track_height = .3,
                          dend_track_height = .4)
    }
    if (input$mySliderText == "Branch 2-1-1") {
      circlize_dendrogram(dend[[2]][[1]][[1]],
                          labels_track_height = .3,
                          dend_track_height = .4)
    }
    if (input$mySliderText == "Branch 2-1-2") {
      circlize_dendrogram(dend[[2]][[1]][[2]],
                          labels_track_height = .3,
                          dend_track_height = .4)
    }
    if (input$mySliderText == "Branch 2-2") {
      circlize_dendrogram(dend[[2]][[2]],
                          labels_track_height = .3,
                          dend_track_height = .4)
    }
    
    
  })
  
  # Plot for Correlation Matrix by Stats
  output$correl_play = renderPlot({
    # clean data
    rownames(dist_data) = dist_data[, 1]
    correl_data = dist_data %>% select(., contains("NBA"), contains("NCAA"))
    data = cor(correl_data)
    corrplot(
      data,
      method = "circle",
      tl.col = "black",
      col = brewer.pal(n = 8, name = "RdYlBu")
    )
  })
  
  # Plot for Correlation Matrix NBA-Anthro
  output$correl_anthro_1 = renderPlot({
    # clean data
    rownames(dist_data) = dist_data[, 1]
    correl_data = dist_data %>% select(.,-player, -position,-contains("NCAA"))
    temp = correl_data %>% select(., 8:18, 1:7)
    temp_names = colnames(temp)
    temp_names = gsub("NBA_", "", temp_names)
    names(temp) = temp_names
    data = cor(temp)[1:11, 12:18, drop = FALSE]
    
    # Plot
    corrplot(
      data,
      method = "circle",
      tl.col = "black",
      col = brewer.pal(n = 8, name = "RdYlBu"),
      cl.pos = "n",
      mar=c(1,1,1,1),
      title = "NBA"
    )
    
  })
  
  # Plot for Correlation Matrix NCAA-Anthro
  output$correl_anthro_2 = renderPlot({
    # clean data
    rownames(dist_data) = dist_data[, 1]
    correl_data = dist_data %>% select(.,-player, -position,-contains("NBA"))
    temp = correl_data %>% select(., 8:10, 12, 13, 11, 14, 16, 15, 17:18, 1:7)
    temp_names = colnames(temp)
    temp_names = gsub("NCAA_", "", temp_names)
    names(temp) = temp_names
    data = cor(temp)[1:11, 12:18, drop = FALSE]
    
    # Plot
    corrplot(
      data,
      method = "circle",
      tl.col = "black",
      col = brewer.pal(n = 8, name = "RdYlBu"),
      mar=c(1,1,1,1),
      title = "NCAA"
    )
    
  })
  
  output$scatter = renderPlot({
    g1 = ggplot(data = dist_data, aes(
      x = !!as.symbol(input$x_input),
      y = !!as.symbol(input$y_input)
    )) + geom_point(aes(color = position), size = 4) + theme(legend.position ="top")
    g2 = ggplot(dist_data, aes(x = !!as.symbol(input$x_input))) + geom_density(fill ="slateblue")
    g3 = ggplot(dist_data, aes(x = !!as.symbol(input$y_input))) + geom_density(fill ="slateblue")
    grid.arrange(g1, arrangeGrob(g2, g3, ncol = 1), nrow = 2)

  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
