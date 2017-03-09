library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("data.R")

server <- function(input, output) {
  
  filtered <- reactive({
    db <- data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1) %>% 
      filter(diff > input$diff.choice[1] & diff < input$diff.choice[2])
    return(db)
  })
  
<<<<<<< HEAD
  output$plot <- renderPlot({
    
    p <- ggplot(data = filtered(), mapping = aes(x = Occupation, y = All_workers,fill=diff)) +
      geom_bar(stat="identity") + facet_wrap(input$facet.by) +theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) + ggtitle("Overall Data w/ Differences") + 
      scale_fill_gradient(low = "yellow", high = "red") + ylab("Number of All Workers (x1000)")
    
    
    return(p)
=======
  wage.filtered <- reactive({
    return(filter(data.wage.time, LOCATION == input$time.wage) %>% select(TIME, Value)) 
  })
  output$plot <- renderPlotly({
    x_axis_format <- list(
      title = "Occupations (Hover over bars for details!)",
      titlefont = list(size=12),
      showticklabels = FALSE
    )
    
    y_axis_format <- list(
      title = "Amount of Workers in Occupation (x1000)",
      titlefont = list(size=12, weight = "bold")
      
    )
    
    g<-plot_ly(filtered(), type="bar", x = ~Occupation, y = ~All_workers, color = ~diff,
               text = ~paste("Wage Gap: ", diff)) %>% 
      layout(title = "Overall Data w/ Differences", xaxis = x_axis_format, yaxis = y_axis_format)
  
    return(g)
  })
  
  output$text1 <- renderText({
    paste0("sdasdasaff")
>>>>>>> d2da34508afa730cd0317b9e4df5723524b594ad
  })
  
  filtered2 <- reactive({
    db <- female.data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1)
    return(db)
  })
  
  output$plot2 <- renderPlotly({
    
    x_axis_format <- list(
      title = "Occupations (Hover over bars for details!)",
      titlefont = list(size=12),
      showticklabels = FALSE
    )
    
    y_axis_format <- list(
      title = "Amount of Female Workers in Occupation (x1000)",
      titlefont = list(size=12, weight = "bold")
      
    )
    
<<<<<<< HEAD
    p <- ggplot(data = filtered2(), mapping = aes(x = Occupation, y = F_workers, fill=F_weekly)) +
      geom_bar(stat="identity") + facet_wrap(input$facet.by) + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) +
      ggtitle("Female Data with # of workers & pay rate") + scale_fill_gradient(low = "yellow", high = "red") +
      ylab("Number of Female Workers (x1000)")
=======
    g<-plot_ly(filtered2(), type="bar", x = ~Occupation, y = ~F_workers, color = ~F_weekly,
               text = ~paste("Median wage for Females: $", F_weekly)) %>% 
      layout(title = "Female Data with # of workers & pay rate", xaxis = x_axis_format, yaxis = y_axis_format)
>>>>>>> d2da34508afa730cd0317b9e4df5723524b594ad
    
    
    return(g)
  })
  
  output$text2 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered3 <- reactive({
    db <- male.data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1)
    return(db)
  })
  
  output$plot3 <- renderPlotly({
    
    x_axis_format <- list(
      title = "Occupations (Hover over bars for details!)",
      titlefont = list(size=12),
      showticklabels = FALSE
    )
    
<<<<<<< HEAD
    p <- ggplot(data = filtered3(), mapping = aes(x = Occupation, y = M_workers, fill=M_weekly)) +
      geom_bar(stat="identity") + facet_wrap(input$facet.by) + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) + 
      ggtitle("Male Data with # of workers & pay rate") + scale_fill_gradient(low = "yellow", high = "red") + 
      ylab("Number of Male Workers (x1000)")
=======
    y_axis_format <- list(
      title = "Amount of Male Workers in Occupation (x1000)",
      titlefont = list(size=12, weight = "bold")
      
    )
>>>>>>> d2da34508afa730cd0317b9e4df5723524b594ad
    
    g<-plot_ly(filtered3(), type="bar", x = ~Occupation, y = ~M_workers, color = ~M_weekly,
               text = ~paste("Median wage for Males: ", M_weekly)) %>% 
      layout(title = "Male Data with # of workers & pay rate", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
  })
  
  output$text3 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered4 <- reactive({
    db <- bottom.ten.percent.difference
    return(db)
  })
  
  output$plot4 <- renderPlotly({
    
    x_axis_format <- list(
      title = "Occupations (Hover over bars for details!)",
      titlefont = list(size=12),
      showticklabels = FALSE
    )
    
    y_axis_format <- list(
      title = "Difference in Median Wage (USD)",
      titlefont = list(size=12, weight = "bold")
      
    )
    
    g<-plot_ly(filtered4(), type="bar", x = ~Occupation, y = ~diff, color = ~All_workers,
               text = ~paste("Amount of workers: ", All_workers)) %>% 
      layout(title = "Bottom 10% of Wage Gap", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
  })
  
  output$text4 <- renderText({
    paste0("sdasdasaff")
  }) 
  
  filtered5 <- reactive({
    db <- top.ten.percent.male.difference
    return(db)
  })
  
  output$plot5 <- renderPlotly({
    
    x_axis_format <- list(
      title = "Occupations (Hover over bars for details!)",
      titlefont = list(size=12),
      showticklabels = FALSE
    )
    
    y_axis_format <- list(
      title = "Difference in Median Wage (USD)",
      titlefont = list(size=12, weight = "bold")
      
    )
    
    g<-plot_ly(filtered5(), type="bar", x = ~Occupation, y = ~diff, color = ~(M_workers/All_workers),
               text = ~paste("Percentage of Male Workers: ", (M_workers/All_workers))) %>% 
      layout(title = "Top 10% of Male Favored Gap", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    
    return(g)
  })
  
  output$text5 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered6 <- reactive({
    db <- top.ten.percent.female.difference
    return(db)
  })
  
  output$plot6 <- renderPlotly({
    
    x_axis_format <- list(
      title = "Occupations (Hover over bars for details!)",
      titlefont = list(size=12),
      showticklabels = FALSE
    )
    
    y_axis_format <- list(
      title = "Difference in Median Wage (USD)",
      titlefont = list(size=12, weight = "bold")
      
    )
    
    g<-plot_ly(filtered6(), type="bar", x = ~Occupation, y = ~abs(diff), color = ~(F_workers/All_workers),
               text = ~paste("Percentage of Females workers: ", (F_workers/All_workers))) %>% 
      layout(title = "Top 10% of Female Favored Gap", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
  })
  

  
  desc<-reactive({
    paragraph<-paste0("The graph above is a visual representation for data collected from 150 Iris plants. Currently, the graph represents the ",
                      input$species.choice, " species of Iris plants. The visual shows a correlation between the sepal length of the plant and the
    petal length. Sepal length is on a scale from ", input$sepal.choice[1], "cm to ", input$sepal.choice[2],"cm and petal length
    ranges from ", input$petal.choice[1], "cm to ", input$petal.choice[2], "cm. ")
    return(paragraph)
  })
  
  output$description<- renderText({
    return(desc())
  })
  
  
  output$table <- renderDataTable({
    filtered.data.frame<-filtered()
    colnames(filtered.data.frame)<-c("Occupation", "Number of all workers(x1000)","Median Salary of All Workers",
                                     "Number of Male workers(x1000)","Male Median Salary","Number of Female workers(x1000)", "Female Median Salary",
                                     "Difference of Male to Female Wage", "Wage Gap Gender Favor")
    return(filtered.data.frame)
  })
  
  output$plot7 <- renderPlot ({
    h <- ggplot(data = wage.filtered(),aes(x = TIME, y = Value, colour = Value))+
      geom_line()+
      geom_point()+
      theme_light()+
    labs(title = "Line graph showing change in wage gap over time", x = "Time (Year)", y = "")
    return(h)
  })
  
  output$text <- renderText({
    string <- function(input.wage) {
      if(is.null(input.wage))
        return("Hover over Time Wage Plot")
        paste("Year = ", round(input.wage$x,0), "Wage Gap = ", round(input.wage$y,2))
    }
    paste0("Numbers:", string(input$hover))
  })
  
  output$text6 <- renderText({
    paste0("sdasdasaff")
  })
  
  
}

shinyServer(server)