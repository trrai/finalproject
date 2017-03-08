library(shiny)
library(ggplot2)
library(dplyr)
source("data.R")

server <- function(input, output) {
  
  filtered <- reactive({
    db <- data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1) %>% 
      filter(diff > input$diff.choice[1] & diff < input$diff.choice[2])
    return(db)
  })
  
  wage.filtered <- reactive({
    return(filter(data.wage.time, LOCATION == input$time.wage) %>% select(TIME, Value)) 
  })
  output$plot <- renderPlot({

    p <- ggplot(data = filtered(), mapping = aes(x = Occupation, y = All_workers,fill=diff)) +
      geom_bar(stat="identity") + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
            axis.title = element_text(size=15), plot.title = element_text(size=20)) + ggtitle("Overall Data w/ Differences") + 
      scale_fill_gradient(low = "yellow", high = "red") + ylab("Number of All Workers (x1000)")

  
    return(p)
  })
  
  output$text1 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered2 <- reactive({
    db <- female.data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1)
    return(db)
  })
  
  output$plot2 <- renderPlot({
    
    p <- ggplot(data = filtered2(), mapping = aes(x = Occupation, y = F_workers, fill=F_weekly)) +
      geom_bar(stat="identity") + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) +
      ggtitle("Female Data with # of workers & pay rate") + scale_fill_gradient(low = "yellow", high = "red") +
      ylab("Number of Female Workers (x1000)")
    
    
    return(p)
  })
  
  output$text2 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered3 <- reactive({
    db <- male.data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1)
    return(db)
  })
  
  output$plot3 <- renderPlot({
    
    p <- ggplot(data = filtered3(), mapping = aes(x = Occupation, y = M_workers, fill=M_weekly)) +
      geom_bar(stat="identity") + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) + 
      ggtitle("Male Data with # of workers & pay rate") + scale_fill_gradient(low = "yellow", high = "red") + 
      ylab("Number of Male Workers (x1000)")
    
    
    return(p)
  })
  
  output$text3 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered4 <- reactive({
    db <- bottom.ten.percent.difference
    return(db)
  })
  
  output$plot4 <- renderPlot({
    
    p <- ggplot(data = filtered4(), mapping = aes(x = Occupation, y = diff, fill=All_workers)) +
      geom_bar(stat="identity") + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) + 
      ggtitle("Bottom 10% of Wage Gap") + scale_fill_gradient(low = "yellow", high = "red") +
      ylab("Difference in Median Wage (USD)")
    
    
    return(p)
  })
  
  output$text4 <- renderText({
    paste0("sdasdasaff")
  }) 
  
  filtered5 <- reactive({
    db <- top.ten.percent.male.difference
    return(db)
  })
  
  output$plot5 <- renderPlot({
    
    p <- ggplot(data = filtered5(), mapping = aes(x = Occupation, y = diff, fill=(M_workers/All_workers))) +
      geom_bar(stat="identity") + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) + 
      ggtitle("Top 10% of Male Favored Gap") + scale_fill_gradient(low = "yellow", high = "red") + 
      ylab("Difference in Median Wage (USD)")
    
    
    return(p)
  })
  
  output$text5 <- renderText({
    paste0("sdasdasaff")
  })
  
  filtered6 <- reactive({
    db <- top.ten.percent.female.difference
    return(db)
  })
  
  output$plot6 <- renderPlot({
    
    p <- ggplot(data = filtered6(), mapping = aes(x = Occupation, y = abs(diff), fill=(F_workers/All_workers))) +
      geom_bar(stat="identity") + theme(axis.text = element_text(size=12, angle=-30, hjust=0), 
                                        axis.title = element_text(size=15), plot.title = element_text(size=20)) + 
      ggtitle("Top 10% of Female Favored Gap") + scale_fill_gradient(low = "yellow", high = "red") + 
      ylab("Difference in Median Wage (USD)")
    
    
    return(p)
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