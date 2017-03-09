library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
source("data.R")

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    x <- input$occupation1
    
    updateCheckboxGroupInput(session, "occupation1",
                             selected = c("MANAGEMENT", "BUSINESS")
    )
    
    updateCheckboxGroupInput(session, "occupation2",
                             selected = character(0)
    )
    
  })
  
  filtered <- reactive({
    db <- data %>%
      filter(Occupation %in% input$occupation2 | Occupation %in% input$occupation1) %>% 
      filter(diff > input$diff.choice[1] & diff < input$diff.choice[2])
    return(db)
  })

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
               marker = list(colorbar = list(title = "Wage Gap")),
               text = ~paste("Wage Gap: ", diff)) %>% 
      layout(title = "Overall Data w/ Differences", xaxis = x_axis_format, yaxis = y_axis_format)
  
    return(g)
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
    
    g<-plot_ly(filtered2(), type="bar", x = ~Occupation, y = ~F_workers, color = ~F_weekly,
               marker = list(colorbar = list(title = "Median Weekly Wage")),
               text = ~paste("Median wage for Females: $", F_weekly)) %>% 
      layout(title = "Female Data with # of workers & pay rate", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
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
    

    y_axis_format <- list(
      title = "Amount of Male Workers in Occupation (x1000)",
      titlefont = list(size=12, weight = "bold")
      
    )

    
    g<-plot_ly(filtered3(), type="bar", x = ~Occupation, y = ~M_workers, color = ~M_weekly,
               marker = list(colorbar = list(title = "Median Weekly Wage")),
               text = ~paste("Median wage for Males: ", M_weekly)) %>% 
      layout(title = "Male Data with # of workers & pay rate", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
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
               marker = list(colorbar = list(title = "Amount of Workers")),
               text = ~paste("Amount of workers: ", All_workers)) %>% 
      layout(title = "Bottom 10% of Wage Gap", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
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
               marker = list(colorbar = list(title = "% Male Workers")),
               text = ~paste("Percentage of Male Workers: ", (M_workers/All_workers)))%>% 
      layout(title = "Top 10% of Male Favored Gap", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    
    return(g)
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
               marker = list(colorbar = list(title = "% Female Workers")),
               text = ~paste("Percentage of Females workers: ", (F_workers/All_workers))) %>% 
      layout(title = "Top 10% of Female Favored Gap", xaxis = x_axis_format, yaxis = y_axis_format)
    
    
    return(g)
  })
  
  output$plot7 <- renderPlot ({
    h <- ggplot(data = wage.filtered(),aes(x = TIME, y = Value, colour = Value))+
      geom_line()+
      geom_point()+
      theme_light()+
      labs(title = "Change in Wage Gap Over Time", x = "Time (Year)", y = "Wage Gap", colour = "Wage Gap")
    return(h)
  })
  
  output$table <- renderDataTable({
    filtered.data.frame<-filtered()
    colnames(filtered.data.frame)<-c("Occupation", "Number of all workers(x1000)","Median Salary of All Workers",
                                     "Number of Male workers(x1000)","Male Median Salary","Number of Female workers(x1000)", "Female Median Salary",
                                     "Difference of Male to Female Wage", "Wage Gap Gender Favor")
    return(filtered.data.frame)
  })
  
  output$table2 <- renderDataTable({
    filtered.data.with.time<-wage.filtered()
    return(filtered.data.with.time)
  })
  
  output$text <- renderText({
    string <- function(input.wage) {
      if(is.null(input.wage))
        return("Hover over Time Wage Plot")
        paste("Year = ", round(input.wage$x,0), "Wage Gap = ", round(input.wage$y,2))
    }
    paste0("Year and Gap Information:", string(input$hover))
  })
  

  #PARAGRAPHS HERE 
  output$introductionText <- renderText({
    paste("A difference in pay between genders has been a growing social issue in our modern day. With both sides
           holding strong feelings for their preference, it's important to take a step back and analyze the data related
           to this specific topic. In our report, we've collected information across two separate databases to conduct a 
           professional and organized report showing trends and key details the pertain to this wage gap issue.", 
           
           "The first of the two datasets was collected from the Bureau of Labor Statistics. Jean-Phillipe reformatted
           the original pdf report into a usable .csv data file. After extensive cleansing, it provided a respectable amount 
           of data for our purpose. To compliment the information provided by this dataset, we decided to add another dimension
           to our analysis and incorporate time. Specifically, this information allowed us to compare the trend of the wage gap
           between genders over time. In addition, we were able to apply a global scope as this new dataset provided information
           on other countries.",
           
           "After a thorough work through and breakdown of all the data we collected, our group was able to make educated inferences
          and realizations. As a whole, it's safe to say there is a wage preference for men as compared to women within the U.S. Although, it
          was very interested to see the other side of the argument and take a look at which occupations actually gave women the higher wage!
          The data we collected and presented provides a strong amount of data to support the claim that the wage gap between genders is a 
          ongoing issue, but is actively improving across the globe and needs a larger focus on specific occupations. ")
    
  })
    
  
  plot1text<-reactive({
    paragraph<-paste0("For a general introduction, we wanted to show the general current state of the occupations selected. In the visual below, you can see
                      the amount of workers in relation to the occupation. In addition, the color of each bar represents the degree of difference within the
                      wage for males as compared to females. A brighter yellow represents a high wage difference while a darker purple indicates a lower difference.
                      The current range of acceptable wage gap difference values is from ", input$diff.choice[1], " to ", input$diff.choice[2],". Please hover over 
                      any of the bars to show the specific occupation as well as the details on the wage gap for that field!")
    return(paragraph)
  })
  
  output$plotText1<- renderText({
    return(plot1text())
  })
  
  output$plotText2 <- renderText({
    paste0("Now to get a better understanding of how this wage gap actually breaks down, we can look at the salary information specifically
           for males and females in the specific occupations. The information presented below is collected from the U.S database and provides
           median weekly salaries for women working in each field. To begin, the visual below shows the data for females; relating the occupation
           to the amount of females (in thousands) within that field. This also includes each bar color coordinated to represent the weekly median salary 
           in US dollars. This gives a better idea on the information collected for females, strengthening the understanding of the wage gap.")
  })
  
  output$plotText3 <- renderText({
    paste0("Similar to the previous visual, the plot below is a representation of the filtered male data for each occupation. As before, you're able
           to see the median wage for men in each selected field. In addition, each bar's height is made up of the amount of male workers in that field
           multiplied by a factor of 1000. The filled color for each of the bar shows the median wage each male is being paid in that field. 
           Comparing this plot with the previous gives us a good interpretation of how each gender is compensated.")
  })
  
  output$plotText4 <- renderText({
    paste0("As with any analysis of a critical issue, it's very important to label specific outliers and bring them to light. Our report fulfills this need
           as well, narrowing our data to highlight the least and most severe wage gaps across all of the occupations. Down below, we can see the plot that
           portrays the occupations that make up the bottom 10% of the salary difference between genders. These specific fields have done a good job in treating
           their employees equally and should continue to set an example for the rest. Specifically, the plot shows the difference in median wages and has the 
           bars color coded to represent the total number of works within that field. Also, you may notice that some of the bars go below 0; this means that the 
           gap actually favors women rather than men! ")
  })
  
  output$plotText5 <- renderText({
    paste0("Now that we've seen the better side of things in our current situation with the wage gap in America, it's time to look at the areas of improvement. 
           Analyzing which occupations require more attention will not only make the process of reducing the gap more efficient, it will also allow companies 
           to improve on their weaknesses. The visual representation below shows the occupations that made up the top 10% of the wage gap that preferred males
           over females. Also, the height of the bars shows the amount of the difference while the color represents the proportion of males to females in each
           of the occupations. As usual, make sure to hover over any of the bars to get specific figures pertaining to that job field")
  })
  
  output$plotText6 <- renderText({
    paste0("Contrary to the plot above, the visual below shows a view of the wage gap that's mostly overlooked. Using a high amount of filtering and parsing, 
           we were able to construct a bar graph that highlights occupations that actually pays females higher than males in their field. Although, both the 
           frequency and difference amount is far less than the top 10% of male preferred occupations, it is important to not overlook these occupations when 
           discussing the idea of wage equality. The goal of reaching an ultimately equal salary across both genders begins with recognizing the issues both 
           sides present. ")
  })
  
  output$plotText7 <- renderText({
    paste0("Now it's time to incorporate a factor of time into the data. A bit different from all of the previous visuals, the graph is collected on data collected
           from countries around the world. This mean you're able to see the change in overall wage gap as time progresses for any preferred country. Please select 
           the region you would like visualized within the widget options above! Specifically, this line graph shows the change of the value of the wage gap in relation to time. 
           The x axis represents the time in years, while the y axis represents the wage gap as a percentage of the earnings of men. As you can see, in the case of United states, w
           age gap reached its lowest value in 2008. It has been decreasing over time which reflects a positive change in our society.")
  })
  
  output$conclusionText1 <- renderText({
    paste0("The data has been presented and the visuals have been shown, but it's time to piece it all together. It's clear that we can see which occupations present 
           the highest and lowest wage gaps within America, but we can gain a deeper understanding of the information by understand trends shown by the data. Specifically, 
           occupations that usually had a higher number of male workers showed a wage preference to males. The majority of these fields included STEM, Business, 
           or security related occupations. In addition, it's important to highlight the absurdly higher amount of occupations that payed men more than women. There
           were less than 10 occupations that gave the gap preference to women; and those who did usually kept the gap to a minimal amount.")
  })
  
  output$conclusionText2<- renderText({
    paste0("To make progress towards solving a major social issue like this, it's important to remember actions speak louder than words. Our idea behind putting a spotlight
           on the occupations with the highest and lowest wage gaps was to help develop a plan for improvement. By knowing where the attention should be centered, it assures 
           that resources will not be wasted and encourages an effective strategy. For example, focusing on an occupation that already has a low salary difference would be 
           wasting time and ultimately does not make much of a difference in the overall scope of the issue. Rather, keying in on the major factors that the enlarge the overall
           issue of a wage gap would be much more effective. Taking a step back from only the U.S, the last visual gives a good idea on data from other countries and also 
           introduces the idea of time. This was critical to include in our report, enhancing the comprehension of a country's improvement on this specific social issue. As noted
           before, the U.S has done a lot to help narrow this gap but there is still a pattern that tends to jump around. Although there is a general trend of a decrease for the wage
           gap, a more specific focus on areas of improvement would prove to help this growth greatly. To sum it all up, there is an overall sense of compensation inequality within 
           the U.S, and other countries as well, but a lot of progress has been made over the years and our society will continue to push towards equality. ")
  })
  
  output$tableText<- renderText({
    paste0("A nice compliment to the visuals in this report is a table showing the data we worked with. Below, we've included an organized format of the table used to 
           conduct this report. The columns include occupation, number of workers (overall, and by gender), as well as wage information. All of the salary information 
           provided is in US dollars and the amount of workers is in the thousands. Please feel free to search for specific entries using the search bar, or narrow the
           amount of results by changing the rows per page! ")
  })
  
  reactiveTableText<-reactive({
    text<-paste0("Along with the previous data table showing the information used to make calculations about specific occupations in the U.S, this dataset was used to make the 
           visual representing wage change over time. The current country being shown is ", input$time.wage, " and includes the values of the wage gap as well as the year
                 associated with the data. You can search for specific years using the search bar or change how many entries you'd like to see by adjusting the option 
                 on the left. ")
    return(text)
  })
  
  output$tableText2<- renderText({
    return(reactiveTableText())
  })
  
  
}

shinyServer(server)
