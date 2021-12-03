function(input, output) {
  
  # PAGE 1
  
  output$plot2 <- renderPlotly({
    country_rat <- decade %>% 
      group_by(country) %>% 
      summarise(total_suicides = sum(suicides_no),
                pop = sum(population)) %>% 
      mutate(ratio = (total_suicides / pop)*100) %>% 
      arrange(desc(ratio)) %>% 
      top_n(20)
    country_rat_plot <- country_rat %>% 
      ggplot(aes(x = ratio,
                 y = reorder(country, ratio),
                 text = glue("{country}
                             Ratio: {round(ratio, 4)}
                             Number of suicides: {comma(total_suicides)}
                             Population: {comma(pop)}
                             "))) +
      geom_col(aes(fill = ratio)) +
      scale_fill_gradient(low = 'darkgrey', high = 'firebrick') +
      labs(x = NULL,
           y = NULL,
           title = 'Suicides ratio per population') +
      theme_minimal() +
      theme(legend.position = "none")
    ggplotly(country_rat_plot, tooltip = "text")
  })
  
  output$plot1 <- renderPlotly({
    country_num_plot <- country_rat %>% 
      ggplot(aes(x = total_suicides,
                 y = reorder(country, total_suicides),
                 text = glue("{country}
                         Number of suicides: {comma(total_suicides)}
                         Population: {comma(pop)}
                         Ratio: {round(ratio, 4)}
                         "))) +
      geom_col(aes(fill = total_suicides)) +
      scale_fill_gradient(low = 'darkgrey', high = 'firebrick') +
      labs(x = NULL,
           y = NULL,
           title = 'Suicide number by countries') +
      scale_x_continuous(label = comma_format()) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(country_num_plot, tooltip = "text")
  })
  
  output$plot3 <- renderPlotly({
    sex <- decade %>% 
      group_by(year, sex) %>% 
      summarise(suicides_no = sum(suicides_no))
    
    line = c("grey", "firebrick")
    
    sex_plot <- ggplot(sex, aes(
      x = year,
      y = suicides_no,
      color= sex,
      group = sex,
      text = glue("Sex: {sex}
              Year: {year}
              Total of suicides: {comma(suicides_no)}"))) +
      geom_line(size=1) + 
      scale_color_manual(values = line) +
      labs(x = NULL,
           y = NULL,
           title = 'Total of suicides based on sex') +
      scale_y_continuous(label = comma_format()) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(sex_plot, tooltip = "text")
  })
  
  output$plot_gen <- renderPlotly({
    gen <- decade %>% 
      group_by(generation) %>% 
      summarise(sum(suicides_no)) %>%
      rename('suicides_total' = 'sum(suicides_no)') %>% 
      arrange(desc(suicides_total))
    
    gen_plot <- gen %>%
      ggplot(aes(x = suicides_total,
                 y = reorder(generation, suicides_total),
                 text = glue("{comma(suicides_total)}"))) +
      geom_col(aes(fill=suicides_total)) +
      scale_fill_gradient(high = "firebrick", low = "grey", name = "Total suicides") +
      labs(x = NULL,
           y = NULL,
           title = 'Total of suicides based on generations',
           subtitle = 'from 1995-2015') +
      scale_x_continuous(label = comma_format()) +
      theme_minimal()
    
    ggplotly(gen_plot, tooltip = "text")
  })
  
  # PAGE 2
  
  output$plot4 <- renderPlotly({
    single_country <- decade %>% 
      filter(country == input$country_perf1) %>% 
      group_by(year) %>% 
      summarise(Total.suicides = sum(suicides_no),
                Population = sum(population),
                Ratio = (Total.suicides / Population)*100)
    
    single_plot <- single_country %>% 
      ggplot(aes_string(x = "year", y = input$multiple_choices)) +
      geom_line(color = "salmon", size=1) +
      geom_point(color = "black") +
      theme_minimal() + 
      labs(title = glue("{input$multiple_choices} in {input$country_perf1}"),
           x = NULL,
           y= NULL)
    
    ggplotly(single_plot)
  })
  
  output$plot5 <- renderPlotly({
    age.group <- decade %>% 
      filter(year == input$country_perf3,
             country == input$country_perf2) %>% 
      group_by(year, sex, age) %>% 
      summarise(total.suicides = sum(suicides_no),
                Population = sum(population),
                Ratio = (total.suicides / population)*100)
    
    age.plot <- age.group %>% 
      arrange(Ratio) %>%
      mutate(name = factor(age,
                           levels = c("5-14 years", "15-24 years", "25-34 years",
                                      "35-54 years", "55-74 years", "75+ years"))) %>%
      ggplot(aes(x = name, y = Ratio, text = glue("Age group: {name},
                                              Ratio: {round(Ratio, 4)}"))) +
      geom_col(aes(fill = sex), position = "stack") +
      scale_fill_manual(values = c("grey", "firebrick")) +
      labs(x = NULL,
           y = "Ratio",
           title = glue("Suicide ratio in {input$country_perf2} ({input$country_perf3})"),
           subtitle = "Based on age group") +
      theme_minimal()+
      theme(legend.position = "top")
    
    ggplotly(age.plot, tooltip = "text")
    
  })
  
  
  # PAGE 3
  
  output$plot_maps <- renderPlotly({
    mapping <- read_csv("country code update.csv")
    
    decade2 <- decade %>% 
      inner_join(mapping, decade, by = "country") %>% #inner_join(data1, data2, by = "ID")
      select(country, year, suicides_no, population, Code)
    
    decade_new <- decade2 %>% 
      group_by(country, year, Code) %>% 
      summarise(total_suicides = sum(suicides_no),
                total_population = sum(population),
                ratio = (total_suicides / total_population)*100) %>% 
      mutate(Ratio = round(ratio, 4))
    
    mapping_graph <- plot_geo(decade_new,
                              locationmode = 'country names',
                              frame = ~year) %>%
      add_trace(locations = ~country,
                z = ~Ratio,
                zmin = 0,
                zmax = max(decade_new$ratio),
                color = ~Ratio,
                colorscale = 'Inferno') %>%
      layout(geo = list(scope = "world"))
    
    mapping_graph
  })
  
  output$data <- renderDataTable({
    DT::datatable(data = decade,
                  options = list(scrollX = T,
                                 pageLength = 5,
                                 lengthMenu = c(5, 10, 15, 20)))
  })
}