box::use(
    shiny[...],
    shinythemes[...],
    tidyverse[...],
    fs[...],
    reactlog[...],
    DT[...],
    janitor[...],
    plotly[...],
    networkD3[...],
    shinyWidgets[...],
    shinyjs[...],
    r2dii.colours[...],
    readr[...],
    dplyr[...],
    tidyr[...],
    shiny[bootstrapPage, div, moduleServer, NS, renderUI, tags, uiOutput],
    app/logic/constant[...],
    app/logic/helpers[...],
    app/logic/plot_timeseries_one_provider[...],
    app/logic/plot_timeseries_two_providers[...]
)

#' @export
ui <- function(id) {
  tagList(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "colors.css")
  ),
  
  navbarPage(
    
    id = 'fsst_overview',
    title = div(
      div(
        img(src = 'static/2DII-Logo-White150px.png',
            style = 'position:absolute;right:15px;bottom:15px;top:5px;z-index:1000000')
      )),
    
    # FSST Home
    tabPanel("Home", 
             fluidRow(
               column(12, align = 'center',
                      actionButton('to_repo', 'To the Stress Test Repository', icon("arrow-alt-circle-right"), 
                                   style = "color: #E9E4E3; background-color: #F53D3F; border-color: #E9E4E3; display:center-align; padding:10px; font-size: 150%")),
               column(12,
                      img(src = 'static/Jenga - lighter.jpg', width = '100%'))
               )),
    
    # FSST Repository
    tabPanel("Repository Tool", 
             h1('Repository Tool'),
             p('This is your place to go for comparing climate stress testing scenarios, as well along approaches as to their final outcome.'),
             fluidRow(
               column(12, 
                      column(4, 
                             h2('The inputs'),
                             p('In this repository, we hold three scenarios. They share the following indicators.'),
                             p('Please explore the net in order to see which providers we currently feature (colored nodes),
                        and which indicators they share (grey nodes).')), 
                      column(8, forceNetworkOutput("repository_scatter"))),
               column(12, 
                      column(4, 
                             h2('The outputs'),
                             p('While the scenarios and providers share some input information, they share very little output indicators,
                               that is indicators how the stress test outcome is denominated.'),
                             p('De Nederlandsche Bank (DNB) expresses financial impact in terms of changed equity returns.'),
                             p('Banque de France (BdF) expresses those financial impacts in terms of changes in costs of capital.'),
                             p('Harmonizing those indicators would be a major contribution to make scenarios comparable across providers.')),
                      column(8, plotlyOutput("repository_bar"))),
               br(),
               br(),
               column(12, fluidRow(
                 column(4, 
                        h2('The comparison'),
                        p('This section gives you the opportunity for comparison - either across scenarios and indicators of one provider or across two prodiders.'),
                        p('Please choose in the box below.')))),
                 column(12, fluidRow(
                   column(4, p('What would you like to do?')),
                   column(4, selectInput('provider_choice', label = NULL, choices = list('Compare two providers', 'Focus on one provider'))),
                   column(4, '')
                 )),
                 br(),
                 br(),
                 column(12, fluidRow(
                   uiOutput('one_or_two_providers')
                 ))
                ),
             br(),
             fluidRow(
               column(4, 
                      h2('The data'),
                      p('In this last section of the Stress Test Repository, you get to search and compare data in a table format.'),
                      p('You can download information about the various scenarios according to your selection.'))
             ),
             br(),
             fluidRow(column(12, DT::dataTableOutput("repository_overview"))),
             fluidRow(column(12, align = 'center', downloadButton('downloadData', 'Download filtered data')))
             ),
    
    # FSST Scenario configurator
    tabPanel("Scenario configurator",
             fluidRow(column(12, 'Under construction', align = 'center'),
                      column(12, align = 'center',
                             img(src = 'static/elvir-k-Xtyh5b5GGX4-unsplash.jpg', width = 500)))),
    
    # FSST Portfolio tool 
    tabPanel("Portfolio Tool", 
             fluidRow(column(12, 'Under construction', align = 'center'),
                      column(12, align = 'center',
                             img(src = 'static/elvir-k-Xtyh5b5GGX4-unsplash.jpg', width = 500)))), 
    
    # Methodology
    tabPanel("Methodology", 
             fluidRow(column(12, includeMarkdown('static/methodology.Rmd')))
             ),
    
    # About us 
    tabPanel("About 2° Investing Initiative",
             fluidRow(column(12, includeMarkdown('static/2dii.Rmd')))
             ), 
    
    # Footer -------------------------------
    windowTitle = 'The FSST Platform ',
    footer = 
      div(style = "margin-top:15px;float:right;",
          fluidRow(
            column(4, p('')),
            column(3, p('This project received funding from EIT Climate-KIC.')),
            column(5, img(src = 'static/EIT-CKIC-Logo_Transparent_Standard.png', height = 100)))
          )
    )
  )

}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
      
  observeEvent(input$to_repo, {
    updateTabsetPanel(session, "fsst_overview",selected = "Repository Tool")
  })
  
    # # Load helpers
    # 
    # source('helpers.R')
  banks <- list('Banque de France', 'De Nederlandsche Bank', 'Bank of England')
  banks <- stats::setNames(banks, banks)
  
    output$repository_bar <- renderPlotly({
      
      boxplot_data %>% 
        get_data_box(.) %>% 
        plot_box_per_provider(y_axis_title = list('Change in cost of capital (%)', 'Change in equity value'))
      # TODO Facets for the provider better as we currently have different indicators and units!
      # TODO Fix the x-axis label: It is given per sector_group_type, not provider 
      
   })
    
    output$repository_scatter <- renderForceNetwork({
      
      create_network_chart(repository_data)

    })
    
    # The second row of output depends on choice of one or two providers
    output$one_or_two_providers <- renderUI({
      
      if (input$provider_choice=='Focus on one provider') {
        
        fluidRow(
          column(12, fluidRow(
            column(4, 'Select provider'),
            column(4, selectInput('provider_single', NULL, choices = banks, selected = 'Bank of England')),
            column(4, '')
            )),
          # column(12, fluidRow(
          #   column(4, 'Select geography'),
          #   column(4, selectInput('geography_single', NULL, choices = list('Netherlands', 'Global', 'France', 'US'))),
          #   column(4, '')
          #   )), 
          column(12, fluidRow(
            fluidRow(
              
              column(4, 
                     column(12, selectInput('x_axis_left', 'Horizontal', choices = list('Year'))),
                     column(12, selectInput('y_axis_left', 'Vertical', choices = list('GDP growth', 'inflation/ consumer prices level')))),#, 'carbon price', 'end user price coal')))),
              column(8, 
                     plotlyOutput('chart_left_single'))
            )),
          column(12, fluidRow(
            fluidRow(
              column(4, 
                     column(12, selectInput('x_axis_right', 'Horizontal', choices = list('GDP growth', 'inflation/ consumer prices level'), selected = 'GDP growth')),#, 'carbon price', 'end user price coal'))),
                     column(12, selectInput('y_axis_right', 'Vertical', choices = list('inflation/ consumer prices level', 'GDP growth'), selected = 'inflation/ consumer prices level'))),#, 'carbon price', 'end user price coal')))),
              column(8, 
                     plotlyOutput('chart_right_single'))
            )
          )
          )
        )
      )
        
      } else {
        fluidRow(
          column(12, fluidRow(
            column(4, 
                   selectInput('provider_total_1', 
                               'Select two providers', 
                               choices = banks, 
                               selected = 'Banque de France'),
                   selectInput('provider_total_2', 
                               NULL,
                               choices = banks, 
                               selected = 'Bank of England'),
                   # selectInput('geography_total', 
                   #             'Select a geography', 
                   #             choices = list('Netherlands', 'Global', 'France', 'US')),
                   selectInput('indicator_total',
                               'Select an indicator', 
                               choices = list('GDP growth', 'inflation/ consumer prices level'), #'carbon price', 'end user price coal'),
                               selected = 'inflation/ consumer prices level')),
            column(8, 
                   plotlyOutput('chart_left_double')
                   )
          ))
        )
      }
    })
    
    output$chart_left_double <- renderPlotly({

       if (input$provider_total_1=='Banque de France') {
          provider_1 <- 'BdF'
        } else if (input$provider_total_1=='De Nederlandsche Bank') {
          provider_1 <- 'DNB'
        } else if (input$provider_total_1=='Bank of England') {
          provider_1 <- 'BoE'
        } else {
          provider_1 <- 'NGFS'
        }
      
      if (input$provider_total_2=='Banque de France') {
        provider_2 <- 'BdF'
      } else if (input$provider_total_2=='De Nederlandsche Bank') {
        provider_2 <- 'DNB'
      } else if (input$provider_total_2=='Bank of England') {
        provider_2 <- 'BoE'
      } else {
        provider_2 <- 'NGFS'
      }
      
      level_prov1 <- unique((repository_data %>% filter(provider == provider_1))$scenario)
      level_prov2 <- unique((repository_data %>% filter(provider == provider_2))$scenario)
      
      # TODO: make sure that the order is right here, I think that we need to fix it
      level_prov1 <- unique((repository_data %>% filter(provider == provider_1))$scenario)
      level_prov2 <- unique((repository_data %>% filter(provider == provider_2))$scenario)
      
      fig <- plot_timeseries_two_providers(
        data = repository_data %>%
          filter(!is.na(scenario)), 
        provider1 = provider_1, 
        provider2 = provider_2, 
        indicator = input$indicator_total,
        geographies = unique(repository_data$geography),
        scenario_order1 = level_prov1,
        scenario_order2 = level_prov2
        )
      
      print(fig)
    })
    
    
    output$chart_left_single <- renderPlotly({
      
      if (input$provider_single=='Banque de France') {
        provider_value <- 'BdF'
      } else if (input$provider_single=='De Nederlandsche Bank') {
        provider_value <- 'DNB'
      } else if (input$provider_single=='Bank of England') {
        provider_value <- 'BoE'
      } else {
        provider_value <- 'NGFS'
      }
      
      if (input$x_axis_left=='Year') {
        level_prov <- unique((repository_data %>% filter(provider == provider_value))$scenario)
        fig <- plot_timeseries_one_provider(
          data = repository_data %>%
            filter(!is.na(scenario)), 
          provider = provider_value, 
          indicator = input$y_axis_left, 
          scenario_order = level_prov, 
          geographies = unique(repository_data$geography))
      } else {
        variables <- c(input$x_axis_left, input$y_axis_left)
        data_GDP <- repository_data %>% 
          ungroup() %>% 
          filter(provider == provider_value,
                 matching_indicator %in% variables) %>% 
          select(converted_value, provider, matching_indicator, year, scenario) %>% 
          distinct() %>% 
          pivot_wider(id_cols = -c(matching_indicator, converted_value), 
                      names_from = matching_indicator, 
                      values_from = converted_value) %>% 
          clean_names()
        
        non_variables <- c('provider', 'scenario')
        variables <- setdiff(names(data_GDP), non_variables)
        
        variable_x <- variables[1]
        variable_y <- variables[2]
        
        fig <- plot_ly(
          data_GDP,
          x = as.formula(paste("~", variable_x)),
          y = as.formula(paste("~", variable_y)),
          color = ~scenario,
          type = "scatter",
          mode = "lines", 
          colors = "Set1") %>% 
          layout(
            title = provider_value, 
            xaxis = list(title = stringr::str_to_title(variable_x)), 
            yaxis = list(title = stringr::str_to_title(variable_y)), 
            paper_bgcolor='#E9E4E3',
            plot_bgcolor='#E9E4E3'
          )
      }
      
      print(fig)
      
    })
    
    output$chart_right_single <- renderPlotly({
      
      
      if (input$provider_single=='Banque de France') {
        provider_value <- 'BdF'
      } else if (input$provider_single=='De Nederlandsche Bank') {
        provider_value <- 'DNB'
      } else if (input$provider_single=='Bank of England') {
        provider_value <- 'BoE'
      } else {
        provider_value <- 'NGFS'
      }
      
      variables <- c(input$x_axis_right, input$y_axis_right)
      
      data_GDP_CPI <- repository_data %>% 
        ungroup() %>% 
        filter(provider == provider_value,
               matching_indicator %in% variables) %>% 
        select(provider, matching_indicator, converted_value, matching_indicator, year, scenario) %>% 
        distinct() %>% 
        pivot_wider(id_cols = c(provider, year, scenario), 
                    names_from = matching_indicator, 
                    values_from = converted_value) %>% 
        clean_names()
      
      non_variables <- c('provider', 'scenario', 'year')
      variables <- setdiff(names(data_GDP_CPI), non_variables)
      
      variable_x <- variables[1]
      variable_y <- variables[2]
      
      fig2 <- plot_ly(
        data_GDP_CPI,
        x = as.formula(paste("~", variable_x)),
        y = as.formula(paste("~", variable_y)),
        color = ~scenario,
        type = "scatter",
        mode = "markers", 
        colors = 'Set1'
      ) %>% 
        layout(
          title = provider_value, 
          xaxis = list(title = stringr::str_to_title(variable_x)), 
          yaxis = list(title = stringr::str_to_title(variable_y)), 
          paper_bgcolor='#E9E4E3',
          plot_bgcolor='#E9E4E3'
        )
      
      print(fig2)
      
    })
    

    dataset_repository <- reactive({
      repository_data
    })
    data <- repository_data %>% 
      ungroup() %>% 
      select(provider, scenario, year, year_count, matching_indicator, converted_value, matching_unit, geography, subset) %>% 
      clean_names(case = 'title') %>%
      mutate(across(is.character, as.factor)) %>% 
      mutate(across(matches("Year"), as.factor))
    col_filter <- list(position = "top", clear = FALSE, plain = TRUE)
    output$repository_overview <- DT::renderDataTable(data, server = FALSE, filter = col_filter)
      # formatStyle(background = '#E9E4E3')

    # Download the filtered data 
    
    output$downloadData <- downloadHandler(
        filename = function() {
          paste('data-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          indices <- input$repository_overview_rows_all
          filtered <- data[indices, , drop = FALSE]
          readr::write_csv(filtered, con)
        }
      )
  })
}
