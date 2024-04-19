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
  ns <- NS(id)

tagList(

  tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');

        body {
          background-color: #E9E4E3;
          color: #000000; /* text color */
          font-family: 'ITC Cheltenham Std Light Condensed', serif; /* Ensure a fallback font is provided */
        }

        h2 {
          font-family: 'Roc Grotesk Bold Condensed', sans-serif; /* Ensure a fallback font is provided */
        }

        /* Change the navbar panel */
        li a {
          background-color: #000000; /* background color */
          color: white; /* text color */
        }

        .navbar {
          background-color: #000000;
          min-height: 80px;
        }

        .navbar-brand {
          padding: 0 15px;
          height: 80px;
          line-height: 80px;
        }

        .navbar-toggle {
          /* (80px - button height 34px) / 2 = 23px */
          margin-top: 23px;
          padding: 9px 10px !important;
        }

        @media (min-width: 768px) {
          .navbar-nav > li > a {
            /* (80px - line-height of 27px) / 2 = 26.5px */
            padding-top: 26.5px;
            padding-bottom: 26.5px;
            line-height: 27px;
          }
        }
      "))
    ),
    navbarPage(
      id = ns('fsst_overview'),
      title = "Financial Stress Test Tools",
      theme = shinythemes::shinytheme("flatly"),
      tabPanel("Repository Tool",
        fluidRow(
          column(12, 
            column(4, h2('The Inputs'), p('Explore three different scenarios and their shared indicators.')),
            column(8, forceNetworkOutput(ns("repository_scatter")))
          ),
          column(12, 
            column(4, h2('The Outputs'), p('See how different organizations express financial impacts.')),
            column(8, plotlyOutput(ns("repository_bar")))
          ),
          br(),
          column(12, h2('The Data'), p('Search and compare data in a table format.'), DT::dataTableOutput(ns("repository_overview"))),
          column(12, align = 'center', downloadButton(ns("downloadData"), 'Download Filtered Data'))
        )
      ),
      tabPanel("Methodology",
        fluidRow(column(12, includeMarkdown('app/logic/methodology.md')))
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
  banks <- list('Banque de France', 'De Nederlandsche Bank', 'Bank of England', 'Bank of Canada')
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
            column(4, selectInput(session$ns("provider_single"), NULL, choices = banks, selected = 'Bank of England')),
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
                     column(12, selectInput(session$ns('x_axis_left'), 'Horizontal', choices = list('Year'))),
                     column(12, selectInput(session$ns('y_axis_left'), 'Vertical', choices = list('GDP growth', 'inflation/ consumer prices level')))),#, 'carbon price', 'end user price coal')))),
              column(8,
                     plotlyOutput(session$ns('chart_left_single')))
            )),
          column(12, fluidRow(
            fluidRow(
              column(4,
                     column(12, selectInput(session$ns('x_axis_right'), 'Horizontal', choices = list('GDP growth', 'inflation/ consumer prices level'), selected = 'GDP growth')),#, 'carbon price', 'end user price coal'))),
                     column(12, selectInput(session$ns('y_axis_right'), 'Vertical', choices = list('inflation/ consumer prices level', 'GDP growth'), selected = 'inflation/ consumer prices level'))),#, 'carbon price', 'end user price coal')))),
              column(8,
                     plotlyOutput(session$ns('chart_right_single')))
            )
          )
          )
        )
      )
      } else {
        fluidRow(
          column(12, fluidRow(
            column(4,
                   selectInput(session$ns('provider_total_1'),
                               'Select two providers',
                               choices = banks,
                               selected = 'Banque de France'),
                   selectInput(session$ns('provider_total_2'),
                               NULL,
                               choices = banks,
                               selected = 'Bank of England'),
                   # selectInput('geography_total',
                   #             'Select a geography',
                   #             choices = list('Netherlands', 'Global', 'France', 'US')),
                   selectInput(session$ns('indicator_total'),
                               'Select an indicator',
                               choices = list('GDP growth', 'inflation/ consumer prices level'), #'carbon price', 'end user price coal'),
                               selected = 'inflation/ consumer prices level')),
            column(8,
                   plotlyOutput(session$ns('chart_left_double'))
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
        } else if (input$provider_total_1 == 'Bank of Canada') {
          provider_1 <- 'BoC'
        } else {
          provider_1 <- 'NGFS'
        }

      if (input$provider_total_2=='Banque de France') {
        provider_2 <- 'BdF'
      } else if (input$provider_total_2=='De Nederlandsche Bank') {
        provider_2 <- 'DNB'
      } else if (input$provider_total_2=='Bank of England') {
        provider_2 <- 'BoE'
      }  else if (input$provider_total_1 == 'Bank of Canada') {
        provider_1 <- 'BoC'
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
      } else if (input$provider_single=='Bank of Canada') {
        provider_value <- 'BoC'
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
          x = stats::as.formula(paste("~", variable_x)),
          y = stats::as.formula(paste("~", variable_y)),
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
      } else if (input$provider_single=='Bank of Canada') {
        provider_value <- 'BoC'
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
        x = stats::as.formula(paste("~", variable_x)),
        y = stats::as.formula(paste("~", variable_y)),
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
    
    # Ensure that 'dataset_repository' is defined as a reactive expression that fetches your data
    dataset_repository <- reactive({
      # Your data fetching logic here
      repository_data
    })
    output$repository_overview <- DT::renderDataTable({
      # Assuming 'dataset_repository()' is a reactive expression that returns your dataset
      DT::datatable(
        dataset_repository(),
        options = list(server = TRUE),  # Enable server-side processing
        filter = 'top'
      )
    })
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
