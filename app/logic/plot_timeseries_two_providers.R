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
    dplyr[...],
    app/logic/plot_timeseries_one_provider[...]
)

plot_timeseries_two_providers <- function(
  data, 
  provider1, 
  provider2, 
  indicator,
  scenario_order1 = c("Early Action", "No Additional Action", "Late Action"),
  scenario_order2 = c("Sudden", "Orderly", "Late"),
  geographies = c("RoW", "rest of world")
  ) {
  
  fig1 <- plot_timeseries_one_provider(
    data, 
    provider = provider1, 
    indicator = indicator, 
    scenario_order = scenario_order1, 
    geographies = geographies,
    title = FALSE
    )

  fig2 <- plot_timeseries_one_provider(
    data, 
    provider = provider2, 
    indicator = indicator, 
    scenario_order = scenario_order2, 
    geographies = geographies,
    title = FALSE
    )

  fig <- subplot(
    fig1, 
    fig2, 
    shareX = TRUE,
    shareY = TRUE,
    titleY = TRUE, 
    titleX = TRUE, 
    margin = 0.05
    ) %>% layout(
      annotations = list(
       list(
         x = 0.2 , 
         y = 1.02, 
         text = provider1, 
         showarrow = FALSE,
         xref='paper', 
         yref='paper'
         ),
       list(
         x = 0.8 , 
         y = 1.02, 
         text = provider2, 
         showarrow = FALSE,
         xref='paper', 
         yref='paper'
         )
      ),
      legend_tracegroupgap = 180
    )
    
  fig
}