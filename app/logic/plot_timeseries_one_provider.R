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
    dplyr[...]
)

plot_timeseries_one_provider <- function(
  data, 
  provider, 
  indicator, 
  scenario_order = c("Early Action", "No Additional Action", "Late Action"),
  geographies = c("RoW", "rest of world"),
  title = TRUE
  ) {
  
  data_plot <- data %>%
      filter(
        .data$provider == .env$provider,
        .data$matching_indicator == .env$indicator,
        .data$geography %in% .env$geographies
        ) %>%
      mutate(
        scenario = factor(scenario, levels = scenario_order)
        ) %>%
      arrange(scenario, year)
    
  x_axis_title = "Year" 
  y_axis_title = paste(indicator, "(", unique(data_plot$matching_unit[!is.na(data_plot$matching_unit)])[1],")")
  scenario_colours <- r2dii.colours::get_colours(c("green", "blue", "red"))
  
  data_plot <- data_plot %>%
    mutate(
      hovertext = glue::glue(
            '<b>Scenario:</b> {scenario}
            <b>Year:</b> {year}
            <b>{y_axis_title}:</b> {format(converted_value, digits = 2)}'
            )
    )
  
  fig <- data_plot %>%
    plot_ly(
      x = ~ year,
      y = ~ converted_value,
      color = ~scenario,
      type = "scatter",
      mode = "lines",
      hoveron = "points",
      hoverinfo = "text",
      text = ~ hovertext,
      colors = "Set1", #scenario_colours
      legendgroup = provider
    ) %>% layout(
        font = list(family = "ITC Cheltenham Std Light Condensed"),
        xaxis = list(title = x_axis_title),
        yaxis = list(title = y_axis_title),
        legend = list(title = list(text = "Scenario"))
      )
  if (title) {
    fig <- fig %>%
      layout(title = provider)
  }
    
  fig
}