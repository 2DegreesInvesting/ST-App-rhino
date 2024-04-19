box::use(
  plotly[...],
  igraph[...],
  networkD3[...],
  tibble[...],
  dplyr[...],
  r2dii.colours[...],
    dplyr[...]
)

convert_to_rgba_strings <- function(hex_vec, alpha = 0.5) {
  str_vec <- c()
  for (hex in hex_vec) {
    rgb_vec <- grDevices::col2rgb(hex)
    str <- glue::glue("rgba(", paste(rgb_vec, collapse = ","), ", {as.character(alpha)})")
    str_vec <- c(str_vec, str)
  }
  str_vec
}

get_data_box <- function(.data) {
  
  .data <- .data %>% 
    dplyr::group_by(provider, scenario, scenario_type, sector_group, sector_group_type, 
                    unit, year, indicator) %>%
    dplyr::summarise(
      value = unique(value),
      nace_codes = paste(unique(nace_code), collapse = ",")
    ) %>%
    dplyr::filter(
      year %in% c(NA, 2050)
    ) %>%
    dplyr::mutate(
      sector_group_type = factor(
        sector_group_type, 
        levels = c("Low Carbon Exposure", "Medium Carbon Exposure", "High Carbon Exposure"))
    ) %>%
    dplyr::mutate(
      y_title = paste(indicator, " (", unit, ")", sep = ""),
      hovertext = glue::glue(
        '<b>{if_else(is.na(sector_group), "", sector_group)}</b>
            <b>NACE codes:</b> {nace_codes}
            <b>{y_title}:</b> {format(value, digits = 2)}'
      )
    ) 
  
  .data
  
}

# Function to create the boxplot like graph
create_boxplot_financials <- function(.data, y_axis_title){
  
  sector_group_type_colours <- r2dii.colours::get_colours(c("green", "blue", "red")) 
  sector_group_rgba <- convert_to_rgba_strings(sector_group_type_colours, alpha = 0.4)
  sector_group_types <- levels(.data$sector_group_type)
  
  providers <- unique(.data$provider)
  
  x_axis_title <- providers[1]
  y_axis_title <- y_axis_title
  
  fig <- plotly::plot_ly() 
  
  for (i in 1:length(sector_group_types)) {
    data_trace <- .data %>% 
      dplyr::filter(
        .data$sector_group_type == sector_group_types[i]
      )
  
  fig <- fig %>% 
    plotly::add_trace(
      data = data_trace,
      x = ~ sector_group_type,
      y = ~ value,
      type = "box",
      boxpoints = "all",
      fillcolor = "rgba(255,255,255,0)",
      hoveron = "points",
      hoverinfo = "text",
      jitter = 0.7,
      line = list(color = "rgba(255,255,255,0)"),
      showlegend = FALSE,
      pointpos = 0,
      text = ~ hovertext,
      marker = list(
        color = sector_group_rgba[i],
        line = list(width = 1, color = sector_group_type_colours[i]),
        size = 10
      )
    )
  
  }
  
  fig <- fig %>%
    plotly::layout(
      xaxis = list(title = x_axis_title),
      yaxis = list(title = y_axis_title),
      paper_bgcolor = r2dii.colours::palette_1in1000_background,
      plot_bgcolor = r2dii.colours::palette_1in1000_background
      )
  
  fig

}

plot_box_per_provider <- function(.data, y_axis_title) {
  
  providers <- unique(.data$provider)
  
  data1 <- .data %>%
    dplyr::filter(provider == providers[1])
  
  fig1 <- create_boxplot_financials(data1, y_axis_title = y_axis_title[[1]])
  fig1
  
  data2 <- .data %>%
    dplyr::filter(provider == providers[2])
  
  fig2 <- create_boxplot_financials(data2, y_axis_title[[2]])
  fig2
  
  fig <- plotly::subplot(
    fig1, 
    fig2, 
    titleY = TRUE, 
    titleX = TRUE, 
    margin = 0.05
  )
  fig
}


# Function to create the network chart 
create_network_chart <- function(.data){
browser()
  data <- .data %>% 
    ungroup() %>% 
    dplyr::select(provider, matching_indicator) %>% 
    filter(!is.na(matching_indicator), !is.na(provider)) %>% 
    rename(variable=matching_indicator) 
  
  vertices <- tibble::tibble(
    names = unique(data$provider),
    group = unique(data$provider),
    size = 70
  )
  
  vertices <- vertices %>%
    bind_rows(tibble::tibble(
      names = unique(data$variable),
      group = "indicator",
      size = 16)
    )
  
  g <- graph_from_data_frame(data, directed = FALSE)

  net <- igraph_to_networkD3(g, group = vertices)
  
  colors <- r2dii.colours::get_colours(c('pink', 'violet', 'orange'))
  
  js_colour_string <- glue::glue('d3.scaleOrdinal(["',paste(colors, collapse = '", "'), '", "#585858"]);')
  
  final_net <-   forceNetwork(Links = net$links, Nodes = net$nodes,
                              Source = 'source', Target = 'target', NodeID = 'name',
                              Group = 'group', Nodesize = 'size', fontSize = 16,
                              colourScale = JS(js_colour_string),
                              charge = -2000, opacity = 0.8,
                              opacityNoHover = 1)
  
  return(final_net)
  
}
