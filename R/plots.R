#' Plotting Functions
#' @param hClustMet
#' @param baseModel
#' @param thresh
#' @param title
#' @param varNames


plot_parspas = function(hClustMet, baseModel, thresh = 0.5, title, varNames){
  base = data.frame(variable_name = varNames, Stable = baseModel >= thresh)
  segments = hClustMet$segments
  ends = segments %>%
    filter(yend == 0) %>%
    left_join(hClustMet$labels, by = 'x') %>%
    rename(variable_name = label) %>%
    left_join(base, by = 'variable_name')


  labs = ggdendro::label(x = hClustMet) %>%
    rename(variable_name = label) %>%
    left_join(base, by = 'variable_name')
  stable = ifelse(base$Stable == TRUE, "#00BFC4", "#F8766D")
  mPlot = ggplot2::ggplot() +
    ggplot2::geom_segment(data = segments,
                          mapping = aes(x = x,
                                        y = y,
                                        xend = xend,
                                        yend = yend)) +
    ggplot2::geom_segment(data = ends,
                          mapping = aes(x = x,
                                        y = y.x,
                                        xend = xend,
                                        yend = yend,
                                        colour = Stable)) +
    scale_x_continuous(breaks = seq_along(hClustMet$labels$label),
                       labels = stringr::str_trunc(hClustMet$labels$label, 6)) +
    ggplot2::labs(title = title) +
    ggdendro::theme_dendro() +
    theme(axis.title.x=element_blank(),
          axis.text.x= element_text(angle = 90, colour = stable[match(hClustMet$labels$label, base$variable_name)]),
          axis.ticks.x=element_blank()) +
    guides(color = guide_legend(override.aes = list(size = 5) ))
  base::return(list(mPlot,
                    varNames = varNames))
}

model_plot = function(plot, model){
  base = data.frame(variable_name = plot$varNames, Model = model)
  segments = hClustMet$segments
  ends = segments %>%
    filter(yend == 0) %>%
    left_join(hClustMet$labels, by = 'x') %>%
    rename(variable_name = label) %>%
    left_join(base, by = 'variable_name')

#' @export


  labs = ggdendro::label(x = hClustMet)
  mPlot = ggplot2::ggplot() +
    ggplot2::geom_segment(data = segments,
                          mapping = aes(x = x,
                                        y = y,
                                        xend = xend,
                                        yend = yend)) +
    ggplot2::geom_segment(data = ends,
                          mapping = aes(x = x,
                                        y = y.x,
                                        xend = xend,
                                        yend = yend,
                                        colour = Stable)) +
    ggplot2::geom_text(data = labs,
                       mapping = aes(label = label,
                                     x = x,
                                     y = -0.5),
                       angle = 270) +
    ggplot2::labs(title = title) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  base::return(mPlot)
}

change_plot = function(plot, var, add = TRUE){

}
