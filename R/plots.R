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
    dplyr::filter(yend == 0) %>%
    dplyr::left_join(hClustMet$labels, by = 'x') %>%
    dplyr::rename(variable_name = label) %>%
    dplyr::left_join(base, by = 'variable_name')


  labs = ggdendro::label(x = hClustMet) %>%
    dplyr::rename(variable_name = label) %>%
    dplyr::left_join(base, by = 'variable_name')
  stable = ifelse(base$Stable == TRUE, "#00BFC4", "#F8766D")
  mPlot = ggplot2::ggplot() +
    ggplot2::geom_segment(data = segments,
                          mapping = ggplot2::aes(x = x,
                                        y = y,
                                        xend = xend,
                                        yend = yend)) +
    ggplot2::geom_segment(data = ends,
                          mapping = ggplot2::aes(x = x,
                                        y = y.x,
                                        xend = xend,
                                        yend = yend,
                                        colour = Stable)) +
    ggplot2::scale_x_continuous(breaks = seq_along(hClustMet$labels$label),
                       labels = stringr::str_trunc(hClustMet$labels$label, 6)) +
    ggplot2::labs(title = title) +
    ggdendro::theme_dendro() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 90, colour = stable[match(hClustMet$labels$label, base$variable_name)]),
          axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 5) ))
  base::return(list(mPlot,
                    varNames = varNames))
}

model_plot = function(plot, model){
  base = data.frame(variable_name = plot$varNames, Model = model)
  segments = hClustMet$segments
  ends = segments %>%
    dplyr::filter(yend == 0) %>%
    dplyr::left_join(hClustMet$labels, by = 'x') %>%
    dplyr::rename(variable_name = label) %>%
    dplyr::left_join(base, by = 'variable_name')


  labs = ggdendro::label(x = hClustMet)
  mPlot = ggplot2::ggplot() +
    ggplot2::geom_segment(data = segments,
                          mapping = ggplot2::aes(x = x,
                                        y = y,
                                        xend = xend,
                                        yend = yend)) +
    ggplot2::geom_segment(data = ends,
                          mapping = ggplot2::aes(x = x,
                                        y = y.x,
                                        xend = xend,
                                        yend = yend,
                                        colour = Stable)) +
    ggplot2::geom_text(data = labs,
                       mapping = ggplot2::aes(label = label,
                                     x = x,
                                     y = -0.5),
                       angle = 270) +
    ggplot2::labs(title = title) +
    theme(axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::F_blank(),
          axis.ticks.x = ggplot2::element_blank())
  base::return(mPlot)
}


#' Combine all plots
#'@param list
#'
#'@retun
#'
#'@export

combine_plot = function(list){
  bigPlot = ggpubr::ggarrange(ggpubr::ggarrange(list$avgPlot, list$nonPlot, nrow = 2),
                    list$allPlot,
                    ggpubr::ggarrange(list$medPlot, list$varPlot, nrow = 2),
                    ncol = 3, widths = c(1,1.5,1))

  return(bigPlot)
}
