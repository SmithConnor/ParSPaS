#' Plotting Functions
#' @param hClustMet


plot_parspas = function(hClustMet){
  labs = ggdendro::label(x = hClustMet)
  mPlot = ggplot2::ggplot(ggdendro::segment(x = hClustMet)) +
    ggplot2::geom_segment(mapping = aes(x = x,
                                        y = y,
                                        xend = xend,
                                        yend = yend)) +
    ggplot2::geom_text(data = labs,
                       mapping = aes(label = label,
                                     x = x,
                                     y = 0)) +
    ggplot2::labs(title = "")
  base::return(mPlot)
}
