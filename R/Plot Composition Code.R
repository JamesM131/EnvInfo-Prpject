library(patchwork)
a1_5 <- plot_list[1:10] %>%
  imap(~{
    p <- .x  +
      theme(plot.title = element_text(size=22))

    # if(.y != 5) {
    #   p <-p + theme(legend.position = "none")
    # } else {
    #   p <- p + theme(legend.position = "bottom")
    # }

    if(.y != 3) {
      p <- p + ylab("")
    }
    if(.y != 5){
      p <- p + xlab("")
    }
    p <-p + theme(legend.position = "none")

    return(p)
  })

wrap_plots(a1_5, ncol = 2)
