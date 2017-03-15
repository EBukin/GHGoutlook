# Thanks Headly
# https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

grid_arrange_shared_legend <-
  function(...,
           plots_list,
           position = c("bottom", "right"),
           n_page) {
    require(ggplot2)
    require(gridExtra)
    require(grid)
    
    nplots <- length(plots_list)
    if(nplots < n_page) {
      l_ply(seq(nplots+1,n_page), 
            function(x) {
              plots_list[[length(plots_list)+1]] <<- ggplot() + geom_blank()
            })
      nplots <- length(plots_list)
    }
    ncol <- ceiling(sqrt(nplots))
    nrow = ceiling(nplots / ncol)
    
    # plots <- list(...)
    plots <- plots_list
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    
    if (names(dev.cur()) %in% c("RStudioGD", "null device"))
    {
      grid.newpage()
      grid.draw(combined)
    }
    
    if (names(dev.cur()) == "pdf")
    {
      grid.draw(combined)
      grid.newpage()
    } else if (!names(dev.cur()) %in% c("RStudioGD", "null device")) {
      grid.draw(combined)
      grid.newpage()
    }
    
    
    
  }