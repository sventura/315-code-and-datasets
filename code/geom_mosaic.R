#  Authors:  Peter Elliott and Sam Ventura
library(ggplot2)

# creates StatMosaic object
StatMosaic <- ggproto("StatMosaic", Stat,
                      default_aes = aes(fill=NA),
                      
                      setup_params = function(data, params) {
                        if (!is.null(data$fill) || !is.null(params$fill)) {
                          stop("stat_mosaic() must not be used with a fill aesthetic.", 
                               call. = FALSE)
                        }
                        params
                      },
                      
                      setup_data = function(data, params) {
                        tab <- table(data$x, data$y)
                        # normalizes row sums
                        prop.tab <- sweep(tab, 1, rowSums(tab), FUN ="/")
                        
                        # assignments colors to fields based on residuals
                        expected <- outer(rowSums(tab),colSums(tab))/sum(tab)
                        std_resid <- (tab-expected)/sqrt(expected)
                        resid_cat <- ifelse(std_resid>4, "r > 4",
                                            ifelse(std_resid>2, "2 < r < 4",
                                                   ifelse(std_resid> -2, "-2 < r < 2",
                                                          ifelse(std_resid> -4, "-4 < r < -2", 
                                                                 "r < -4"))))
                        
                        # find placements for vertical bars
                        bar_edges <- c(0, cumsum(rowSums(tab))/sum(tab)) *
                          nrow(tab) + .5
                        # find breakpoints within bars
                        bar_heights <- cbind(rep(0, nrow(tab)),
                                             matrixStats::rowCumsums(prop.tab)) *
                          ncol(tab) + .5
                        
                        # group parameters for plotting rectangles
                        xmin <- rep(head(bar_edges,-1), each=ncol(tab))
                        xmax <- rep(bar_edges[-1], each=ncol(tab))
                        ymin <- as.vector(t(bar_heights[,-ncol(bar_heights)]))
                        ymax <- as.vector(t(bar_heights[,-1]))
                        col <- factor(t(resid_cat),
                                      levels=c("r > 4","2 < r < 4",
                                               "-2 < r < 2","-4 < r < -2",
                                               "r < -4"))
                        data.frame(xmin, xmax, ymin, ymax, col, 
                                   group = 1:length(xmin),
                                   PANEL = rep(1,length(xmin)))
                      },
                      
                      compute_group = function(self, data, scales, params) {
                        cols = colorRampPalette(c("blue","white","red"))(5)
                        transform(data, fill=factor(col,levels=levels(col),
                                                    labels=cols))
                      }
)



mosaic_legend <- function(color1 = "blue", color2 = "red") {
  palette = c(color1, "white", color2)
  scale_fill_manual(name="Standardized\nResiduals",
                    values=colorRampPalette(palette)(5),
                    labels=c("r > 4", "2 < r < 4",
                             "-2 < r < 2", "-4 < r < -2",
                             "r < -4"),
                    drop = FALSE)
  
}



# wrapper using StatMosaic
geom_mosaic <- function(mapping = NULL, data = NULL,
                        stat = "mosaic", position = "identity",
                        color = "black", ..., na.rm = FALSE,
                        show.legend = TRUE, inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      color=color,
      ...
    )
  )
}
