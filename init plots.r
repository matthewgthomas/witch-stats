##
## Options, parameters, etc. common to all plots
##
plots.dir = "plots"

common_theme = theme_bw() +
  #eliminates baground, gridlines, and chart border
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.background = element_blank()
    ,axis.text=element_text(size=12) 
    ,axis.title=element_text(size=12)  # 12 for printing; 22 for poster
  ) +
  theme(legend.position="none")
