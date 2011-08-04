library(grid)
library(lattice)
library(playwith) ## (for panel.usertext, etc)
## + might need others, often library(latticeExtra).
## Assuming that the data are attached and any
## customised style settings are in place; save with
## myStyle <- trellis.par.get(); then restore with
## trellis.par.set(myStyle)
print(xyplot(mpg ~ disp, data = mtcars, ylim = c(0, 
35.55)))
## set up viewports
downViewport(trellis.vpname("toplevel"))
pushViewport(viewport(name = "pageAnnotationVp", 
    yscale = c(1, 0)))
upViewport(0)
## draw brushed (highlighted) points
seekViewport("plot_01.panel.1.1.vp")
panel.brushpoints(400, 19.2)
## add labels to data points
seekViewport("plot_01.panel.1.1.vp")
panel.usertext(146.7, 24.4, "Merc 240D", pos = 1)
panel.usertext(258, 21.4, "Hornet 4 Drive", 
    pos = 3)
## draw custom annotations
seekViewport("plot_01.panel.1.1.vp")
panel.usertext(303.423, 29.7165, "asasa")
panel.arrows(398.6351, 23.01873, 399.7892, 
    19.60287, length = 0.15, unit = "inches")
upViewport(0)