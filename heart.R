library(grid)
heart <- function(lcolor){t=seq(0, 2*pi, by=0.1)
x=16*sin(t)^3
y=13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
a=(x-min(x))/(max(x)-min(x))
b=(y-min(y))/(max(y)-min(y))
grid.lines(a,b,gp=gpar(col=lcolor,lty = "solid",lwd = 6))}
heart("hotpink")
grid.newpage()

grid.newpage()
pushViewport(viewport(x=0.1, y=0.1,w=0.2, h=0.2))
grid.newpage()
  for (j in 1:30) {
       vp <- viewport(.5, .5, w=.9, h=.9)
       pushViewport(vp)
       heart("hotpink")}
