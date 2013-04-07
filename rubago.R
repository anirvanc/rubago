library(ggplot2)
library(maps)

fgw = data.frame(check.rows=)

go <- function() {

# load us map data
all_states <- map_data("state")

# start a ggplot. it won't plot til we type p
p <- ggplot()  

# add U.S. states outlines to ggplot
p <- p + geom_polygon(data=all_states, aes(x=long, y=lat, group = group),
     colour="grey", fill="white" )

# add total Population
p <- p + geom_point(data=df1, aes(x=longitude, y=latitude, size = totalPop), 
     colour="#b5e521")

# add sub Population as separate layer with smaller points at same long,lat
p <- p + geom_point(data=df1, aes(x=longitude, y=latitude, size = subPop), 
     colour="#00a3e8")

# change name of legend to generic word "Population"
p <- p + guides(size=guide_legend(title="Population"))

# display plot
p 
}