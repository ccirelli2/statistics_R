'
Ref : https://cengel.github.io/R-data-viz/interactive-graphs.html
https://plotly.com/r/getting-started/
https://shiny.rstudio.com/gallery/
'

# Install / Load Packages
install.packages("plotly")
library(plotly)
library(dplyr)

# Load Data
dir.data <- "C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\hw3\\data"
dir.scripts <- "C:\\Users\\chris.cirelli\\Desktop\\repositories\\gsu_fall_2020\\Visualizations\\hw3"

# Load Data
setwd(dir.data)
lr.data <- read.csv("ar_little_rock_2020_04_01.csv")
setwd(dir.scripts)                      

# Remove Nan
lr.nonan <- na.omit(lr.data)

# Groups
lr.groupby.veh.sex <- lr.nonan %>%
  group_by(vehicle_type, subject_sex) %>%
  summarise(cnt = n())

lr.groupby.veh.sex

#########################################################
# Create Interactive Plot Using ggplot2 + plotly
#########################################################

# Create Plotly Plot
'Step1 : Create a ggplot and assign it to a varaible'
p1 <- ggplot(data=lr.groupby.veh.sex, aes(x=cnt, y=vehicle_type)) +
      geom_col() + coord_flip()
p1

# Pass to Plotly
ggplotly(p1)


#########################################################
# Create Interactive Plot Using Plotly Only
#########################################################

plot_ly(data=lr.groupby.veh.sex, x= ~cnt, y= ~vehicle_type, type='bar')
help(plot_ly)



#########################################################
# Tutorial #2 https://plotly.com/r/getting-started/
#########################################################
dev.off()
rm(list=ls())
library(plotly)

# Box Plot
fig1 <- plot_ly(midwest, x=~percollege, color=~state, type='box')
fig1

# Render Plot in Browser By Printing Figure
print(fig1)


#########################################################
# Basic Scatter on Map Plot
#########################################################

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_us_airport_traffic.csv')

# Pass List Object To Create Geo Styles
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5
)

# Call Plot Geo & Assign to fig
fig <- plot_geo(df, lat = ~lat, lon = ~long)

# Add Markers 
fig <- fig %>% add_markers(
  text = ~paste(airport, city, state, paste("Arrivals:", cnt), sep = "<br />"),
  color = ~cnt, symbol = I("square"), size = I(8), hoverinfo = "text"
)
fig <- fig %>% colorbar(title = "Incoming flights<br />February 2011")
fig <- fig %>% layout(
  title = 'Most trafficked US airports<br />(Hover for airport)', geo = g
)

fig



