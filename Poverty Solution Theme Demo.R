library(remotes)
library(tidyverse)
library(extrafont)

## if your font installation below doesn't work, then revert your version of Rttf2pt1 to a previous version.
#remotes::install_version("Rttf2pt1", version = "1.3.8")

## ensure that you have "DIN" and "DIN Black" installed on your computer, available here:https://cofonts.com/download/din-font-2/
## import fonts from computer
extrafont::font_import()

# define poverty solutions theme
theme_povsol <-
  theme_classic() + theme(
    text = element_text(family = "DIN"),
    plot.title = element_text("DIN Black"),
    panel.grid.major = element_line(colour = "grey90")
  )

# Internal list of poverty solutions color standards

povsol_colors <- c(
  michigan_blue      = "#00274c",
  maize              = "#ffcb05",
  secondary_blue     = "#2d68b7",
  blue1              = "#0054A3",
  blue2              = "#0080FA",
  blue3              = "#51AAFF",
  blue4              = "#A8D5FF",
  brown1             = "#a79d96",
  brown2             = "#655a52",
  red1               = "#7a121c",
  orange1            = "#cc6600"

)

get_colors <- function(..., palette = povsol_colors) {
  colors <- c(...)
  
  if (is.null(colors))
    return (palette)
  palette[colors]
}

# Palettes using Poverty Solutions colors

povsol_pal_list <- list(
  main = get_colors("michigan_blue",
                    "maize",
                    "secondary_blue",
                    "red1",
                    "orange1"
                    ),
  blues = get_colors("michigan_blue", "blue1", "blue2","blue3","blue4"),
  browns = get_colors("brown1", "brown2", "sidewalk")
)



scale_color_povsol <- function(...,
                            palette = "main",
                            discrete = T,
                            reverse = F,
                            pal_list = povsol_pal_list,
                            named = F) {
  
  pal <- povsol_pal_list[[palette]]
  
  if(!named)
    names(pal) <- NULL
  
  if (reverse) pal <- rev(pal)
  if (discrete) {
    scale_color_manual(..., values = pal)
  } else {
    scale_color_gradientn(..., colors = pal)
  }
}

scale_fill_povsol <- function(...,
                           palette = "main",
                           discrete = T,
                           reverse = F,
                           pal_list = povsol_pal_list,
                           named = F) {
  
  pal <- povsol_pal_list[[palette]]
  
  if(!named)
    names(pal) <- NULL
  
  if (reverse) pal <- rev(pal)
  if (discrete) {
    scale_fill_manual(..., values = pal)
  } else {
    scale_fill_gradientn(..., colors = pal)
  }
}


### chart of categorical data
ggplot(iris, aes(Sepal.Width, fill = Species)) +
  geom_histogram(position = "dodge") +
  labs(title = "Poverty Solutions Histogram Test",
       subtitle = "Subtitle Test",
       caption = "Poverty Solutions at the University of Michigan") +
  scale_fill_povsol() +
  theme_povsol


## scatterplot with more of the colors to see
midwest %>%
  ggplot(aes(y= percbelowpoverty, x = perchsd, color = state)) +
  geom_point() + 
  theme_povsol + 
  scale_color_povsol(palette = "main") +
  labs(title = "Poverty Solutions Scatterplot Test",
       subtitle = "Poverty and High School Completion",
       caption = "Poverty Solutions at the University of Michigan")


## bar chart with categorical data
## note how the palette setting changes the palette to "blues"
midwest %>% 
  group_by(state) %>%
  summarise(childpov = mean(percchildbelowpovert)) %>%
  ggplot(aes(x = state, y = childpov, fill = state)) +
  geom_bar(stat = "identity") + 
  theme_povsol +
  scale_fill_povsol(palette = "blues") +
  labs(title = "Poverty Solutions Scatterplot Test",
       subtitle = "Poverty Rate by Midwestern State",
       caption = "Poverty Solutions at the University of Michigan")

## chart with time series data

economics_long %>%
  filter(variable %in% c("psavert","uempmed")) %>%
ggplot(aes(x = date, y = value, color = variable)) +
  geom_line() +
  scale_color_povsol() +
  labs(title = "Poverty Solutions Time Series Test",
       subtitle = "Unemployment Rate and Personal Savings Rate",
       caption = "Poverty Solutions at the University of Michigan") +
  theme_povsolutions










