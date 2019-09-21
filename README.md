# ggNestedBarChart
An easier way to create nested, grouped, heirarchical bar charts with ggplot2

### Screenshot
![ggNestedBarChart Example](p2.png?raw=true "ggNestedBarChart Example")

### Installation

``` r
devtools::install_github("davedgd/ggNestedBarChart")
```

### Usage

``` r
library(ggNestedBarChart)
library(stringr)

# set up data
data(mtcars)
mtcars <- mtcars %>%
  mutate(car = rownames(mtcars),
         vs = recode(vs, "0" = "V-Shaped", "1" = "Straight"),
         am = recode(am, "0" = "Automatic", "1" = "Manual"),
         gear = paste(gear, "Gears"),
         carb = paste(carb, "Carbs"),
         brand = str_split(rownames(mtcars), " ") %>% map_chr(., 1))

# create base ggplot2 bar chart with theme tweaks
p1 <- ggplot(mtcars, aes(x = brand, fill = carb)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(0.5), color = "white", fontface = "bold") +
  facet_wrap(vars(am, vs, gear), strip.position = "top", scales = "free_x", nrow = 1) +
  theme_bw(base_size = 15) +
  theme(panel.spacing = unit(0, "lines"),
        strip.background = element_rect(color = "black", size = 0, fill = "grey92"),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(colour = "grey"),
        panel.border = element_rect(color = "black", fill = NA, size = 0),
        panel.background = element_rect(fill = "white"))

# show/save base ggplot2 bar chart
p1
ggsave("p1.png", width = 20, height = 5)
```
![Example](p1.png?raw=true "Base ggplot2 Example")

``` r
# show/save ggNestedBarChart
(p2 <- ggNestedBarChart(p1))
ggsave("p2.png", width = 20, height = 5)
```
![ggNestedBarChart Example](p2.png?raw=true "ggNestedBarChart Example")