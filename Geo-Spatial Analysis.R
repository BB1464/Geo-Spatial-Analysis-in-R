## ----setup, include=FALSE--------------------------------------------------------------
options(htmltools.dir.version = FALSE)
knitr::opts_chunk$set(warning = FALSE, 
                      message = FALSE, 
                      fig.align = "center", 
                      dpi = 400,
                      fig.height = 4.5)


## --------------------------------------------------------------------------------------
plot(x = 1, y = 1,
     ylab = "", xlab = "", 
     main = "Point")


## --------------------------------------------------------------------------------------
plot(x = c(1, 2, 3), y = c(1, 4, 2), 
     ylab = "", xlab = "",
     main = "Line", type = "l")


## --------------------------------------------------------------------------------------
plot(x = c(1, 2, 4, 3, 1), y = c(2, 4, 3, 1, 2),
     ylab = "", xlab = "",
     main = "Polygon", type = "l")


## ----echo = FALSE----------------------------------------------------------------------
library(rnaturalearth)
library(sf)
library(dplyr)

nigeria_df <- ne_countries(type = 'countries', scale = 'small', returnclass = "sf") %>%
  filter(name == "Nigeria")

plot(nigeria_df$geometry, col = rgb(59, 132,86, maxColorValue = 255), border = 'grey', 
     axes = TRUE, main = "Nigeria")


## ----world_map, echo= FALSE, fig.height=9, fig.width=9, fig.align = "top"--------------
library(rnaturalearth)
library(sf)

world <- ne_countries(type = 'countries', scale = 'small', returnclass = "sf")

plot(world$geometry)


## --------------------------------------------------------------------------------------
library(rnaturalearth)

world <- ne_countries(type = 'countries', 
                      scale = 'small', 
                      returnclass = "sf")


## --------------------------------------------------------------------------------------
class(world)


## --------------------------------------------------------------------------------------
str(world)


## --------------------------------------------------------------------------------------
head(world)


## --------------------------------------------------------------------------------------
library(sf)

plot(world)


## --------------------------------------------------------------------------------------
plot(world$geometry)


## --------------------------------------------------------------------------------------
plot(world$geometry)


## --------------------------------------------------------------------------------------
library(dplyr)

world1 <- world %>% filter(continent != "Antarctica") 

plot(world1$geometry)


## --------------------------------------------------------------------------------------
africa <- world %>% filter(continent == "Africa")
plot(africa$geometry)


## --------------------------------------------------------------------------------------
nigeria <- ne_states(country = "Nigeria", returnclass = "sf")

head(nigeria)


## --------------------------------------------------------------------------------------
plot(nigeria$geometry, 
     col = sf.colors(37, categorical = TRUE), 
     border = 'grey', axes = TRUE, main = "Nigeria")


## ----cache=FALSE-----------------------------------------------------------------------
library(coronavirus)

covid19_daily <- refresh_coronavirus_jhu()

head(covid19_daily)


## --------------------------------------------------------------------------------------
library(tidyr)

df <- covid19_daily %>%
  filter(location_type == "country") %>%
  group_by(location, location_code, data_type) %>%
  summarise(cases = sum(value),
            .groups = "drop") %>%
  pivot_wider(names_from = data_type, values_from = cases) %>%
  setNames(c("country", "country_code", "total_cases", "total_death"))

head(df)


## --------------------------------------------------------------------------------------
world_covid1 <- world %>% 
  filter(continent != "Antarctica") %>%
  select(country = sovereignt, geometry) %>%
  left_join(df, by = "country")

plot(world_covid1[, c("total_cases")])


## --------------------------------------------------------------------------------------
world_covid2 <- world %>% 
  filter(continent != "Antarctica") %>%
  select(country_code = iso_a2, geometry) %>%
  left_join(df, by = "country_code")

plot(world_covid2[, c("total_cases")])


## ---- cache=FALSE----------------------------------------------------------------------
library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population"

page <- read_html(url)
tables <- html_node(page, ".wikitable")
pop_table <- html_table(tables, fill = TRUE) %>%
  select(state_temp = State, pop_2006_temp = `Population (2006)`, pop_2016_temp = `Population (2016)`)


pop_table$pop_2006 <- as.numeric(gsub(x = pop_table$pop_2006_temp,pattern = ",", replacement = ""))
pop_table$pop_2016 <- as.numeric(gsub(x = pop_table$pop_2016_temp,pattern = ",", replacement = ""))
pop_table$state <- gsub(x = pop_table$state_temp,pattern = " State", replacement = "")
pop_table <- pop_table %>% select(-state_temp, -pop_2006_temp, - pop_2016_temp) %>%
  select(state, pop_2006, pop_2016) %>%
  mutate(state_fix = state)


## --------------------------------------------------------------------------------------
head(pop_table)


## --------------------------------------------------------------------------------------
pop_table$state_fix[which(pop_table$state_fix == "Nasarawa")] <- "Nassarawa"


## --------------------------------------------------------------------------------------
nigeria_pop <- nigeria %>% left_join(pop_table, by = c("name" = "state_fix"))


## ----fig.height=4----------------------------------------------------------------------
plot(nigeria_pop["pop_2016"], key.pos = 1, 
     axes = TRUE, main = "Nigeria Population by State",
     key.width = lcm(1.3), key.length = 1.0)


## --------------------------------------------------------------------------------------
library(mapview)

mapview(nigeria_pop, 
        zcol = "pop_2016",
        legend = TRUE,
        layer.name = "Population")



## --------------------------------------------------------------------------------------
library(coronavirus)
library(tmap)

sf_use_s2(FALSE)

data("covid19_vaccine")

head(covid19_vaccine)


## --------------------------------------------------------------------------------------
map <- ne_countries(returnclass = "sf") %>%
  select(name, iso2 = iso_a2, iso3 = iso_a3, geometry)


df <- map %>% left_join(
   covid19_vaccine %>%
    filter(date == max(date),
           is.na(province_state)) %>%
    mutate(perc = round(100 * people_fully_vaccinated / population, 2)) %>%
    select(country_region, iso2, iso3, people_fully_vaccinated, perc, continent_name),
    by = c("iso2", "iso3")
)


## --------------------------------------------------------------------------------------
df1 <- df %>% 
  filter(!name %in% c("Greenland", "Antarctica"))

p1 <- tm_shape(df1) + 
  tm_polygons(col = "perc",  
              n = 8,
              title = "Fully Vaccinated %",
              palette = "Blues")


## --------------------------------------------------------------------------------------
p1


## --------------------------------------------------------------------------------------

p2 <- tm_shape(df1) + 
  tm_polygons(col = "perc",  
              n = 8,
              projection = 3857,
              title = "Fully Vaccinated %",
              palette = "Blues")


## --------------------------------------------------------------------------------------
p2


## --------------------------------------------------------------------------------------
df2 <- df1 %>%
  filter(continent_name == "South America")


p3 <- tm_shape(df2) + 
  tm_polygons(col = "perc",  
              n = 5,
              title = "Perc. Group",
              palette = "Blues") 


## --------------------------------------------------------------------------------------
p3


## --------------------------------------------------------------------------------------
p4 <- tm_shape(df2) + 
  tm_polygons(col = "perc",  
              n = 5,
              title = "Perc. Group",
              palette = "Blues") + 
  tm_style("cobalt") + 
  tm_text("iso3", size = 0.7) +
  tm_layout(
            title= "% of Population Fully Vaccinated", 
            title.position = c('right', 'top') ,
            inner.margins = c(0.02, .02, .1, .25))


## --------------------------------------------------------------------------------------
p4


## --------------------------------------------------------------------------------------
tmap_mode("view")

p5 <- tm_shape(df2) + 
  tm_polygons(col = "perc",  
              n = 5,
              title = "Perc. Group",
              palette = "Blues") 


## --------------------------------------------------------------------------------------
p5


## --------------------------------------------------------------------------------------
tmap_mode("plot")
nigeria_pop_plot <- tm_shape(nigeria_pop) +
    tm_polygons(col = "pop_2016", 
                style = "order",
                title = "Population",
                palette = "Blues") +
   tm_style("cobalt") + 
   tm_text("state", size = 0.7) +
  tm_credits("Source: Wikipedia - List of Nigerian states by population",
             position = c("LEFT", "BOTTOM")) + 
  tm_layout(title= "Nigeria Population by States", 
            title.position = c('right', 'top') ,
            inner.margins = c(0.02, .02, .1, .15))


## --------------------------------------------------------------------------------------
nigeria_pop_plot


## --------------------------------------------------------------------------------------
tm_shape(nigeria_pop) + 
  tm_polygons(col = "pop_2016",  
              n = 5,
              title = "Total Population",
              palette = "Greens") + 
  tmap_options(limits = c(facets.view = 13)) + 
  tm_facets(by = "name")


## --------------------------------------------------------------------------------------
library(ggplot2)
library(viridis)
ggplot(data = nigeria_pop, aes(fill = `pop_2016`)) + 
  geom_sf() + 
  scale_fill_viridis_b()


## --------------------------------------------------------------------------------------
ggplot(data = nigeria_pop, aes(fill = `pop_2016`)) + 
  geom_sf(size = 0.1) + 
    scale_fill_viridis(alpha = 0.9,
                       begin = 0.01,
                       discrete = FALSE,
                       end = 0.9) + 
  geom_sf_label(aes(label = state)) + 
  labs(fill = "Population",
       title = "Population by State",
       caption = "Source: https://en.wikipedia.org/wiki/List_of_Nigerian_states_by_population") + 
   theme_void()

