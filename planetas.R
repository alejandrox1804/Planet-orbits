# source: http://www.ijsrp.org/research-paper-0516/ijsrp-p5328.pdf

library(tidyverse)
library(gapminder)
library(gganimate)
library(gifski)
library(av)

f_planent_orbits <- function(inner_planet,
                             outer_planet,
                             file_name,
                             format = 'gif',
                             years = 1,
                             duration = 10,
                             fps = 50,
                             scale = 1,
                             background_color = 'black',
                             line_color = 'white'){

    planet_orbits_par <-  data.frame(planet = c('Mercury','Venus','Earth','Mars','Jupiter','Saturn','Uranus','Neptune'),
                                     a = c(57.9,108,150,228,779,1430,2870,4500),
                                     b = c(56.6703,107.9974,149.9783,226.9905,778.0643,488.1149,2866.9619,4499.7277),
                                     year = c(88,225,365,687,4315,10759,30667,60182),
                                     units = c(rep(7,5),rep(30,3)))

    a_inner <- planet_orbits_par %>% filter(planet == inner_planet) %>% .$a
    b_inner <- planet_orbits_par %>% filter(planet == inner_planet) %>% .$b
    a_outer <- planet_orbits_par %>% filter(planet == outer_planet) %>% .$a
    b_outer <- planet_orbits_par %>% filter(planet == outer_planet) %>% .$b

    df_length <- round(years*(planet_orbits_par %>% filter(planet == outer_planet) %>% .$year)/(planet_orbits_par %>% filter(planet == outer_planet) %>% .$units),digits = 0)
    inner_planet_years <- (planet_orbits_par %>% filter(planet == outer_planet) %>% .$year)/(planet_orbits_par %>% filter(planet == inner_planet) %>% .$year)

    planet_distances_df <- data.frame(xinner = a_inner*cos(seq(0,years*inner_planet_years*2*pi,length.out = df_length)),
                                      yinner = b_inner*sin(seq(0,years*inner_planet_years*2*pi,length.out = df_length)),
                                      xouter = a_outer*cos(seq(0,years*2*pi,length.out = df_length)),
                                      youter = b_outer*sin(seq(0,years*2*pi,length.out = df_length)),
                                      time = 1:(df_length))

    gif <- ggplot() +
        geom_segment(data = planet_distances_df,
                     mapping = aes(x = xinner,
                                   xend = xouter,
                                   y = yinner,
                                   yend = youter,
                                   group = seq_along(time)),
                     color = line_color, size = 0.01,linetype = 'dotted') +
        theme_void() +
        theme(panel.background = element_rect(fill = background_color)) +
        transition_reveal(time) +
        NULL

    if (format == 'gif') {

        animate(gif,duration = duration, fps = fps, width = scale*500, height = (b_outer/a_outer)*scale*500, renderer = gifski_renderer())
        anim_save(paste0(file_name,'.gif'))

    }

    if (format == 'mp4') {

        animate(gif,duration = duration, fps = fps, width = scale*500, height = (b_outer/a_outer)*scale*500, renderer = av_renderer())
        anim_save(paste0(file_name,'.mp4'))

    }

}

# Test

f_planent_orbits(inner_planet = 'Mercury',
                 outer_planet = 'Venus',
                 file_name = 'Mercury v. Venus',
                 format = 'gif',
                 years = 10,
                 duration = 10,
                 fps = 50,
                 scale = 1.4,
                 background_color = '#00212b',
                 line_color = '#bde300'  )
