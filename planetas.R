# source: http://www.ijsrp.org/research-paper-0516/ijsrp-p5328.pdf

library(tidyverse)
library(gapminder)
library(gganimate)
library(gifski)
library(av)
library(extrafont)
loadfonts(device = 'win')

f_planent_orbits <- function(inner_planet,
                             outer_planet,
                             file_name = NULL,
                             format = 'gif',
                             years = 5,
                             duration = 10,
                             fps = 20,
                             scale = 1,
                             background_color = 'black',
                             line_color = 'white'){

    if (is.null(file_name)) { file_name <-  paste0(inner_planet,' v. ',outer_planet) }

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
        ggtitle(paste0(inner_planet,' v. ',outer_planet,'\n\n')) +
        theme_void() +
        theme(panel.background = element_rect(fill = background_color),
              plot.background = element_rect(fill = background_color),
              plot.title = element_text(hjust = 0.5,face = 'bold',color = line_color, size = 16,family = 'Century Gothic')) +
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

f_planent_orbits(inner_planet = 'Earth',
                 outer_planet = 'Jupiter',
                 # file_name = 'Earth v. Jupiter',
                 # format = 'gif',
                 # years = 10,
                 # duration = 15,
                 # fps = 40,
                 # scale = 1.5,
                 # background_color = '#00212b',
                 # line_color = '#bde300',
                 NULL)

# Todas las combinaciones

planets <- c('Mercury','Venus','Earth','Mars','Jupiter','Saturn','Uranus','Neptune')

colors <- c('#ffd700','#edfd00','#00ff00','#00ffc5','#ff004d','#00ecff','#ff7400',
            '#0078ff','#6f00ff','#2f00ff','#bde300','#00ff2b','#ff00f4','#f0ff00',
            '#ffd700','#edfd00','#00ff00','#00ffc5','#ff004d','#00ecff','#ff7400',
            '#0078ff','#6f00ff','#2f00ff','#bde300','#00ff2b','#ff00f4','#f0ff00')

k <- 0

for (i in 2:8) {
    for (j in 1:(i - 1)) {

        .rs.restartR()
        gc()
        k <- k + 1

        inner_p <- planets[j]
        outer_p <- planets[i]

        f_planent_orbits(inner_planet = inner_p,
                         outer_planet = outer_p,
                         format = 'gif',
                         years = 10,
                         duration = 15,
                         fps = 40,
                         scale = 1.5,
                         background_color = '#00212b',
                         line_color = colors[k],
                         NULL)

    }
}

