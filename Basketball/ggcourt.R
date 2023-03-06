library(tidyverse)

ggcourt<-function(color1, color2)
{
# court specifications 
## using https://www.thehoopsgeek.com/basketball-court-dimensions/
## and https://proformancehoops.com/basketball-court-dimensions/
## and https://www.dimensions.com/element/basketball-rims-nets
## and ballR: https://github.com/toddwschneider/ballr/blob/master/plot_court.R

width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
back_of_hoop_length = 0.5 # back part is 6 inches
hoop_radius = 0.75 # hoop is 9 inches 
hoop_center_y = backboard_offset + back_of_hoop_length + hoop_radius
three_point_radius = 23.75 
three_point_side_radius = 22
three_point_side_height = 14

hash_mark_width = .8
hash_height_1 = 7
hash_height_2 = 8
hash_height_3 = 11
hash_height_4 = 14

# Creating circle in ggplot2 (https://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2)
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(tibble(x = center[1] + radius * cos(angles),
                y = center[2] + radius * sin(angles)))
}

# the outside length and with of the court
# since using one side of court, divide the width by 2

court_points = tibble(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
  y = c(height, 0, 0, height, height),
  desc = "perimeter",
  color = "transparent"
)

# the outer key = smaller rectangle inside the court
# creating geom_rect to fill
outer_key= tibble(
  xmin= -outer_key_width/2,
  xmax= outer_key_width/2,
  ymin=0,
  ymax=key_height)

# lane lines - lines inside outer_key 
court_points = bind_rows(court_points, tibble(
  x = c(inner_key_width/2, inner_key_width/2, -inner_key_width/2, -inner_key_width/2),
  y = c(0, key_height, key_height, 0),
  desc = "lane_lines",
  color = color2
))

# backboard offset = 4 inches 
# backboard with spans both sides of court 
court_points = bind_rows(court_points , tibble(
  x = c(-backboard_width / 2, backboard_width / 2),
  y = c(backboard_offset, backboard_offset),
  desc = "backboard",
  color = color2
))

# back of hoop or neck 
court_points = bind_rows(court_points , tibble(
  x = c(0, 0), 
  y = c(backboard_offset, backboard_offset + back_of_hoop_length), 
  desc = "back_of_hoop",
  color = color2
))

# circle's center is the top part of outer_key (at 0,19)
# the diameter is = inner_key_width (radius is half)
foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)

# filter the top part
foul_circle_top = filter(foul_circle, y > key_height) %>%
  mutate(desc = "foul_circle_top",
         color = color1)

# filter bottom part - dashed 
foul_circle_bottom = filter(foul_circle, y < key_height) %>%
  mutate(
    angle = atan((y - key_height) / x) * 180 / pi,
    angle_group = floor((angle - 5.625) / 11.25),
    desc = paste0("foul_circle_bottom_", angle_group),
    color = color2
  ) %>%
  filter(angle_group %% 2 == 0) %>%
  select(x, y, desc, color)

# hoop circle 
hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
  mutate(desc = "hoop",
         color= color2)

# restricted semi-circle 
restricted = circle_points(center = c(0, hoop_center_y), radius = 4) %>%
  filter(y >= hoop_center_y) %>%
  mutate(desc = "restricted",
         color = color2)

three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
  filter(y >= three_point_side_height, y >= hoop_center_y)

three_point_line = tibble(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line",
  color=color1)

hash<-tibble(
  x = c(rep(outer_key_width/2, 4),
        rep(- outer_key_width/2, 4)
        ),
  xend = c(rep(outer_key_width/2 + hash_mark_width, 4),
           rep(-outer_key_width/2 - hash_mark_width, 4)),
  y = c(hash_height_1, hash_height_2, hash_height_3, hash_height_4,
        hash_height_1, hash_height_2, hash_height_3, hash_height_4),
  yend = c(hash_height_1, hash_height_2, hash_height_3, hash_height_4,
           hash_height_1, hash_height_2, hash_height_3, hash_height_4),
  group = c("hash_right_1", "hash_right_2", "hash_right_3", "hash_right_4",
            "hash_left_1", "hash_left_2", "hash_left_3", "hash_left_4"),
  color = rep(color1, 8)
)
court_points = bind_rows(
  court_points,
  foul_circle_top,
  foul_circle_bottom,
  hoop,
  restricted,
  three_point_line
)

court_points <<- court_points

plot<-ggplot() +
  geom_rect(
    data = outer_key,
    aes(xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax),
    fill = color1,
    color =color2,
    linewidth = .8
  )+
  geom_path(
    data = court_points,
    aes(x = x, y = y, group = desc, color = color),
    linewidth=.8
  ) +
  geom_segment(
    data = hash,
    aes(x = x, y=y, xend=xend, yend=yend, group=group, color=color),
    linewidth = .8
  )+
  scale_color_identity()+
  coord_fixed(ylim = c(0, height), xlim = c(-width/2,width/2), clip="off") +
  theme_void()+
  theme(legend.position = "none")

return(plot)
}
