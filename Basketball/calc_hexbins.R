library(tidyverse)

# adopted from https://github.com/toddwschneider/ballr/blob/master/hex_chart.R
# tweaked to use geom_star() instead of geom_polygon

# from the geom_hexbin function
hex_bounds <- function(x, binwidth) {
  c(
    # the number rounds down using binwidth size as accuracy (min)
    plyr::round_any(min(x), binwidth, floor) - 1e-6,
    
    # the number rounds up using binwidth size as accuracy (max)
    plyr::round_any(max(x), binwidth, ceiling) + 1e-6
  )
}


calculate_hex_coords = function(dataset, binwidths, season) {
  
  # filter dataset by season
  dataset <- dataset %>%
  filter(Season %in% season)
  
  # the x location (start and end) of the hexagon
  xbnds = hex_bounds(dataset$LOC_X, binwidths[1])
  
  # the number of x bins 
  xbins = diff(xbnds) / binwidths[1]
  
  # the y location (start and end) location of hexagon 
  ybnds = hex_bounds(dataset$LOC_Y, binwidths[2])
  
  # the number of y bins 
  ybins = diff(ybnds) / binwidths[2]
  
  # assigns the x and y coordinate to hexbins based on the hexagonal breakup of grid
  
  hb = hexbin::hexbin(
    x = dataset$LOC_X,
    y = dataset$LOC_Y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  # add the hexagon_id to the dataset 
  dataset = dataset %>%
            mutate(hexbin_id = hb@cID)
  
  # summarize by hexbin 
  hexbin_stats = dataset %>%
    group_by(hexbin_id,TEAM_NAME) %>%
    summarize(
      # number of shots
      hex_attempts = n(),
      # number of shots made 
      hex_pct = mean(SHOT_MADE_FLAG),
      # total points per shot
      hex_points_scored = sum(SHOT_MADE_FLAG * shot_value),
      # mean points per shot 
      hex_points_per_shot = mean(SHOT_MADE_FLAG * shot_value),
      # drop group
      .groups = "drop"
    )
  
  hexbin_ids_to_zones = dataset %>%
    # group by hexbin_id, shot_zone_range and shot_zone_area
    group_by(hexbin_id, SHOT_ZONE_RANGE, SHOT_ZONE_AREA) %>%
    
    # calculate attempts per each id, zone range and shot zone area 
    summarize(attempts = n(), .groups = "drop") %>%
    
    # arrange by decreasing attempts 
    arrange(hexbin_id, desc(attempts)) %>%
    
    # group by hexbin_id 
    group_by(hexbin_id) %>%
    
    # filter the shot zone range/shot zone area for each hexbin with higher # of attempts 
    filter(row_number() == 1) %>%
    select(hexbin_id, SHOT_ZONE_RANGE, SHOT_ZONE_AREA)
  
  # do an inner join to now have a dataset with unique hexbin_id and one zone range/area assigned to it 
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # not used 
  # # from hexbin package, see: https://github.com/edzer/hexbin
  # # x bin ratio
  # sx = hb@xbins / diff(hb@xbnds)
  # # y bin ratio 
  # sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  # dx = 1 / (2 * sx)
  # dy = 1 / (2 * sqrt(3) * sy)
  # origin_coords = hexbin::hexcoords(dx, dy)
  
  # calculate the centers of each cell 
  hex_centers = hexbin::hcell2xy(hb)
  
  # get a table which has one center_x, center_y for each hexbin_id
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    tibble(
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i])}))
  
  # inner join on hexbin stats 
   inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

calculate_hexbins_from_shots = function(dataset = lebron_regular, league_averages = league_regular, season, binwidths = c(1, 1), fg_diff_limits = c(-0.12, 0.12), fg_pct_limits = c(0.2, 0.7), pps_limits = c(0.5, 1.5)) {
  if (nrow(dataset) == 0) {
    return(list())
  }
  
  dataset<-dataset %>% filter(Season %in% season)
  
  league_averages<-league_averages %>% filter(Season %in% season)
  
  # summary by zone - individual
  grouped_shots = dataset %>% group_by(SHOT_ZONE_RANGE, SHOT_ZONE_AREA)
  
  zone_stats = grouped_shots %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(SHOT_MADE_FLAG),
      zone_points_scored = sum(SHOT_MADE_FLAG * shot_value),
      zone_points_per_shot = mean(SHOT_MADE_FLAG * shot_value),
      .groups = "drop"
    )
  
  # summary by zone - league 
  league_zone_stats = league_averages %>%
    group_by(SHOT_ZONE_RANGE, SHOT_ZONE_AREA) %>%
    summarize(league_pct = sum(FGM) / sum(FGA), .groups = "drop")
  
  hex_data = calculate_hex_coords(dataset = dataset, binwidths = binwidths, season = season)
  
  # individual and league join keys 
  join_keys = c("SHOT_ZONE_AREA", "SHOT_ZONE_RANGE")
  
  # join hex_data (hex_bin, coords) on zone stats and league stats
  hex_data = hex_data %>%
    inner_join(zone_stats, by = join_keys) %>%
    inner_join(league_zone_stats, by = join_keys)
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = hex_data %>%
                    mutate(
                    season = paste0(str_sub(season[1],1,4),"-",str_sub(season[length(season)],6,7)),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]),
                    team_logo = case_when(
                      str_detect(TEAM_NAME,"Cleveland") ~ "https://cdn.nba.com/teams/uploads/sites/1610612739/2022/03/Asset-4-3.svg",
                      str_detect(TEAM_NAME, "Lakers") ~ "https://cdn.nba.com/teams/uploads/sites/1610612747/2021/12/lakers_70x70.svg",
                      str_detect(TEAM_NAME,"Heat") ~ "https://cdn.freebiesupply.com/images/large/2x/miami-heat-logo-transparent.png",
                      TRUE ~ ""
                    ))
  
  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
}

