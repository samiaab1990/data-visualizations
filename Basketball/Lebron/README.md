# Lebron James, Looking at A Making of NBA History 

After Lebron James surpassed the NBA all-time scoring record, I created these series of visualizations to look at various facets of his career so far leading up the record. The first two are shot charts, inspired largely by Kirk Goldberry's basketball visualization style - the first, which is looking at field goal percent vs league average for each season in Lebron's career. The size of the hexagon bins correspond to the frequency of shots taken from the location and the color corresponds to the percent difference in Lebron's field goal attempts versus the league average from the given shot zone area and range.

<p align = 'center'><img src='plots/lebron_gif.gif' alt='Lebron Shot Chart Through the Years' width='80%' height='80%'></p>

The second shot chart, shown below, shows all attempted career shots categorized by whether they were made or missed, with the record-breaking shot indicated by a yellow star.

<p align = 'center'><img src='plots/lebron_shot_chart.png' alt='Lebron Shot Chart' width='80%' height='80%'></p>

The next series of visualizations summarize the points scored and efficiency of attempted shots. The waffle chart in the scoring summary below shows when and where most of the points were scored and the latter chart shows a time series of points scored in each season broken down by type of shot (free throw vs two and three point field goals).

<p align = 'center'><img src='plots/lebron_points_summary.png' alt='Lebron Points Summary' width='80%' height='80%'></p>

Most points were scored with the Cavaliers between 2003–2010 — which is expected in part because that was the team Lebron played in the longest. The greatest number of points scored during any season occured in the 2005–06 season with Cleveland, with the greatest share of these points coming from free throws and 3 point field goals subsequent to 2 point field goals. The number of points from 3 point field goals surpassed points from free throws after the 2017–18 season with Cleveland.

The final visualization summarizing Lebron’s career is a look into shooting efficiency, which is a combination of statistics accounting for free throw and field goal percentage. Efficiency is best represented by the true shooting percentage —that accounts for field goals and free throw attempts differently. The free throw percentage and field goal percentage is also included in the chart. The best year, efficiency wise, was in 2013–14 with Miami Heat.

<p align = 'center'><img src='plots/lebron_fg_stats_summary.png' alt='Lebron FG Stats Summary' width='80%' height='80%'></p>

The chart on the bottom is a summary of field goal percent versus league average. While the animated shot chart in the first part of this article showed field goal versus league average differences with greater granularity as it accounted for location — this chart here aggregates the data to show what the difference looked like when looking at the whole season, which also demonstrates 2013–14 being the better season from the perspective of shots made over all attempted for Lebron.

<b>Credits:</b> This summary was largely made possible by the code used to create the [ballR shiny application](https://github.com/toddwschneider/ballr) and the [NBA API library](https://github.com/swar/nba_api) in Python.


## Scripts to Retrieve Data

- [NBA API using R for detailed shotchart stats-Lebron James](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_career_get_data.R)
- [NBA API using Python for detailed shotchart stats-Lebron James](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_career_get_data.py)
- [NBA API using Python for overall stats-Lebron James](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_overall_stats.py)

## Resulting CSV files

- [CSVs](https://github.com/samiaab1990/Data-Visualizations/tree/main/Basketball/Lebron/CSV)

## Scripts to Create Basketball Court (ggcourt) and Hexbins for Shotchart Viz

- [ggcourt](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/ggcourt.R)
- [Script to Calculate Hexbins](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/calc_hexbins.R)

## Data Prep/Cleaning

- [Data Cleaning Script](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/data_cleaning.R)

## Scripts for Viz

- [Animated Shotchart](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_shotchart_viz.R)
- [All Shots Made Shotchart](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_all_shots_viz.R)
- [Points Summary Viz](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_points_summary_viz.R)
- [Efficiency Viz](https://github.com/samiaab1990/Data-Visualizations/blob/main/Basketball/Lebron/lebron_efficiency_summary.R)



