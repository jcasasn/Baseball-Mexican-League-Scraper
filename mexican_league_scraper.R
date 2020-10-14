library(tidyverse)
library(rvest)

# Get HTML data from BR
url = "https://www.baseball-reference.com/register/league.cgi?code=MEX&class=AAA"
html = read_html(url)

# Make a table of all the URLs for each season's page, with its respective Year
bb_data = html %>%
  html_node("#lg_history") %>%
  html_nodes("th a") # All years + links were in table's th, all teams in td

urls_per_season = tibble(URL = html_attr(bb_data, "href"),
                         Year = as.integer(html_text(bb_data))) %>%
  mutate(URL = paste("https://www.baseball-reference.com", URL, sep = ""))

# Return a table with data from a specific season, given its URL and Year
get_season = function(URL, Year) {
  #Sys.sleep(2)
  season_data = read_html(URL)
  season_table = season_data %>%
    html_nodes(xpath = '//comment()') %>% # Select all comments
    html_text() %>%                       # Extract comment text
    paste(collapse = '') %>%              # Collapse to a single string
    read_html() %>%                       # Re-parse to HTML
    html_node('table#league_batting') %>% # Select the desired table
    html_table(fill = TRUE) %>%           # Parse table
    rename(Team = 1) %>%                  # Rename team names column
    subset(Team != "League Totals") %>%   # Remove Totals rows
    select(-Aff) %>%                      # Remove 'Aff' column (it's empty)
    mutate(BatAge = na_if(BatAge, 0)) %>% # Replace 0s with NA in BatAge
    add_column(Year = Year, .after = 1)   # Add a 'Year' column
  return(season_table)
}

# Get a data frame with data from all seasons
final_data = pmap_df(urls_per_season, get_season)

# Get 2018 season data
url_2018 = c(
  "https://www.baseball-reference.com/register/league.cgi?id=4c4fdbdd",
  "https://www.baseball-reference.com/register/league.cgi?id=51bf55cf"
)
html_2018 = map2_df(url_2018, 2018, get_season)

data_2018 = html_2018 %>%
  group_by(Team, Year) %>%
  summarise_each(sum) %>%
  mutate(
    BatAge = round(BatAge / 2, 1),
    `R/G` = round(R / G, 2),
    BA = round(H / AB, 3),
    OBP = round((H + BB + HBP) / (AB + BB + HBP + SF), 3),
    SLG = round(TB / AB, 3),
    OPS = round(OBP + SLG, 3)
  )

# Add 2018 data to final data
final_data = add_row(final_data, data_2018, .after = 16)

# Remove problematic rows
final_data = final_data[final_data$OBP > 0.150, ]

# Export the final data frame to a csv
write.csv(
  final_data,
  "/path/to/file/mexican_league_batting_data.csv",
  row.names = TRUE
)
