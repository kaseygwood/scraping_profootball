# scraping_profootball
Shiny App that scrapes quarterback data from the internet. 

## The Data
The data used in this project is from the pro football reference website, https://www.pro-football-reference.com/.
Each player has their own url within the website. The data is scraped based on quarterbacks chosen by the user.

## The App
The shiny app contains two tabs with table data displayed. The first tab is to represent each quarterbacks career data. The career tab allows the user to select up to five players to compare. The user must hit the generate button in order for the app to process the web scraping for each player. The second tab represents each players best season according the the pro football reference website. The best season was chosen by the approximate value statistic that is calculated by the creators of the site. This tab allows the user to select up to 2 players to display and also includes a generate button. There are dropdown bars for each statistic that gives a brief description and explanation. A link was added to the information for the approximate value statistic given by pro-football-reference.

## Scraping Functions
The scraping functions file contains all functions, not just scraping, that were used to build the app. These functions include a generate url, generate career table, generate tables, and a function to determine a players best season using the AV stat. These functions only work for generating quarterback tables. 

The get_url function takes a list of names (given by the user in the app) and formats the name to the pro football reference urls. 

The generate dataframe table creates the career table shown in the app. This function takes a list of names and uses the get_url function to read in the correct url and uses the rvest package to scrape the html_text from the url. It does this for each name in the list and formats the data into a new dataframe. 

The get_tables function is a small function that takes a list of names and returns the tables that were present within the website url. 

The best_year function was created in order to determine a players best season for the playoffs table. This function uses the regular season table and simply outputs the year of the players best season (determined by the AV again). 

The best_game_table function recieves a name and an index. This function is used to create both the regular season and playoff tables in the second tab in the app. This function uses the get_tables function and determines the best season for either regular season or playoffs (uses the best_year function) and outputs the statistic table. 
