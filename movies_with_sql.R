# Loading necessary libraries.
library(tidyverse)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(DT)

# Connecting the the SQLite database
connection <- dbConnect(SQLite(), "movies.sqlite") 

# Loading data the R environment as data frame
movies_data_R <- read.csv("movie_data.csv")

# Getting a feel for the data before we write it to the database.
str(movies_data_R)

# All of the columns are in chr format, lets do the necessary data type conversions.

movies_data_R$Release_Date <- dmy(movies_data_R$Release_Date)
str(movies_data_R$Release_Date)

movies_data_R <- movies_data_R %>%
  mutate(Budget = str_replace_all(Budget, "\\$", ""),  # Remove dollar signs
         Budget = str_replace_all(Budget, ",", ""),      # Remove commas
         Budget = as.numeric(Budget))                    # Convert to numeric

movies_data_R <- movies_data_R %>% 
  mutate(Revenue = str_replace_all(Revenue, "\\$", ""),
         Revenue = str_replace_all(Revenue, ",", ""),
         Revenue = as.numeric(Revenue))

str(movies_data_R)

# Now all of the data types are in correct format, we can write our table to the database.
dbWriteTable(connection, "movies", movies_data_R, overwrite = TRUE, row.names = FALSE)


query <- "
  PRAGMA table_info(movies);
"
result <- dbGetQuery(connection, query)
View(result)

# Our Release_Date column is stored as a numeric value, counting from the days starting form 1970-01-01.
# We will create a new table with the necessary columns, also will convert the date column to a readable format. 
# I also want to create a new column in this table named 'Movie_Range' which will classify the movies ranges according to their
# revenues. Like Elite Blockbuster, Blockbuster, Regular and Underdog. 

query <- "
  CREATE TABLE new_movies AS
    SELECT 
      Movie_Title,
      DATE('1970-01-01', '+' || CAST(Release_Date AS INTEGER) || ' days') AS Release_Date,
      Genre,
      Director1,
      Cast1,
      Cast2,
      Budget,
      Revenue,
      CASE
        WHEN Revenue > 728100000 THEN 'Elite BlockBuster'
        WHEN Revenue BETWEEN 160000000 AND 728100000 THEN 'BlockBuster'
        WHEN Revenue BETWEEN 130000000 AND 160000000 THEN 'Regular'
        WHEN Revenue < 130000000 THEN 'Underdog'
        ELSE 'No Classification'
      END AS 'Movie_Range'
    FROM movies;
"
dbExecute(connection, query)

# Now we must have two tables in our database, the old and the new, lets check it.
dbListTables(connection)

# We will drop the old table, and also rename our new table to the same name. 

dbExecute(connection, "
            DROP TABLE movies;
          ")

dbExecute(connection, "
            ALTER TABLE new_movies RENAME TO movies;
          ")

dbListTables(connection)

dbGetQuery(connection, "
            PRAGMA table_info(movies);
           ") %>% View()


# Great, now our new table is all correctly formatted and ready for filtering and analysis. Let's take a look.
dbGetQuery(connection, "
            SELECT *
            FROM movies;
           ") %>% View()

# We want to check the number of movies released each year.

movies_each_year <- dbGetQuery(connection, "
           SELECT
              STRFTIME('%Y', Release_Date) AS Year,
              COUNT(Movie_Title) AS number_of_movies
            FROM movies
            GROUP BY Year
            ORDER BY Year;
           ")
View(movies_each_year)

ggplot(data = movies_each_year, mapping = aes(x = Year, y = number_of_movies, fill = Year)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = number_of_movies), vjust = -0.17, color = "black")+
  labs(title = "Number of Movies by Year", x = "Year", y = "Movie Count")+
  scale_fill_viridis_d()+
  theme_minimal()


# We want to check revenues by each year.

revenue_each_year <- dbGetQuery(connection, "
                                  SELECT 
                                    STRFTIME('%Y', Release_Date) AS Year,
                                    SUM(Revenue) AS total_revenue
                                  FROM movies
                                  GROUP BY Year
                                  ORDER BY Year;
                                ")
View(revenue_each_year)

ggplot(data = revenue_each_year, mapping = aes(x = Year, y = total_revenue, fill = Year))+
  geom_bar(stat = "identity")+
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B"))+
  labs(title = "Revenues by Year", x = "Year", y = "Total Revenue")+
  scale_fill_viridis_d()+
  theme_minimal()


# Revenue comparison by top 5 genre.

revenue_each_genre <- dbGetQuery(connection, "
                                    SELECT 
                                      Genre, 
                                      SUM(Revenue) AS total_revenue
                                    FROM movies
                                    GROUP BY Genre
                                    ORDER BY total_revenue DESC
                                    LIMIT 5;
                                 ")
View(revenue_each_genre)

ggplot(data = revenue_each_genre, mapping = aes(x = total_revenue, y = Genre))+
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5)+
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-9, suffix = "B"))+
  labs(title = "Revenue by Genre", x = "Total Revenue", y = "Genre")+
  theme_minimal()
  


# The relationship between budget and revenue.

budget_revenue <- dbGetQuery(connection, "
                                SELECT Budget, Revenue
                                FROM movies
                                ORDER BY Budget DESC, Revenue DESC;
                             ")
View(budget_revenue)

correlation <- cor(budget_revenue$Budget, budget_revenue$Revenue, use = "complete.obs")
print(correlation)

ggplot(data = budget_revenue, mapping = aes(x = Budget, y = Revenue)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth()+
  labs(title = "Budget vs Revenue", x = "Budget", y = "Revenue") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) +  
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "M")) + 
  annotate("text", x=Inf, y =Inf, label = paste("r=", correlation))+
  coord_cartesian(clip = "off")









