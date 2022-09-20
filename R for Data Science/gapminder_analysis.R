# #############################   
# ######### LIBRARIES #########   
# #############################   
library(dplyr)
library(ggplot2)
library(plotly)


# #############################
# ######### CONSTANTS #########
# #############################
FILE_PATH <- 
  "D:/DE/projects/R/wd/Bioinformatics/Bioinformatics Research Network/02 R for Data Science/gapminder_clean.csv"

# Plot titles
CO2_EMISSIONS <- "CO2 Emissions (Metric Tons per Capita)"
CONTINENT <- "Continent"
COUNTRY   <- "Country"
ENERGY_USE_KG_OIL_EQUIV_PER_CAPITA <- "Energy Use - KG of Oil Equivalent per Capita"
GDP_PER_CAPITA <- "GDP per Capita"
CO2_EMMISIONS_METRIC_TONS_PER_CAPITA <- "CO2 Emmisions - Metric Tons per Capita"
IMPORTS_GOODS_SERVICES_GDP <- "Imports of Goods and Services of GDP"
INCREASE_LIFE_EXPECTANCY   <- "Increase in Life Expectancy"
POPULATION_DENSITY_PEOPLE_KM2 <- "Population Density (People/KM2)"
YEAR <- "Year"


# #############################
# ########## PROCESS ##########
# #############################

# #### PART 1 ####
# Read data file
gapminder_clean <- read.csv(FILE_PATH)

# Clean column names
gapminder_clean$X <- NULL

gapminder_clean <- gapminder_clean %>%
  rename(
    country.name = Country.Name,
    year = Year,
    agriculture.value.added.of.gdp = Agriculture..value.added....of.GDP.,
    co2.emissions.metric.tons.per.capita = CO2.emissions..metric.tons.per.capita.,
    domestic.credit.provided.by.financial.sector.of.gdp = 
      Domestic.credit.provided.by.financial.sector....of.GDP.,
    electric.power.consumption.kWh.per.capita = Electric.power.consumption..kWh.per.capita.,
    energy.use.kg.of.oil.equivalent.per.capita = Energy.use..kg.of.oil.equivalent.per.capita.,
    exports.of.goods.and.services.of.gdp = Exports.of.goods.and.services....of.GDP.,
    fertility.rate.total.births.per.woman = Fertility.rate..total..births.per.woman.,
    gdp.growth.annual = GDP.growth..annual...,
    imports.of.goods.and.services.of.gdp = Imports.of.goods.and.services....of.GDP.,
    industry.value.added.of.gdp = Industry..value.added....of.GDP.,
    inflation.gdp.deflator.annual = Inflation..GDP.deflator..annual...,
    life.expectancy.at.birth.total.years = Life.expectancy.at.birth..total..years.,
    population.density.people.per.sq.km.of.land.area = Population.density..people.per.sq..km.of.land.area.,
    services.etc.value.added.of.gdp = Services..etc...value.added....of.GDP.
  )

# Filter the data to include only rows where Year is 1962
gapminder_1962 <- gapminder_clean %>%
  filter((year == 1962))

# Outlier: Make a scatter plot comparing 'CO2 emissions (metric tons per capita)' and gdpPercap 
# for the filtered data
gapminder_1962_complete_cases <- gapminder_1962 %>%
  filter(!(is.na(co2.emissions.metric.tons.per.capita) | is.na(gdpPercap)))

ggplotly(
  ggplot(
  gapminder_1962_complete_cases,
  aes(
    x = gdpPercap,
    y = co2.emissions.metric.tons.per.capita
  )
) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = GDP_PER_CAPITA,
    y = CO2_EMMISIONS_METRIC_TONS_PER_CAPITA
  ))

# Calculate the correlation of 'CO2 emissions (metric tons per capita)' and gdpPercap
correlation_outlier <- cor.test(
  gapminder_1962_complete_cases$co2.emissions.metric.tons.per.capita,
  gapminder_1962_complete_cases$gdpPercap
)

# No outlier: Make a scatter plot comparing 'CO2 emissions (metric tons per capita)' 
# and gdpPercap for the filtered data
gapminder_1962_complete_cases_no_outlier <- gapminder_1962 %>%
  filter(!(is.na(co2.emissions.metric.tons.per.capita) | is.na(gdpPercap)) & (gdpPercap < 25000))

ggplotly(
  ggplot(
  gapminder_1962_complete_cases_no_outlier,
  aes(
    x = gdpPercap,
    y = co2.emissions.metric.tons.per.capita
  )
) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = GDP_PER_CAPITA,
    y = CO2_EMMISIONS_METRIC_TONS_PER_CAPITA
  ))

# Calculate the correlation of 'CO2 emissions (metric tons per capita)' and gdpPercap
correlation_no_outlier <- cor.test(
  gapminder_1962_complete_cases_no_outlier$co2.emissions.metric.tons.per.capita,
  gapminder_1962_complete_cases_no_outlier$gdpPercap
)

# On the unfiltered data, answer "In what year is the correlation between 'CO2 emissions 
# (metric tons per capita)' and gdpPercap the strongest"?
gapminder_clean_by_year <- gapminder_clean %>%
  select(year, co2.emissions.metric.tons.per.capita, gdpPercap, pop, continent)

gapminder_clean_by_year <- gapminder_clean_by_year[complete.cases(gapminder_clean_by_year), ]

correlation_by_year <- gapminder_clean_by_year %>%
  group_by(year) %>%
  summarise(correlation = cor.test(co2.emissions.metric.tons.per.capita, gdpPercap)$estimate[[1]])

strongest_correlation_by_year <- (correlation_by_year %>%
  arrange(desc(correlation)))$year[1]

# Filter the dataset to that year for the next step...
gapminder_strongest_correlation <- gapminder_clean %>%
  filter(year == strongest_correlation_by_year)

# Using plotly, create an interactive scatter plot comparing 'CO2 emissions (metric tons per capita)' 
# and gdpPercap, where the point size is determined by pop (population) and the color is determined 
# by the continent
ggplotly(ggplot(
  gapminder_strongest_correlation,
  aes(
    x = gdpPercap,
    y = co2.emissions.metric.tons.per.capita,
    color = continent,
    size = pop
  )
) +
  geom_point() +
  scale_x_log10() +
  labs(
    x = GDP_PER_CAPITA,
    y = CO2_EMMISIONS_METRIC_TONS_PER_CAPITA
  ))


# #### PART 2 ####
# What is the relationship between continent and 'Energy use (kg of oil equivalent per capita)'? 
# (stats test needed)
gapminder_clean_continent_energy <- gapminder_clean %>%
  select(year, continent, energy.use.kg.of.oil.equivalent.per.capita) %>%
  filter(continent != "")

gapminder_clean_continent_energy <- 
  gapminder_clean_continent_energy[complete.cases(gapminder_clean_continent_energy), ]

gapminder_clean_continent_energy$continent <- as.factor(gapminder_clean_continent_energy$continent)

ggplotly(
  ggplot(
  gapminder_clean_continent_energy,
  aes(
    x = continent,
    y = energy.use.kg.of.oil.equivalent.per.capita
  )
) +
  geom_point() +
  labs(
    x = CONTINENT,
    y = ENERGY_USE_KG_OIL_EQUIV_PER_CAPITA
  ))

ggplotly(
  ggplot(gapminder_clean_continent_energy) +
  geom_boxplot(aes(
    continent,
    energy.use.kg.of.oil.equivalent.per.capita
  )) +
  labs(
    x = CONTINENT,
    y = ENERGY_USE_KG_OIL_EQUIV_PER_CAPITA
  ))

# If your categorical variable is not dichotomous, you can use the Kruskal-Wallis test
levels(gapminder_clean_continent_energy$continent)
kruskal.test(gapminder_clean_continent_energy$continent ~ 
             gapminder_clean_continent_energy$energy.use.kg.of.oil.equivalent.per.capita)

# Is there a significant difference between Europe and Asia with respect to 'Imports of goods 
# and services (% of GDP)' in the years after 1990? (stats test needed)
gapminder_clean_europe_asia_1990 <- gapminder_clean %>%
  select(year, continent, imports.of.goods.and.services.of.gdp) %>%
  filter((year >= 1990) & ((continent == "Europe") | (continent == "Asia")))

gapminder_clean_europe_asia_1990 <- 
  gapminder_clean_europe_asia_1990[complete.cases((gapminder_clean_europe_asia_1990)), ]

ggplotly(
  ggplot(
  gapminder_clean_europe_asia_1990,
  aes(
    x = continent,
    y = imports.of.goods.and.services.of.gdp
  )
) +
  geom_point() +
  labs(
    x = CONTINENT,
    y = IMPORTS_GOODS_SERVICES_GDP
  ))

ggplotly(
  ggplot(gapminder_clean_europe_asia_1990) +
  geom_boxplot(aes(
    continent,
    imports.of.goods.and.services.of.gdp
  )) +
  labs(
    x = CONTINENT,
    y = IMPORTS_GOODS_SERVICES_GDP
  ))

gapminder_clean_europe_asia_1990$continent <- as.factor(gapminder_clean_europe_asia_1990$continent)
levels(gapminder_clean_europe_asia_1990$continent)

wilcox.test(
  gapminder_clean_europe_asia_1990$imports.of.goods.and.services.of.gdp[
    which(gapminder_clean_europe_asia_1990$continent == "Asia")],
  gapminder_clean_europe_asia_1990$imports.of.goods.and.services.of.gdp[
    which(gapminder_clean_europe_asia_1990$continent == "Europe")]
)

# What is the country (or countries) that has the highest 'Population density (people per sq. km 
# of land area)' across all years? (i.e., which country has the highest average ranking in this 
# category across each time point in the dataset?)
highest_pop_density <- gapminder_clean %>%
  select(year, country.name, population.density.people.per.sq.km.of.land.area)

highest_pop_density <- highest_pop_density[complete.cases(highest_pop_density), ]
highest_pop_density <- highest_pop_density %>%
  group_by(year, country.name) %>%
  summarise(pop_density = population.density.people.per.sq.km.of.land.area) %>%
  arrange(year, desc(pop_density)) %>%
  top_n(1, pop_density)

ggplotly(ggplot(
  highest_pop_density,
  aes(
    x = year,
    y = pop_density,
    color = country.name
  )
) +
  labs(
    x = YEAR,
    y = POPULATION_DENSITY_PEOPLE_KM2,
    colour = COUNTRY
  ) +
  geom_line() +
  geom_point(aes(year)))

# test...
y <- gapminder_clean %>%
  select(year, country.name, population.density.people.per.sq.km.of.land.area)

y <- y[complete.cases(y), ]
y <- y %>%
  group_by(year) %>%
  summarise(pop_density = max(population.density.people.per.sq.km.of.land.area))

# What country (or countries) has shown the greatest increase in 'Life expectancy at birth, total (years)' 
# since 1962?
increase_life_expectancy <- gapminder_clean %>%
  select(year, country.name, life.expectancy.at.birth.total.years)

increase_life_expectancy <- increase_life_expectancy[complete.cases(increase_life_expectancy), ]
increase_life_expectancy <- increase_life_expectancy %>%
  filter(year >= 1962) %>%
  group_by(year, country.name) %>%
  summarise(life_expectancy = life.expectancy.at.birth.total.years) %>%
  arrange(year, desc(life_expectancy)) %>%
  top_n(1, life_expectancy)

ggplotly(ggplot(increase_life_expectancy, aes(x = year, y = life_expectancy, color = country.name)) +
  labs(
    x = YEAR,
    y = INCREASE_LIFE_EXPECTANCY,
    colour = COUNTRY
  ) +
  geom_line() +
  geom_point(aes(year)))
