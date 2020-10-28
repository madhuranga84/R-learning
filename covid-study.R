library(magrittr) # pipe operations
library(lubridate) # date operations
library(tidyverse) # ggplot2, tidyr, dplyr... 
library(gridExtra) # multiple grid-based plots on a page 
library(ggforce) # accelerating ggplot2 
library(kableExtra) # complex tables
library(leaflet) # map

## source data files
#filenames <- c('time_series_covid19_confirmed_global.csv', 'time_series_covid19_deaths_global.csv',
#               'time_series_covid19_recovered_global.csv')
#url.path <- paste0('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/',
#                   'master/csse_covid_19_data/csse_covid_19_time_series/')
## download files to local
#download <- function(filename) {
#  url <- file.path(url.path, filename)
#  dest <- file.path('~/Desktop/Chuti-me/Covid-19-R-studies/new_data', filename) 
#  download.file(url, dest)
#}

#bin <- lapply(filenames, download)


setwd('~/Desktop/Chuti-me/Covid-19-R-studies/COVID-19-master/csse_covid_19_data/')
setwd('csse_covid_19_time_series')
getwd()

raw.data.confirmed <- read.csv('time_series_covid19_confirmed_global.csv') 
raw.data.deaths <- read.csv('time_series_covid19_deaths_global.csv')
raw.data.recovered <- read.csv('time_series_covid19_recovered_global.csv')

n.col <- ncol(raw.data.confirmed)
## get dates from column names
dates <- names(raw.data.confirmed)[5:n.col] %>% substr(2,8) %>% mdy() 
range(dates)

min.date <- min(dates)
max.date <- max(dates)
min.date.txt <- min.date %>% format('%d %b %Y')
max.date.txt <- max.date %>% format('%d %b %Y') %>% paste('UTC')

## data cleaning and transformation
cleanData <- function(data) {
  ## remove some columns
  data %<>% select(-c(Province.State, Lat, Long)) %>% rename(country=Country.Region) 
  ## convert from wide to long format
  data %<>% gather(key=date, value=count, -country)
  ## convert from character to date
  data %<>% mutate(date = date %>% substr(2,8) %>% mdy())
  data %<>% group_by(country, date) %>% summarise(count=sum(count, na.rm=T)) %>% as.data.frame()
  return(data) 
  }


filter <- stats::filter()

## clean the three datasets
data.confirmed <- raw.data.confirmed %>% cleanData() %>% rename(confirmed=count) 
data.deaths <- raw.data.deaths %>% cleanData() %>% rename(deaths=count) 
data.recovered <- raw.data.recovered %>% cleanData() %>% rename(recovered=count)

## merge above 3 datasets into one, by country and date
data <- data.confirmed %>% merge(data.deaths, all=T) %>% merge(data.recovered, all=T) 
# data %<>% mutate(recovered = ifelse(is.na(recovered), lag(recovered, 1), recovered))

## countries/regions with confirmed cases, excl. cruise ships
countries <- data %>% pull(country) %>% setdiff('Cruise Ship')

## first 10 records when it first broke out in China
data %>% filter(country=='China') %>% head(10) %>%
  kable('latex', booktabs=T, caption='Raw Data (with first 10 Columns Only)',
        format.args=list(big.mark=',')) %>%
  kable_styling(latex_options = c('striped', 'hold_position', 'repeat_header'))

## counts for the whole world
data.world <- data %>% group_by(date) %>% 
  summarise(country='World',
            confirmed = sum(confirmed, na.rm=T), 
            deaths = sum(deaths, na.rm=T), 
            recovered = sum(recovered, na.rm=T))

data %<>% rbind(data.world)

## current confirmed cases
data %<>% mutate(current.confirmed = confirmed - deaths - recovered)

## sort by country and date
data %<>% arrange(country, date)

## sort by country and date
data %<>% arrange(country, date)
## daily increases of deaths and recovered cases ## set NA to the increases on day1
n <- nrow(data)
day1 <- min(data$date)

data %<>% mutate(new.confirmed = ifelse(date == day1, NA, confirmed - lag(confirmed, n=1)), 
                 new.deaths = ifelse(date == day1, NA, deaths - lag(deaths, n=1)),
                 new.recovered = ifelse(date == day1, NA, recovered - lag(recovered, n=1)))

## change negative number of new cases to zero
data %<>% mutate(
  new.confirmed = ifelse(new.confirmed < 0,0,new.confirmed),
  new.confirmed = ifelse(new.deaths < 0,0,new.deaths),
  new.recovered = ifelse(new.recovered < 0,0,new.recovered),
)

## death rate based on total deaths and recovered cases
data %<>% mutate(rate.upper = (100 * deaths / (deaths+recovered) )  %>% round(1))
data %<>% mutate(rate.lower = (100 * deaths / recovered )  %>% round(1))
data %<>% mutate(rate.daily = (100 * new.deaths / (new.deaths+new.recovered) )  %>% round(1))

## convert from wide to long format, for drawing area plots
data.long <- data %>%
  select(c(country, date, confirmed, current.confirmed, recovered, deaths)) %>% 
  gather(key=type, value=count, -c(country, date))
## set factor levels to show them in a desirable order
data.long %<>% mutate(type=recode_factor(type, confirmed='Total Confirmed', 
                                         current.confirmed='Current Confirmed',
                                         recovered='Recovered',
                                         deaths='Deaths'))

## convert from wide to long format, for drawing area plots
rates.long <- data %>%
  # filter(country %in% top.countries) %>%
  select(c(country, date, rate.upper, rate.lower, rate.daily)) %>% 
  # mutate(country=factor(country, levels=top.countries)) %>% 
  gather(key=type, value=count, -c(country, date))
  # set factor levels to show them in a desirable order
  rates.long %<>% mutate(type=recode_factor(type, rate.daily='Daily', 
                                            rate.lower='Lower bound',
                                            rate.upper='Upper bound'))
  
  
  ## select last column, which is the number of latest confirmed cases
  x <- raw.data.confirmed
  x$confirmed <- x[, ncol(x)]
  x %<>% select(c(Country.Region, Province.State, Lat, Long, confirmed)) %>%
    mutate(txt=paste0(Country.Region, ' - ', Province.State, ': ', confirmed))
  m <- leaflet(width=1200, height=800) %>% addTiles() 
  # circle marker (units in pixels)
  m %<>% addCircleMarkers(x$Long, x$Lat,
                          # radius=2+log2(x$confirmed),
                          radius=0.03*sqrt(x$confirmed), stroke=F,
                          color='red', fillOpacity=0.3, popup=x$txt)
  # world
  m
  
  world.long <- data.long %>% filter(country == 'World')
  ## cases - area plot
  plot1 <- world.long %>% filter(type != 'Total Confirmed') %>% 
    ggplot(aes(x=date, y=count)) +
    geom_area(aes(fill=type), alpha=0.5) +
    labs(title=paste0('Numbers of Cases Worldwide - ', max.date.txt)) + 
    scale_fill_manual(values=c('red', 'green', 'black')) + 
    theme(legend.title=element_blank(), 
          legend.position='bottom',
          plot.title = element_text(size=7), 
          axis.title.x=element_blank(), 
          axis.title.y=element_blank(),
          legend.key.size=unit(0.2, 'cm'), 
          legend.text=element_text(size=6), 
          axis.text=element_text(size=7), 
          axis.text.x=element_text(angle=45, hjust=1))
  
  plot2 <- world.long %>%
    ggplot(aes(x=date, y=count)) +
    geom_line(aes(color=type)) +
    labs(title=paste0('Numbers of Cases Worldwide (log scale) - ', max.date.txt)) + 
    scale_color_manual(values=c('purple', 'red', 'green', 'black')) + 
    theme(legend.title=element_blank(), legend.position='bottom',
          plot.title = element_text(size=7), 
          axis.title.x=element_blank(), 
          axis.title.y=element_blank(), 
          legend.key.size=unit(0.2, 'cm'), 
          legend.text=element_text(size=6), 
          axis.text=element_text(size=7), 
          axis.text.x=element_text(angle=45, hjust=1)) +
    scale_y_continuous(trans='log10') 
  ## show two plots side by side 
  grid.arrange(plot1, plot2, ncol=2)
  
  data.world <- data %>% filter(country=='World')
  n <- nrow(data.world)
  
  ## current confirmed and daily new confirmed
  plot1 <- ggplot(data.world, aes(x=date, y=current.confirmed)) + geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title='Current Confirmed Cases') + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  ## current confirmed and daily new confirmed
  plot2 <- ggplot(data.world, aes(x=date, y=new.confirmed)) + geom_point() + geom_smooth() +
    xlab('') + ylab('Count') + labs(title='Daily new Confirmed Cases') + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
  
  ## show two plots side by side
  grid.arrange(plot1, plot2, ncol=2)
  
