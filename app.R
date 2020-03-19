#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)

# ---------------------------- UI -----------------------------------
ui <- fluidPage(
   
   # Application title
   titlePanel("COVID-19 Visualizations"
            ),
   p("by: Joshua Paik"),
   p("email: joshdpaik@gmail.com"),
   p("Data is from Johns Hopkins CSSE (https://github.com/CSSEGISandData/COVID-19)
     which is scraped from various sources and updated once daily. 
     This app is still in development. If you would like to contribute, please do! The
     repository is at https://github.com/joshp112358/COVID19-VIZ ."),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        
        selectInput("countries", "Countries",
                           c(
                             "World" = "World",
                             "World without China" = "World without China",
                             "Afghanistan" = "Afghanistan"
                             ,"Albania" = "Albania"
                             ,"Algeria" = "Algeria"
                             ,"Andorra" = "Andorra"
                             ,"Antigua and Barbuda" = "Antigua and Barbuda"
                             ,"Argentina" = "Argentina"
                             ,"Armenia" = "Armenia"
                             ,"Aruba" = "Aruba"
                             ,"Australia" = "Australia"
                             ,"Austria" = "Austria"
                             ,"Azerbaijan" = "Azerbaijan"
                             ,"Bahrain" = "Bahrain"
                             ,"Bangladesh" = "Bangladesh"
                             ,"Belarus" = "Belarus"
                             ,"Belgium" = "Belgium"
                             ,"Bhutan" = "Bhutan"
                             ,"Bolivia" = "Bolivia"
                             ,"Bosnia and Herzegovina" = "Bosnia and Herzegovina"
                             ,"Brazil" = "Brazil"
                             ,"Brunei" = "Brunei"
                             ,"Bulgaria" = "Bulgaria"
                             ,"Burkina Faso" = "Burkina Faso"
                             ,"Cambodia" = "Cambodia"
                             ,"Cameroon" = "Cameroon"
                             ,"Canada" = "Canada"
                             ,"Cayman Islands" = "Cayman Islands"
                             ,"Chile" = "Chile"
                             ,"China" = "China"
                             ,"Colombia" = "Colombia"
                             ,"Congo (Kinshasa)" = "Congo (Kinshasa)"
                             ,"Costa Rica" = "Costa Rica"
                             ,"Cote d'Ivoire" = "Cote d'Ivoire"
                             ,"Croatia" = "Croatia"
                             ,"Cruise Ship" = "Cruise Ship"
                             ,"Cuba" = "Cuba"
                             ,"Curacao" = "Curacao"
                             ,"Cyprus" = "Cyprus"
                             ,"Czechia" = "Czechia"
                             ,"Denmark" = "Denmark"
                             ,"Dominican Republic" = "Dominican Republic"
                             ,"Ecuador" = "Ecuador"
                             ,"Egypt" = "Egypt"
                             ,"Estonia" = "Estonia"
                             ,"Eswatini" = "Eswatini"
                             ,"Ethiopia" = "Ethiopia"
                             ,"Finland" = "Finland"
                             ,"France" = "France"
                             ,"French Guiana" = "French Guiana"
                             ,"Gabon" = "Gabon"
                             ,"Georgia" = "Georgia"
                             ,"Germany" = "Germany"
                             ,"Ghana" = "Ghana"
                             ,"Greece" = "Greece"
                             ,"Guadeloupe" = "Guadeloupe"
                             ,"Guatemala" = "Guatemala"
                             ,"Guernsey" = "Guernsey"
                             ,"Guinea" = "Guinea"
                             ,"Guyana" = "Guyana"
                             ,"Holy See" = "Holy See"
                             ,"Honduras" = "Honduras"
                             ,"Hungary" = "Hungary"
                             ,"Iceland" = "Iceland"
                             ,"India" = "India"
                             ,"Indonesia" = "Indonesia"
                             ,"Iran" = "Iran"
                             ,"Iraq = Iraq"
                             ,"Ireland" = "Ireland"
                             ,"Israel" = "Israel"
                             ,"Italy" = "Italy"
                             ,"Jamaica" = "Jamaica"
                             ,"Japan" = "Japan"
                             ,"Jersey" = "Jersey"
                             ,"Jordan" = "Jordan"
                             ,"Kazakhstan" = "Kazakhstan"
                             ,"Kenya" = "Kenya"
                             ,"Korea, South" = "Korea, South"
                             ,"Kuwait" = "Kuwait"
                             ,"Latvia" = "Latvia"
                             ,"Lebanon" = "Lebanon"
                             ,"Liechtenstein" = "Liechtenstein"
                             ,"Lithuania" = "Lithuania"
                             ,"Luxembourg" = "Luxembourg"
                             ,"Malaysia" = "Malaysia"
                             ,"Maldives" = "Maldives"
                             ,"Malta" = "Malta"
                             ,"Martinique" = "Martinique"
                             ,"Mauritania" = "Mauritania"
                             ,"Mexico" = "Mexico"
                             ,"Moldova" = "Moldova"
                             ,"Monaco" = "Monaco"
                             ,"Mongolia" = "Mongolia"
                             ,"Morocco" = "Morocco"
                             ,"Namibia" = "Namibia"
                             ,"Nepal" = "Nepal"
                             ,"Netherlands" = "Netherlands"
                             ,"New Zealand" = "New Zealand"
                             ,"Nigeria" = "Nigeria"
                             ,"North Macedonia" = "North Macedonia"
                             ,"Norway" = "Norway"
                             ,"Oman" = "Oman"
                             ,"Pakistan" = "Pakistan"
                             ,"Panama" = "Panama"
                             ,"Paraguay" = "Paraguay"
                             ,"Peru" = "Peru"
                             ,"Philippines" = "Philippines"
                             ,"Poland" = "Poland"
                             ,"Portugal" = "Portugal"
                             ,"Qatar" = "Qatar"
                             ,"Reunion" = "Reunion"
                             ,"Romania" = "Romania"
                             ,"Russia" = "Russia"
                             ,"Rwanda" = "Rwanda"
                             ,"Saint Lucia" = "Saint Lucia"
                             ,"Saint Vincent and the Grenadines" = "Saint Vincent and the Grenadines"
                             ,"San Marino" = "San Marino"
                             ,"Saudi Arabia" = "Saudi Arabia"
                             ,"Senegal" = "Senegal"
                             ,"Serbia" = "Serbia"
                             ,"Seychelles" = "Seychelles"
                             ,"Singapore" = "Singapore"
                             ,"Slovakia" = "Slovakia"
                             ,"Slovenia" = "Slovenia"
                             ,"South Africa" = "South Africa"
                             ,"Spain" = "Spain"
                             ,"Sri Lanka" = "Sri Lanka"
                             ,"Sudan" = "Sudan"
                             ,"Suriname" = "Suriname"
                             ,"Sweden" = "Sweden"
                             ,"Switzerland" = "Switzerland"
                             ,"Thailand" = "Thailand"
                             ,"Togo" = "Togo"
                             ,"Trinidad and Tobago" = "Trinidad and Tobago"
                             ,"Tunisia" = "Tunisia"
                             ,"Turkey" = "Turkey"
                             ,"Ukraine" = "Ukraine"
                             ,"United Arab Emirates" = "United Arab Emirates"
                             ,"United Kingdom" = "United Kingdom"
                             ,"Uruguay" = "Uruguay"
                             ,"US" = "US"
                             ,"Venezuela" = "Venezuela"
                             ,"Vietnam" = "Vietnam"
                             ),
                           selected = "World"),
        checkboxInput("logscale", "Log Scale", value = FALSE),
        checkboxInput("rawchange", "First Derivative", value = FALSE),
        checkboxInput("percChange", "Percent Change", value = FALSE)),
        
      mainPanel(
        tabsetPanel(id = "tab_being_displayed",
                    tabPanel("Confirmed, Survived, Dead",
                             
                             plotOutput("HistCSD"),
                             textOutput("CSD"),
                             p("This graphic shows the number of Confirmed, Survived, 
                               and Dead."),
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             br(),br(),br(),br(),br(),br()),
                    tabPanel("Deaths",
                          
                             plotOutput("Deaths"),
                             textOutput("DeathToll"),
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             br(),br(),br(),br(),br(),br()),
                    tabPanel("Deaths/(Deaths+Recovered)",
                             
                             plotOutput("HistDoverDR"),
                             textOutput("DDR"),
                             h2("Limitations"),
                             p("Dividing Deaths/(Deaths + Recovered) is preferred
                               to dividing Deaths/Confirmed. This is because 
                               a certain number of those with an unknown status will 
                               die. However, as different countries have
                               different recovery rates, for countries that have
                               been more recently afflicted, this
                               overestimates the mortality rate. However, on a global scale, this is 
                               more accurate than Deaths/Confirmed. Note if there are 0 deaths, an error will return under a log scale"),
                             
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             br(),br(),br(),br(),br(),br()),
                    tabPanel("Deaths/Confirmed",
              
                             plotOutput("HistDoverTotal"),
                             textOutput("Naive"),
                             h2("Limitations"),
                             p("Dividing Deaths/Confirmed underestimates the 
                               mortality rates of COVID. This is because a certain number of those 
                               with unknown status will die, but this is unaccounted for in the numerator.Note if there are 0 deaths, an error will return under a log scale"),
                             
                             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                             br(),br(),br(),br(),br(),br()))
      )
   )
)

# ---------------------------SERVER---------------------------------
server <- function(input, output) {
  url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
  url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
# ------ Data -----  
  ## Raw Data
  confirmed <- read_csv(url(url_confirmed))
  deaths <- read_csv(url(url_deaths))
  recovered <- read_csv(url(url_recovered))

  ## Per Country
  data_confirmed <- confirmed %>%
    select(-c(`Province/State`, Lat, Long)) %>%
    rename(Country = `Country/Region`) %>%
    group_by(Country) %>%
    summarize_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
    pivot_longer(-Country, names_to = "Date", values_to = "Confirmed")
  
  data_deaths <- read_csv(url(url_deaths)) %>%
    select(-c(`Province/State`, Lat, Long)) %>%
    rename(Country = `Country/Region`) %>%
    group_by(Country) %>%
    summarize_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
    pivot_longer(-Country, names_to = "Date", values_to = "Deaths")
  data_recovered <- read_csv(url(url_recovered)) %>%
    select(-c(`Province/State`, Lat, Long)) %>%
    rename(Country = `Country/Region`) %>%
    group_by(Country) %>%
    summarize_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
    pivot_longer(-Country, names_to = "Date", values_to = "Recovered")
  
  data_recovered$Date<-as.Date(data_recovered$Date,format = "%m/%d/%y")
  data_deaths$Date<-as.Date(data_deaths$Date,format = "%m/%d/%y")
  data_confirmed$Date<-as.Date(data_confirmed$Date,format = "%m/%d/%y")
  
  worldwide_confirmed <- colSums(confirmed[,5:length(confirmed)])
  worldwide_deaths <- colSums(deaths[,5:length(deaths)])
  worldwide_recovered <- colSums(recovered[,5:length(recovered)])
  
  WW_proportion <- worldwide_deaths/(worldwide_deaths+worldwide_recovered)*100
  dates <- as.Date(colnames(confirmed),format = "%m/%d/%y")
  
  WW_death_over_confirmed <- worldwide_deaths/worldwide_confirmed*100
  
  cconfirmed <- filter(confirmed,`Country/Region` != "China")
  ddeaths <- filter(deaths, `Country/Region` != "China")
  rrecovered <- filter(recovered, `Country/Region` != "China")
  
  worldwide_confirmed_noChina <- colSums(cconfirmed[,5:length(cconfirmed)])
  worldwide_deaths_noChina <- colSums(ddeaths[,5:length(ddeaths)])
  worldwide_recovered_noChina <- colSums(rrecovered[,5:length(rrecovered)])
  
  WW_proportion_noChina <- worldwide_deaths_noChina/(worldwide_deaths_noChina+worldwide_recovered_noChina)*100
  WW_death_over_confirmed_noChina <- worldwide_deaths_noChina/worldwide_confirmed_noChina*100

  
##helper 
  
  percChange <- function(df){
    y<-numeric()
    for(i in 2:length(df)){
      y<-append(y,(df[i]-df[i-1])/df[i-1] * 100)
    }
    return(y)
  }

## CODE -------
  
  output$CSD <- renderText({ 
    if (input$countries == "World"){
      paste("As of", dates[length(dates)],",",
            "the confirmed number of cases in the World is", 
            #tail(worldwide_confirmed,1),
            worldwide_confirmed[length(worldwide_confirmed)],
            ", the total number of deaths is", 
            #tail(worldwide_deaths,1),
            worldwide_deaths[length(worldwide_deaths)],
            ", and the total number recovered is", 
            worldwide_recovered[length(worldwide_recovered)])
    }
    else if (input$countries == "World without China"){
      paste("As of", dates[length(dates)],",",
            "the confirmed number of cases in the World without China is", 
            #tail(worldwide_confirmed_noChina,1),
            worldwide_confirmed_noChina[length(worldwide_confirmed_noChina)],
            ", the total number of deaths is", 
            #tail(worldwide_deaths_noChina,1),
            worldwide_deaths_noChina[length(worldwide_deaths_noChina)],
            ", and the total number recovered is", 
            worldwide_recovered_noChina[length(worldwide_recovered_noChina)])
    }
    else {
      Country_confirmed <- filter(data_confirmed, Country==input$countries)
      Country_deaths <- filter(data_deaths, Country==input$countries)
      Country_recovered <- filter(data_recovered, Country==input$countries)
      paste("As of", dates[length(dates)], ",",
            "the confirmed number of cases", "in", input$countries, "is", 
            tail(Country_confirmed$Confirmed,1),
            ", the total number of deaths is", 
            tail(Country_deaths$Deaths,1),
            ", and the total number recovered is", 
            tail(Country_recovered$Recovered,1),".")
    }
  })
  
## Plots Confirmed, Recovered, Dead
  output$HistCSD <- renderPlot({
    if (input$countries == "World"){
      #_____ start of Log Scale Boolean
      if (input$logscale== TRUE){
        # Start of raw change
        if (input$rawchange==TRUE){
          # Start of perc CHange
          if (input$percChange == TRUE){
            plot(percChange(diff(log(worldwide_confirmed)))~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Percentage Change of Log Scale \n
                               Daily Count in Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Percentage of Log Scale Count", type = "o")
            points(percChange(diff(log(worldwide_deaths)))~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(percChange(diff(log(worldwide_recovered)))~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
          #end of perc Change
          else{
            plot(diff(log(worldwide_confirmed))~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Log Scale Daily Count in Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Log Scale Count", type = "o")
            points(diff(log(worldwide_deaths))~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(diff(log(worldwide_recovered))~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
        }
        #end of raw change
        else{
          #Start of perc Change
          if (input$percChange == TRUE){
            plot(percChange(log(worldwide_confirmed))~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Percentage Change in Log Scale Count of Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Log Scale Count", type = "o")
            points(percChange(log(worldwide_deaths))~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(percChange(log(worldwide_recovered))~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
          #end of perc change
          else{
            
            plot(log(worldwide_confirmed)~as.Date(colnames(confirmed[,5:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Log Scale Count", type = "o")
            points(log(worldwide_deaths)~as.Date(colnames(deaths[,5:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(log(worldwide_recovered)~as.Date(colnames(recovered[,5:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
        }
      }
      #end of log scale
      else {
        #
        if(input$rawchange==TRUE){
          if(input$percChange == TRUE){
            plot(percChange(diff(worldwide_confirmed))~as.Date(colnames(confirmed[,7:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Percentage Change in Daily Count in Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Percentage", type = "o")
            points(percChange(diff(worldwide_deaths))~as.Date(colnames(deaths[,7:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(percChange(diff(worldwide_recovered))~as.Date(colnames(recovered[,7:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
          else {
            plot(diff(worldwide_confirmed)~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Daily Count in Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Count", type = "o")
            points(diff(worldwide_deaths)~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(diff(worldwide_recovered)~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
          
        }
        else{
          if(input$percChange == TRUE){
            plot(percChange(worldwide_confirmed)~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Percentage Change in Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Percentage", type = "o")
            points(percChange(worldwide_deaths)~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(percChange(worldwide_recovered)~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
            
          }
          else{
            plot(worldwide_confirmed~as.Date(colnames(confirmed[,5:length(confirmed)]),format = "%m/%d/%y"), col="red",
                 main =  paste(input$countries,"Confirmed Cases, Deaths, and Recovered"),
                 xlab= "Time", ylab = "Count", type = "o")
            points(worldwide_deaths~as.Date(colnames(deaths[,5:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
            points(worldwide_recovered~as.Date(colnames(recovered[,5:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
            legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                   col=c("red", "black", "green"),lty=1:1, cex=0.8)
          }
        }
      }
    }
    
    else if (input$countries == "World without China"){
      if (input$logscale== TRUE){
        if (input$rawchange==TRUE){
          plot(diff(log(worldwide_confirmed_noChina))~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
               main =  paste(input$countries,"Log Scale Daily Count in Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
          points(diff(log(worldwide_deaths_noChina))~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
          points(diff(log(worldwide_recovered_noChina))~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
        else {
          plot(log(worldwide_confirmed_noChina)~as.Date(colnames(confirmed[,5:length(confirmed)]),format = "%m/%d/%y"), col="red",
               main =  paste(input$countries,"Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
          points(log(worldwide_deaths_noChina)~as.Date(colnames(deaths[,5:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
          points(log(worldwide_recovered_noChina)~as.Date(colnames(recovered[,5:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
      }
      else {
        if (input$rawchange==TRUE){
          plot(diff(worldwide_confirmed_noChina)~as.Date(colnames(confirmed[,6:length(confirmed)]),format = "%m/%d/%y"), col="red",
               main =  paste(input$countries,"Daily Count in Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Count", type = "o")
          points(diff(worldwide_deaths_noChina)~as.Date(colnames(deaths[,6:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
          points(diff(worldwide_recovered_noChina)~as.Date(colnames(recovered[,6:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
        else{
          plot(worldwide_confirmed_noChina~as.Date(colnames(confirmed[,5:length(confirmed)]),format = "%m/%d/%y"), col="red",
               main =  paste(input$countries,"Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Count", type = "o")
          points(worldwide_deaths_noChina~as.Date(colnames(deaths[,5:length(deaths)]),format = "%m/%d/%y"), col="black", type = "o")
          points(worldwide_recovered_noChina~as.Date(colnames(recovered[,5:length(recovered)]),format = "%m/%d/%y"), col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
      }
    }
    
    else {
      
      if (input$logscale == TRUE) {
        if (input$rawchange == TRUE){
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          plot(diff(log(Country_confirmed$Confirmed))~Country_confirmed$Date[2:length(Country_deaths$Date)], col="red",
               main =  paste(input$countries,"Log Scale Daily Count in Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
          points(diff(log(Country_deaths$Deaths))~Country_deaths$Date[2:length(Country_deaths$Date)], col="black", type = "o")
          points(diff(log(Country_recovered$Recovered))~Country_recovered$Date[2:length(Country_deaths$Date)], col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
        else {
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          plot(log(Country_confirmed$Confirmed)~Country_confirmed$Date, col="red",
               main =  paste(input$countries,"Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
          points(log(Country_deaths$Deaths)~Country_deaths$Date, col="black", type = "o")
          points(log(Country_recovered$Recovered)~Country_recovered$Date, col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
      }
      
      else {
        if (input$rawchange == TRUE){
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          plot(diff(Country_confirmed$Confirmed)~Country_confirmed$Date[2:length(Country_confirmed$Date)], col="red",
               main =  paste(input$countries,"Daily Count in Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Count", type = "o")
          points(diff(Country_deaths$Deaths)~Country_deaths$Date[2:length(Country_deaths$Date)], col="black", type = "o")
          points(diff(Country_recovered$Recovered)~Country_recovered$Date[2:length(Country_recovered$Date)], col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
      
        else{
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          plot(Country_confirmed$Confirmed~Country_confirmed$Date, col="red",
               main =  paste(input$countries," Confirmed Cases, Deaths, and Recovered"),
               xlab= "Time", ylab = "Count", type = "o")
          points(Country_deaths$Deaths~Country_deaths$Date, col="black", type = "o")
          points(Country_recovered$Recovered~Country_recovered$Date, col="green", type = "o")
          legend("topleft", legend=c("Confirmed Cases", "Deaths", "Recovered "),
                 col=c("red", "black", "green"),lty=1:1, cex=0.8)
        }
      }
    }
  })
  
##------------ DEAD ---------------
  
  output$DeathToll <- renderText({
    if (input$countries == "World"){
      paste("As of",dates[length(dates)], 
            "the total number of Worldwide deaths is",
            tail(worldwide_deaths[length(worldwide_deaths)]))
    }
    else if (input$countries == "World without China"){
      paste("As of",dates[length(dates)], 
            "the total number of deaths in the World without China is",
            tail(worldwide_deaths_noChina[length(worldwide_deaths_noChina)]))
    }
    else {
      Country_deaths <- filter(data_deaths, Country==input$countries)
      paste("As of",dates[length(dates)], 
            "the total number of deaths in",input$countries ,"is",
            tail(Country_deaths$Deaths[length(Country_deaths$Deaths)]))
    }
  })
  
  output$Deaths <- renderPlot({
    
    if (input$countries == "World") {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          plot(diff(log(worldwide_deaths))~dates[6:length(dates)],
               main = paste(input$countries,"Log Scale Daily Count Deaths"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
        }
        else {
          plot(log(worldwide_deaths)~dates[5:length(dates)],
               main = paste(input$countries,"Deaths"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          plot(diff(worldwide_deaths)~dates[6:length(dates)],
               main = paste(input$countries,"Daily Count Deaths"),
               xlab= "Time", ylab = "Count", type = "o")
        }
        else {
          plot(worldwide_deaths~dates[5:length(dates)],
               main = paste(input$countries,"Deaths"),
               xlab= "Time", ylab = "Count", type = "o")
        }
        
      }
    }
    
    else if (input$countries == "World without China") {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          plot(diff(log(worldwide_deaths_noChina))~dates[6:length(dates)],
               main = paste(input$countries,"Log Scale Daily Count Deaths"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
        }
        else{
          plot(log(worldwide_deaths_noChina)~dates[5:length(dates)],
               main = paste(input$countries,"Daily Count Deaths"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          plot(diff(worldwide_deaths_noChina)~dates[6:length(dates)],
               main = paste(input$countries,"Daily Count Deaths"),
               xlab= "Time", ylab = "Count", type = "o")
        } 
        else {
          plot(worldwide_deaths_noChina~dates[5:length(dates)],
               main = paste(input$countries,"Deaths"),
               xlab= "Time", ylab = "Count", type = "o")
        }
      }
    }
    
    else {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          #Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          #Country_recovered <- filter(data_recovered, Country==input$countries)
          #Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(diff(log(Country_deaths$Deaths))~Country_deaths$Date[2:length(Country_deaths$Date)], 
               main = paste(input$countries,"Log Scale Daily Count Deaths"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
        }
        else{
          #Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          #Country_recovered <- filter(data_recovered, Country==input$countries)
          #Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(log(Country_deaths$Deaths)~Country_deaths$Date, 
               main = paste(input$countries,"Daily Count Deaths"),
               xlab= "Time", ylab = "Log Scale Count", type = "o")
        }
        
      }
      else {
        if (input$rawchange == TRUE){
          #Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          #Country_recovered <- filter(data_recovered, Country==input$countries)
          #Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(diff(Country_deaths$Deaths)~Country_deaths$Date[2:length(Country_deaths$Date)], 
               main = paste(input$countries,"Daily Count Deaths"),
               xlab= "Time", ylab = "Count", type = "o")
        }
        else {
          #Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          #Country_recovered <- filter(data_recovered, Country==input$countries)
          #Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(Country_deaths$Deaths~Country_deaths$Date, 
               main = paste(input$countries,"Deaths"),
               xlab= "Time", ylab = "Count", type = "o")
        }
      }
    }
  })
  
## Plots Dead/Dead+Recovered
  
  output$DDR <- renderText(
    if (input$countries == "World"){
      paste("As of", dates[length(dates)], ",",
            "the Dead/(Dead+Recovered) Rate in the World is",
            round(WW_proportion[length(WW_proportion)],2), "%")
    }
    
    else if (input$countries == "World without China"){
      paste("As of", dates[length(dates)], ",",
            "the Dead/(Dead+Recovered) Rate in the World without China is",
            round(WW_proportion_noChina[length(WW_proportion_noChina)],2), "%")
    }
    
    else {
      Country_deaths <- filter(data_deaths, Country==input$countries)
      Country_recovered <- filter(data_recovered, Country==input$countries)
      Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
      paste("As of", dates[length(dates)], ",",
            "the Dead/(Dead+Recovered) Rate in", input$countries, "is",
            round(Country_proportion[length(Country_proportion)],2), "%")
    }
  )
  
  output$HistDoverDR <- renderPlot({
    
    if (input$countries == "World") {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          plot(diff(log(WW_proportion))~dates[6:length(dates)],
               main = paste(input$countries,"Log Scale Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
        else {
          plot(log(WW_proportion)~dates[5:length(dates)],
               main = paste(input$countries,"Log Scale Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          plot(diff(WW_proportion)~dates[6:length(dates)],
               main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        else {
          plot(WW_proportion~dates[5:length(dates)],
               main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
      
      }
    }
    
    else if (input$countries == "World without China") {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          plot(diff(log(WW_proportion_noChina))~dates[6:length(dates)],
               main = paste(input$countries,"Log Scale Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        else{
          plot(log(WW_proportion_noChina)~dates[5:length(dates)],
               main = paste(input$countries,"Log Scale Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          plot(diff(WW_proportion_noChina)~dates[6:length(dates)],
               main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Percentage", type = "o")
        } 
        else {
        plot(WW_proportion_noChina~dates[5:length(dates)],
             main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
             xlab= "Time", ylab = "Percentage", type = "o")
        }
      }
    }
            
    else {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(diff(log(Country_proportion))~Country_recovered$Date[2:length(Country_recovered$Date)], 
               main = paste(input$countries,"Log Scale Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        else{
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(log(Country_proportion)~Country_recovered$Date, 
               main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        
      }
      else {
        if (input$rawchange == TRUE){
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(diff(Country_proportion)~Country_recovered$Date[2:length(Country_deaths$Date)], 
               main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
        else {
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          Country_recovered <- filter(data_recovered, Country==input$countries)
          Country_proportion <- Country_deaths$Deaths/(Country_deaths$Deaths+Country_recovered$Recovered)*100
          plot(Country_proportion~Country_recovered$Date, 
               main = paste(input$countries,"Deaths/(Deaths+Recovered)"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
      }
    }
  })
  
## Plots Dead/Confirmed
  
  output$Naive <- renderText(
    if (input$countries == "World"){
      paste("As of", dates[length(dates)], ",",
            "the Dead/Confirmed rate across the World is",
            round(WW_death_over_confirmed[length(WW_death_over_confirmed)],2), "%")
    }
    else if (input$countries == "World without China"){
      paste("As of", dates[length(dates)], ",",
            "the Dead/Confirmed rate across the World without China is",
            round(WW_death_over_confirmed_noChina[length(WW_death_over_confirmed_noChina)],2), "%")
    }
    else {
      Country_confirmed <- filter(data_confirmed, Country==input$countries)
      Country_deaths <- filter(data_deaths, Country==input$countries)
      proportion <- 100*Country_deaths$Deaths/Country_confirmed$Confirmed
      paste("As of", dates[length(dates)], ",",
            "the Dead/Dead+Recovered rate in ", input$countries, "is",
            round(proportion[length(proportion)],2), "%")
    }
  )
  
  #------
  output$HistDoverTotal <- renderPlot({
    if (input$countries == "World"){
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          plot(diff(log(WW_death_over_confirmed))~dates[6:length(dates)],
               main = paste(input$countries,"Log Scale Change per Day Deaths/Confirmed"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        else{
          plot(log(WW_death_over_confirmed)~dates[5:length(dates)],
               main = paste(input$countries,"Log Scale Deaths/Confirmed"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          plot(diff(WW_death_over_confirmed)~dates[6:length(dates)],
               main = paste(input$countries,"Change per Day Deaths/Confirmed"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
        else{
          plot(WW_death_over_confirmed~dates[5:length(dates)],
               main = paste(input$countries,"Deaths/Confirmed"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
      }
    }
    else if (input$countries == "World without China"){
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          plot(diff(log(WW_death_over_confirmed_noChina))~dates[6:length(dates)],
               main = paste(input$countries,"Log Scale Change per Day Deaths/Confirmed"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        else{
          plot(log(WW_death_over_confirmed_noChina)~dates[5:length(dates)],
               main = paste(input$countries,"Log Scale Deaths/Confirmed"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          plot(diff(WW_death_over_confirmed_noChina)~dates[6:length(dates)],
               main = paste(input$countries,"Change per Day Deaths/Confirmed"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
        else{
          plot(WW_death_over_confirmed_noChina~dates[5:length(dates)],
               main = paste(input$countries,"Deaths/Confirmed"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
      }
    }
    
    else {
      if (input$logscale == TRUE){
        if (input$rawchange == TRUE){
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          proportion <- 100*Country_deaths$Deaths/Country_confirmed$Confirmed
          plot(diff(log(proportion))~Country_confirmed$Date[2:length(Country_confirmed$Date)], 
               main =  paste(input$countries,"Log Scale Change per Day Deaths/Confirmed"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
        else {
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          proportion <- 100*Country_deaths$Deaths/Country_confirmed$Confirmed
          plot(log(proportion)~Country_confirmed$Date, 
               main =  paste(input$countries,"Log Scale Deaths/Confirmed"),
               xlab= "Time", ylab = "Log Scale Percentage", type = "o")
        }
      }
      else{
        if (input$rawchange == TRUE){
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          proportion <- 100*Country_deaths$Deaths/Country_confirmed$Confirmed
          plot(diff(proportion)~Country_confirmed$Date[2:length(Country_confirmed$Date)], 
               main =  paste(input$countries,"Change per Day Deaths/Confirmed"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
        else{
          Country_confirmed <- filter(data_confirmed, Country==input$countries)
          Country_deaths <- filter(data_deaths, Country==input$countries)
          proportion <- 100*Country_deaths$Deaths/Country_confirmed$Confirmed
          plot(proportion~Country_confirmed$Date, 
               main =  paste(input$countries,"Deaths/Confirmed"),
               xlab= "Time", ylab = "Percentage", type = "o")
        }
      }
    }
    
    
    
  })
}

# -----------------------------RUN-----------------------------------
shinyApp(ui = ui, server = server)

