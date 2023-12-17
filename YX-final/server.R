library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(sf)
library(png)
library(rsconnect)
library(shiny)
library(rnaturalearthdata)
library(leaflet)

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Define Cyprus coordinates
cyprus_coords <- data.frame(lon = 33.4299, lat = 35.1264)

# Plot the world map
plot_map<-ggplot(data = world) +
  # Fill countries with different colors and set the border color
  geom_sf(fill = "antiquewhite", color = "darkgrey") + 
  # Highlight Cyprus with a red point
  geom_point(data = cyprus_coords, aes(x = lon, y = lat), color = "red", size = 3) +
  # Add text label for Cyprus
  geom_text(data = cyprus_coords, aes(x = lon, y = lat, label = "Cyprus"), 
            color = "darkred", vjust = -1, nudge_y = 0.5, check_overlap = TRUE) +
  # Improve the theme
  theme_minimal() +
  # Remove axis labels and ticks
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank()) +
  # Add title
  labs(title = "Map of the World Highlighting Cyprus") 








data <- read.csv("Northern Cyprus/API_CYP_DS2_en_csv_v2_6260326.csv", skip=4)

# 1 GDP
economic_indicators <- subset(data, grepl("GDP|economy|inflation|unemployment|exports|imports|debt", data$Indicator.Name))
gdp_growth_data <- subset(data, grepl("GDP growth", data$Indicator.Name))
gdp_growth_data_long <- gather(gdp_growth_data, year, value, starts_with("X"))

gdp_growth_data_long$year <- as.numeric(sub("X", "", gdp_growth_data_long$year))

gdp_growth_data_long <- na.omit(gdp_growth_data_long)

gdp_growth_plot <- ggplot(gdp_growth_data_long, aes(x=year, y=value)) + 
  geom_line() + 
  geom_point() +
  ggtitle("GDP Growth Rate of Cyprus (Yearly)") + 
  xlab("Year") + 
  ylab("GDP Growth Rate (%)")



# Import and Export
data_long <- gather(data, year, value, -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code)


data_long$year <- as.numeric(sub("X", "", data_long$year))  
data_long$value <- as.numeric(as.character(data_long$value))  
data_long <- data_long %>% filter(!is.na(value))  


exports_data_long <- filter(data_long, grepl("Merchandise exports", Indicator.Name))
imports_data_long <- filter(data_long, grepl("Merchandise imports", Indicator.Name))


trade_plot <- ggplot() +
  geom_line(data=exports_data_long, aes(x=year, y=value, group=Indicator.Name, color="Exports")) +
  geom_line(data=imports_data_long, aes(x=year, y=value, group=Indicator.Name, color="Imports")) +
  ggtitle("Merchandise Exports and Imports of Cyprus (Yearly)") +
  xlab("Year") + ylab("Amount (US$)") +
  scale_color_manual(values=c("Exports"="blue", "Imports"="red")) +
  theme(legend.position = "bottom")



# Draw a graph of inflation and unemployment rates


inflation_data_long <- filter(data_long, grepl("Inflation, consumer prices", Indicator.Name))
unemployment_data_long <- filter(data_long, grepl("Unemployment", Indicator.Name))

inflation_plot <- ggplot(inflation_data_long, aes(x=year, y=value)) +
  geom_line(color="orange") +
  geom_point() +
  ggtitle("Inflation Rate in Cyprus (Yearly)") +
  xlab("Year") +
  ylab("Inflation Rate (%)")

unemployment_plot <- ggplot(unemployment_data_long, aes(x=year, y=value)) +
  geom_line(color="green") +
  geom_point() +
  ggtitle("Unemployment Rate in Cyprus (Yearly)") +
  xlab("Year") +
  ylab("Unemployment Rate (%)")


population_indicators <- data[grep("population|birth|death", data$Indicator.Name, ignore.case = TRUE), ]


population_indicators_long <- gather(population_indicators, key = "Year", value = "Value", X1960:X2022)


population_indicators_long$Year <- as.numeric(sub("X", "", population_indicators_long$Year))
population_indicators_long <- population_indicators_long %>% 
  filter(!is.na(Value)) %>% 
  mutate(Value = as.numeric(Value))



data_frame <-population_indicators_long

data_frame$Year <- as.numeric(gsub("X", "", data_frame$Year))
data_frame$Value <- as.numeric(data_frame$Value)


key_indicators <- c("Population, female (% of total population)",
                    "Population ages 00-04, male (% of male population)",
                    "Life expectancy at birth, female (years)",
                    "Urban population (% of total population)")


key_data <- data_frame %>% 
  filter(Indicator.Name %in% key_indicators)

# Create separate plots for each key indicator and store them in p1, p2, p3, and p4
plots <- list()

for (i in 1:length(key_indicators)) {
  indicator <- key_indicators[i]
  plot_data <- key_data[key_data$Indicator.Name == indicator, ]
  p <- ggplot(plot_data, aes(x = Year, y = Value, color = Indicator.Name)) +
    geom_line() +
    theme_minimal() +
    labs(title = paste("Key Population Indicator:", indicator),
         x = "Year", y = "Value", color = "Indicator") +
    theme(legend.position = "bottom")
  plots[[i]] <- p
}

# Store the plots in p1, p2, p3, and p4
p1 <- plots[[1]]
p2 <- plots[[2]]
p3 <- plots[[3]]
p4 <- plots[[4]]


cyprus_data <- read.csv("Northern Cyprus/API_CYP_DS2_en_csv_v2_6260326.csv", skip = 4)
greece_data <- read.csv("Greece//API_GRC_DS2_en_csv_v2_6230020.csv", skip = 4)


# Extracting GDP, Unemployment rate, and Education expenditure data
cyprus_gdp <- cyprus_data[cyprus_data$Indicator.Code == "NY.GDP.MKTP.CD",]
cyprus_unemployment <- cyprus_data[cyprus_data$Indicator.Code == "SL.UEM.TOTL.ZS",]
cyprus_education <- cyprus_data[cyprus_data$Indicator.Code == "SE.XPD.TOTL.GD.ZS",]

greece_gdp <- greece_data[greece_data$Indicator.Code == "NY.GDP.MKTP.CD",]
greece_unemployment <- greece_data[greece_data$Indicator.Code == "SL.UEM.TOTL.ZS",]
greece_education <- greece_data[greece_data$Indicator.Code == "SE.XPD.TOTL.GD.ZS",]

# Converting to numeric type
cyprus_gdp_values <- as.numeric(cyprus_gdp[20:67])
greece_gdp_values <- as.numeric(greece_gdp[20:67])

cyprus_unemployment_values <- as.numeric(cyprus_unemployment[36:67])
greece_unemployment_values <- as.numeric(greece_unemployment[36:67])

cyprus_education_values <- as.numeric(cyprus_education[18:67])
greece_education_values <- as.numeric(greece_education[18:67])

# Years
years_gdp <- as.numeric(1975:2022)

# Plotting GDP comparison
p_gdp_car <- ggplot() +
  geom_line(aes(x = years_gdp, y = cyprus_gdp_values, colour = "Cyprus GDP")) +
  geom_line(aes(x = years_gdp, y = greece_gdp_values, colour = "Greece GDP")) +
  labs(title = "GDP Comparison (Cyprus vs Greece)", x = "Year", y = "GDP (current US$)") +
  scale_colour_manual(values = c("Cyprus GDP" = "blue", "Greece GDP" = "red"))

# Creating GDP table
gdp_table <- data.frame(
  Year = years_gdp,
  Cyprus_GDP = cyprus_gdp_values,
  Greece_GDP = greece_gdp_values
)






# Plotting Unemployment Rate comparison


years_unr <- as.numeric(1991:2022)

p_unr_car <- ggplot() +
  geom_line(aes(x = years_unr, y = cyprus_unemployment_values, colour = "Cyprus Unemployment Rate")) +
  geom_line(aes(x = years_unr, y = greece_unemployment_values, colour = "Greece Unemployment Rate")) +
  labs(title = "Unemployment Rate Comparison (Cyprus vs Greece)", x = "Year", y = "Unemployment Rate (%)") +
  scale_colour_manual(values = c("Cyprus Unemployment Rate" = "blue", "Greece Unemployment Rate" = "red"))


# Creating Unemployment Rate table
unemployment_table <- data.frame(
  Year = years_unr,
  Cyprus_Unemployment_Rate = as.numeric(cyprus_unemployment[36:67]),
  Greece_Unemployment_Rate = as.numeric(greece_unemployment[36:67])
)





# Plotting Education Expenditure comparison

years_edu <- as.numeric(1973:2022)


p_edu_car <- ggplot() +
  geom_line(aes(x = years_edu, y = cyprus_education_values, colour = "Cyprus Education Expenditure")) +
  geom_line(aes(x = years_edu, y = greece_education_values, colour = "Greece Education Expenditure")) +
  labs(title = "Education Expenditure Comparison (Cyprus vs Greece)", x = "Year", y = "Education Expenditure (% of GDP)") +
  scale_colour_manual(values = c("Cyprus Education Expenditure" = "blue", "Greece Education Expenditure" = "red"))

# Creating Education Expenditure table
education_table <- data.frame(
  Year = years_edu,
  Cyprus_Education_Expenditure = as.numeric(cyprus_education[18:67]),
  Greece_Education_Expenditure = as.numeric(greece_education[18:67])
)










gdp_table<- tail(gdp_table, 10)



unemployment_table<-tail(unemployment_table,10)


education_table <-tail(education_table,10)






library(shiny)

# Define server logic
server <- function(input, output) {
  
  rsconnect::setAccountInfo(
    name='xyyzwds',
    token='1A0C4C94640EE53A27482EEA418760E6',
    secret='qYMCUotEryHFfb6/8InKaRmUvDfAavxn7yB45A9/'
  )
  # Dynamically render content based on directory selection
  output$content <- renderUI({
    section <- input$section
    
    if (section == "Map") {
      return(tabsetPanel(
        tabPanel("Map 1",
                 plotOutput("mapPlot"),
                 HTML("<h3>World Map Highlighting Cyprus</h3>
<p>
    <strong>The first image is a world map highlighting the location of Cyprus.</strong> Cyprus is distinctly marked, showcasing its strategic position as an island country in the Eastern Mediterranean Sea. Its geographical location is characterized by:
</p>
<ul>
    <li>Being to the <strong>south of Turkey</strong>,</li>
    <li>To the <strong>west of Syria and Lebanon</strong>,</li>
    <li><strong>Northwest of Israel</strong>,</li>
    <li><strong>North of Egypt</strong>,</li>
    <li>And <strong>southeast of Greece</strong>.</li>
</ul>
<p>
    The map’s use of a red dot to mark Cyprus effectively draws attention to its unique and central position in the region.
</p>
")
        ),
        tabPanel("Map 2",
                 leafletOutput("map"),
                 
                 HTML("<h3>Detailed Map of Cyprus</h3>
<p>
    <strong>The second image appears to be a more detailed map of Cyprus itself,</strong> highlighting various administrative regions or cities within the island. Notable features of this map include:
</p>
<ul>
    <li>The <strong>northern part of the island highlighted in pink</strong>, indicating a region or attribute of special interest, such as a separate administrative area or a zone with special status,</li>
    <li>Inclusion of significant cities like <strong>Girne, Lefkoşa, and Gazimağusa</strong>, which play crucial roles in the country's geography and culture.</li>
</ul>
<p>
    This map provides a closer look at the internal divisions and important urban areas of Cyprus, offering insights into its regional dynamics.
</p>
")
        )
      ))
    } else if (section == "Economy") {
      return(tabsetPanel(
        tabPanel("GDP",
                 plotOutput("gdpPlot"),
                 HTML("<h3>Cyprus Annual GDP Growth Rate Analysis</h3>
<p>
    <strong>This chart shows the annual GDP growth rate of Cyprus.</strong> Significant fluctuations are observed in different years, which may reflect the dynamic nature of the economic environment. Key factors influencing these variations include:
</p>
<ul>
    <li>Changes in the <strong>global economic environment</strong>,</li>
    <li>Adjustments in <strong>domestic policies</strong>,</li>
    <li>Other <strong>macroeconomic factors</strong>.</li>
</ul>
<p>
    Notably, certain years have experienced <strong>significant declines</strong> in GDP growth rates, potentially linked to <strong>global financial crises</strong> or <strong>regional economic downturns</strong>.
</p>
")
        ),
        tabPanel("Import and Export",
                 plotOutput("tradePlot"),
                 HTML("<h3>Cyprus Import and Export Analysis</h3>
<p>
    <strong>This chart provides a comparative analysis of Cyprus's export and import amounts over recent years.</strong> Key observations and factors influencing these trends include:
</p>
<ul>
    <li>Fluctuations in export and import amounts, reflecting changes in the <strong>global trade environment</strong>,</li>
    <li>Impact of <strong>international market demand</strong>,</li>
    <li>And Cyprus's <strong>domestic production capacity</strong>.</li>
</ul>
<p>
    By examining the scale of exports versus imports, insights into Cyprus's trade balance are gained. A significant excess of imports over exports in any particular year, for instance, might indicate a <strong>trade deficit</strong>, highlighting economic challenges.
</p>
")
        ),
        tabPanel("Inflation and Unemployment Rates",
                 plotOutput("inflationPlot"),
                 plotOutput("unemploymentPlot"),
                 HTML("<h3>Inflation and Unemployment Rates in Cyprus</h3>
<p>
    <strong>The charts displayed showcase the annual inflation rate and unemployment rate in Cyprus.</strong> These indicators provide valuable insights into the country's macroeconomic stability:
</p>
<ul>
    <li>The left graph illustrates the <strong>annual inflation rate</strong>, with its fluctuations revealing the stability of consumer prices and monetary value changes,</li>
    <li>The right graph depicts the <strong>annual unemployment rate</strong>, a reflection of the job market's health and the intensity of economic activity.</li>
</ul>
<p>
    Together, these two metrics offer a comprehensive view of Cyprus's macroeconomic conditions. Notably, high rates of inflation or unemployment typically signal underlying economic challenges.
</p>
")
        )
      ))
    } else if (section == "Population") {
      return(tabsetPanel(
        tabPanel("Population Female",
                 plotOutput("p1"),
                 HTML("<h3>Female Proportion of the Population in Cyprus</h3>
<p>
    <strong>The chart illustrating the proportion of women in the population</strong> (represented by the blue line) demonstrates:
</p>
<ul>
    <li>Consistent maintenance of the gender ratio around <strong>50%</strong> over the years, indicating a balanced gender distribution,</li>
    <li>A noticeable decline around 1990, possibly reflecting the impact of specific socio-economic events at that time.</li>
</ul>
")
        ),
        tabPanel("0-4 Year Old Boys to Male Population",
                 plotOutput("p2"),
                 HTML("<h3>Proportion of 0-4 Year Old Boys to Male Population</h3>
<p>
    <strong>This indicator (shown by the green line)</strong> has experienced a downward trend since 1960, suggesting:
</p>
<ul>
    <li>A decreasing proportion of young male population,</li>
    <li>Potentially related to a decline in fertility rates.</li>
</ul>
")
        ),
        tabPanel("Life Expectancy of Women of Reproductive Age",
                 plotOutput("p3"),
                 HTML("<h3>Life Expectancy of Women of Reproductive Age in Cyprus</h3>
<p>
    <strong>Indicated by the red line,</strong> this trend reflects:
</p>
<ul>
    <li>An upward trajectory from around 70 years in 1960 to approximately 80 years in recent years,</li>
    <li>Signifying improvements in health, medical services, and overall quality of life in Cyprus.</li>
</ul>
")
        ),
        tabPanel("Urban Population to Total Population",
                 plotOutput("p4"),
                 HTML("<h3>Urban Population Proportion of Total Population in Cyprus</h3>
<p>
    <strong>Represented by the purple line,</strong> this chart shows:
</p>
<ul>
    <li>Growth from less than 40% in 1960 to nearly 70%,</li>
    <li>Illustrating significant urbanization in Cyprus, likely tied to economic development, increased employment opportunities, and enhanced living facilities.</li>
</ul>
")
        )
      ))
    } else if (section == "Comparison") {
      return(tabsetPanel(
        tabPanel("GDP comparison",
                 plotOutput("p_gdp_car"),
                 tableOutput("gdp_table"),
                 HTML("<h3>GDP Comparison Between Cyprus and Greece</h3>
<p>
    <strong>Observing the GDP trends of Cyprus and Greece,</strong> we note:
</p>
<ul>
    <li>Some volatility in both economies, with Greece's GDP generally being higher than that of Cyprus,</li>
    <li>Greece's GDP growth rate has been faster over the past few decades despite economic fluctuations,</li>
    <li>Significant declines in GDP for both countries during economic crises, especially post-2008, with Greece being more severely affected.</li>
</ul>
")
        ),
        tabPanel("Unemployment Rate comparison",
                 plotOutput("p_unr_car"),
                 tableOutput("unemployment_table"),
                 HTML("<h3>Unemployment Rate Comparison Between Cyprus and Greece</h3>
<p>
    <strong>The unemployment rate trends in Cyprus and Greece highlight:</strong>
</p>
<ul>
    <li>Fluctuations in both countries, with Greece's unemployment rate generally higher,</li>
    <li>A sharp increase in Greece's unemployment rate following the 2008 economic crisis, peaking more significantly than in Cyprus,</li>
    <li>A recent downward trend in unemployment rates in both countries, although Greece's decline is slower.</li>
</ul>
")
        ),
        tabPanel("Education Expenditure comparison",
                 plotOutput("p_edu_car"),
                 tableOutput("education_table"),
                 HTML("<h3>Education Expenditure Comparison Between Cyprus and Greece</h3>
<p>
    <strong>Comparing education expenditure as a percentage of GDP,</strong> we find:
</p>
<ul>
    <li>Cyprus generally allocates a higher proportion of its GDP to education than Greece,</li>
    <li>The proportion of education expenditure in Cyprus usually remains high despite fluctuations,</li>
    <li>Economic instabilities may have impacted the educational investments of both countries, especially during difficult economic periods.</li>
</ul>
")
        ), tabPanel("Summary",
                    plotOutput("summary_plot"),
                    HTML("<h3>Summary of Comparative Analysis</h3>
<p>
    By comparing GDP, unemployment rates, and education expenditure between Cyprus and Greece, we can discern:
</p>
<ul>
    <li>Different economic scales and labor market challenges faced by each country,</li>
    <li>Greece, with a larger economy, surpasses Cyprus in economic output but struggles with higher unemployment rates and lower education investment ratios,</li>
    <li>Cyprus, despite its smaller economic scale, demonstrates a higher commitment to education expenditure.</li>
</ul>
<p>
    These differences reflect the distinct economic structures, policy priorities, and challenges of each country.
</p>
")
        )
      ))
    } else if (section == "SWOT Analysis") {
      return(
        HTML("<h3>SWOT Analysis of Cyprus</h3>
              <h4>Strengths</h4>
              <p>- Strategic geographic location<br>
                 - Robust tourism sector<br>
                 - Mature service industries<br>
                 - EU membership benefits</p>
              <h4>Weaknesses</h4>
              <p>- Limited economic diversity<br>
                 - Political division<br>
                 - Relatively high unemployment rate<br>
                 - Limited natural resources</p>
              <h4>Opportunities</h4>
              <p>- Development of renewable energy<br>
                 - Diversification in tourism<br>
                 - Regional cooperation<br>
                 - Technology and innovation</p>
              <h4>Threats</h4>
              <p>- Regional instability<br>
                 - Impact of global economic fluctuations<br>
                 - Climate change risks<br>
                 - International competition in tourism and services</p>")
      )
    }
  })
  
  
  
  
  # Output for the world map
  output$mapPlot <- renderPlot({
    plot_map
  })
  
  # Output for the PNG image
  northern_cyprus <- st_read("northern cyprus.geojson", quiet = TRUE)
  
  output$map <- renderLeaflet({
    leaflet(data = northern_cyprus) %>%
      addTiles() %>%
      addPolygons()  
  })
  
  # Output for GDP Growth Rate
  output$gdpPlot <- renderPlot({
    gdp_growth_plot
  })
  
  # Output for Import and Export
  output$tradePlot <- renderPlot({
    trade_plot
  })
  
  # Output for Inflation Rate
  output$inflationPlot <- renderPlot({
    inflation_plot
  })
  
  # Output for Unemployment Rate
  output$unemploymentPlot <- renderPlot({
    unemployment_plot
  })
  
  # Output for Population Female
  output$p1 <- renderPlot({
    p1
  })
  
  # Output for 0-4 year old boys to male population
  output$p2 <- renderPlot({
    p2
  })
  
  # Output for Life expectancy of women of reproductive age
  output$p3 <- renderPlot({
    p3
  })
  
  # Output for Urban population to total population
  output$p4 <- renderPlot({
    p4
  })
  output$gdp_table<- renderTable({
    
    gdp_table
  })
  
  # Output for GDP comparison image
  output$p_gdp_car <- renderPlot({
    p_gdp_car
  })
  
  output$unemployment_table <- renderTable({
    
    unemployment_table
  })
  
  
  
  
  # Output for Unemployment Rate comparison image
  output$p_unr_car <- renderPlot({
    p_unr_car
  })
  
  
  output$education_table <- renderTable({
    
    education_table
  })
  
  
  
  
  
  # Output for Education Expenditure comparison image
  output$p_edu_car <- renderPlot({
    p_edu_car
  })
  
  
  # Output for Education Expenditure comparison image
  output$summary_plot <- renderPlot({
    img <- readPNG("125.png")
    plot(1:2, type = "n", xlab = "", ylab = "", xlim = c(0, 1), ylim = c(0, 1))
    rasterImage(img, 0, 0, 1, 1)
  })
  
  
}
