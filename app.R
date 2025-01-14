library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(tidyverse)
library(bslib)
library(shinyWidgets)
library(data.table)
load('dataUsed.RData')

ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "Cook  Islands"),
  
  dashboardSidebar(
    sidebarMenu(
      
      id = "Sidebar",
    
      ### MENUS
      menuItem("Homepage", tabName = "homepage", icon = icon("home"), badgeColor = "green"),
      menuItem("General Description", tabName = "general", icon = icon("info-circle"), startExpanded = TRUE,
               menuSubItem("Map", tabName = "map", icon = icon("map-marker")),
               menuSubItem("Key Facts", tabName = "facts", icon = icon("key")),
               menuSubItem("Narrative", tabName = "narrative", icon = icon("align-left"))
      ),
      menuItem("Key Demographics", tabName = "demographics", icon = icon("globe")),
      menuItem("Regional Comparison", tabName = "comparison", icon = icon("chart-bar")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("table"), badgeLabel = "hot!", badgeColor = "red"),
      menuItem("Reference & Special Thanks", tabName = "thanks", icon = icon("heart"))
    )
  ),
  
  dashboardBody(
    
    ## Set Font style and Background
    tags$head(
      tags$style(HTML("
      .main-header .logo {font-family: 'Comic Sans MS', sans-serif; font-size: 18}
      .main-sidebar {font-family: 'Comic Sans MS', sans-serif; font-size: 16}
      .content-wrapper, .content-wrapper * {font-family: 'Comic Sans MS', sans-serif;font-size:15}
      .content-wrapper:before {
          content: '';
          display: block;
          position: absolute;
          top: 0; bottom: 0; left: 0; right: 0;
          background: url('https://cookislands.travel/sites/default/files/2020-08/rarotonga_full_island_aerial.jpg') no-repeat center center;
          background-size: cover;
          opacity: 0.5;
          z-index: -1;
        }
		
        .image-container {
            display: flex;
            justify-content: space-around; /* Align images evenly */
            align-items: center; /* Center images vertically */
        }
        .image-container img {
            width: 200px; /* Set a fixed width */
            height: auto; /* Maintain aspect ratio */
        }

        .content-wrapper {
          position: relative;
        }
                      "))
    ),
    
    ## Set Background Photo

    
    tabItems(
      ## Homepage
      tabItem(tabName = "homepage",
              
              ## title
              
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:5px;margin-bottom:5px"),
              hr(style = "border-top: 4px solid black; width: 90%; margin-top:5px;margin-bottom:5px"),
              h2("Welcome to",
                 style = "text-align: center; font-style: italic; font-size: 35px"),
              h2("Cook Islands!",
                 style = "text-align: center; font-style: italic; font-size: 55px"),
              hr(style = "border-top: 4px solid black; width: 90%; margin-top:5px;margin-bottom:5px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:5px;margin-bottom:5px"),
              div(p("By Zecheng Li"), style = "text-align: center;", textOutput(outputId = "currentDate")),
              div(style = "height: 30px;"),
              
              ## Info box
              
              fluidRow(
                column(8, 
                       infoBox(title = "General Description", value = "Map, Key Facts, and Narrative", fill = TRUE,
                               subtitle = "Learn Basic information about Cook  Islands!",
                               color = "navy", width = NULL),
                       infoBox(title = "Key Demographics",
                               value = "Total Population, Education levels, Economic Activities, etc.", fill = TRUE,
                               subtitle = "See charts, graphs, and tables here!",
                               color = "blue", width = NULL),
                       infoBox("Regional Comparison", fill = TRUE,
                               value = "Comparison with Other Pacific Island Countries",
                               subtitle = "Check out differences in geography and economy!",
                               color = "light-blue", width = NULL),
                       infoBox("SWOT", fill = TRUE, 
                               value = "strengths, weaknesses, opportunities, and threats",
                               subtitle = "Evaluation of Cook  Islands in a competitive position!",
                               color = "teal", width = NULL)
                ),
                column(4, 
                       img(src = "person.jpg", height = "100%", width = "100%", style = "border-radius: 10px;")
                )
              )

              ),
      
      ## GD: MAP
      tabItem(tabName = "map",
              h2("Map of Cook  Islands",
                 style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              box(leafletOutput(outputId = "islandmap"), width = NULL,
                p("The Cook Islands is an island country in Polynesia, 
part of Oceania in the South Pacific Ocean. It consists of 15 islands whose total land 
area is approximately 236.7 square kilometres (91 sq mi). 
The Cook Islands' Exclusive Economic Zone (EEZ) covers 1,960,027 square kilometres (756,771 sq mi) of ocean.
Avarua is its capital."),
p("The Cook Islands comprise 15 islands split between two island groups, which have been called individual names in indigenous languages including Cook Islands Māori and Pukapukan throughout the time they have been inhabited.")
				  )
              ),
      
      ## GD: Key Facts
      tabItem(tabName = "facts",
              h2("Key facts of Cook  Islands",
                 style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(
                id = "tabset1", height = "400px", width = NULL,
				
				tabPanel("Geography",
				HTML("    <p>The Cook Islands is a group of 15 islands located in the South Pacific Ocean, northeast of New Zealand. These islands are divided into two main groups:</p>
    <ul>
        <li><strong>Southern Cook Islands:</strong> Includes Rarotonga, Aitutaki, and several smaller islands. Rarotonga is the largest and most populated island.</li>
        <li><strong>Northern Cook Islands:</strong> Consists of less populated islands like Manihiki and Penrhyn.</li>
    </ul>
    <p>The islands are characterized by their volcanic origins, lush vegetation, and coral atolls. The geography includes:</p>
    <ul>
        <li><strong>Mountains:</strong> Some islands have mountainous interiors, particularly Rarotonga.</li>
        <li><strong>Beaches:</strong> The islands are known for their beautiful white sandy beaches.</li>
        <li><strong>Lagoon:</strong> Most islands have a surrounding lagoon, rich in marine life.</li>
    </ul>
    <p>The climate is tropical, with warm temperatures year-round and a rainy season from November to April.</p>"),
	
	tags$img(src='administrative_divisions.png')
	
				),
                tabPanel("History", 
HTML("    <h4>1. Early Settlement</h4>
    <p>The first inhabitants of the Cook Islands were Polynesians, arriving around 1000 AD. They lived off agriculture and fishing, establishing tribal societies.</p>
    
    <h4>2. European Exploration</h4>
    <p>In 1769, British explorer James Cook became the first European to visit the Cook Islands. Cook's voyages sparked European interest in the South Pacific.</p>
    
    <h4>3. Colonial Period</h4>
    <p>In the 19th century, European powers began colonizing the South Pacific region. The Cook Islands became a protectorate of New Zealand in 1888.</p>
    
    <h4>4. Modern History</h4>
    <p>In 1965, the Cook Islands gained self-governance as a free association with New Zealand. In 1974, the Cook Islands was officially recognized as an independent nation, while maintaining close ties with New Zealand.</p>"),
	
	
	tags$div(class='image-container',
	tags$img(src="hist1.jpg"),
	tags$img(src="hist2.jpg")
	)
	
				),
				
				tabPanel("Religion",
				HTML("<p>The Cook Islands has a rich religious landscape, primarily influenced by Christianity. The main religions practiced include:</p>
    <ul>
        <li><strong>Christianity:</strong> The predominant religion, with the majority of the population identifying as Christian. The Cook Islands Christian Church is the largest denomination.</li>
        <li><strong>Other Denominations:</strong> Other Christian groups present include Roman Catholics, Seventh-day Adventists, and members of the Church of Jesus Christ of Latter-day Saints.</li>
        <li><strong>Indigenous Beliefs:</strong> Some traditional beliefs and practices still exist, often intertwined with Christian practices, reflecting the cultural heritage of the islands.</li>
    </ul>
    <p>Religious festivals and church activities play a significant role in community life, with Sunday being a day of worship and rest.</p>"),
	
	tags$img(src="CHURCH_IN_AVARUA,_RAROTONGA,_COOK_ISLANDS.jpg")
	
				),
				
				tabPanel("Culture",
				HTML("<p>The culture of the Cook Islands is a vibrant blend of Polynesian traditions and contemporary influences. Key aspects of the culture include:</p>
    <ul>
        <li><strong>Language:</strong> The official languages are English and Cook Islands Maori, with the latter being widely spoken in daily life.</li>
        <li><strong>Dance and Music:</strong> Traditional dance forms, such as the 'ura' and 'hula' are integral to cultural celebrations, often accompanied by traditional music featuring drums and ukuleles.</li>
        <li><strong>Arts and Crafts:</strong> The islands are known for their handicrafts, including weaving, wood carving, and tattooing, which reflect the rich cultural heritage.</li>
        <li><strong>Cuisine:</strong> Cook Islands cuisine features local ingredients, such as fish, coconuts, and taro, with dishes often prepared for communal feasting.</li>
        <li><strong>Community and Family:</strong> Strong family ties and community involvement are central to social life, with gatherings and celebrations reinforcing these bonds.</li>
    </ul>
    <p>Festivals and events, such as the annual Te Maeva Nui, celebrate the culture and heritage of the Cook Islands, showcasing traditional performances and crafts.</p>"),
	tags$img(src="330px-Rarotonga-8-Maeva-Nui.jpg")
	
              )
)			  ##end tabBox
              ),
      
      ## GD: Narrative
      tabItem(tabName = "narrative",
              box(width = NULL, div(class = "narrative-container",
                    h2("Narrative of Cook  Islands", style = "text-align: center; margin-bottom: 10px;"),
                    hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
                    HTML("
    <p>The Cook Islands, a stunning archipelago in the South Pacific, comprises 15 islands known for their breathtaking landscapes and rich cultural heritage. The southern group, including Rarotonga and Aitutaki, is characterized by volcanic mountains and beautiful lagoons, while the northern islands are primarily coral atolls. The islands are home to a warm, tropical climate, making them a popular destination for tourists seeking natural beauty and adventure.</p>
    <p>Culture thrives in the Cook Islands, where traditional Polynesian customs blend seamlessly with modern influences. The local population is predominantly Christian, with vibrant community gatherings and celebrations that showcase traditional dance, music, and crafts. The Cook Islands offer a unique experience, where visitors can immerse themselves in the warmth of the local culture while enjoying the stunning natural environment.</p>
"),

tags$div(class="image-container",
tags$img(src="fmiddle.jpg"),
tags$img(src="fleft.jpg"),
tags$img(src="fright.jpg")
)

              ))),
      
      ## Key Demographics
      tabItem(tabName = "demographics",
              h2("Key Demographics of Cook  Islands", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
   
fluidRow(  
column(4, 
prettyRadioButtons(
   inputId = "whatV",
   label = "What to Plot?", 
    choices = unique(resu$what),
   #icon = icon("user"), 
    animation = "tada",
	inline=T
)),
column(4,
              prettyRadioButtons(
   inputId = "plot1type",
   label = "Type of plot", 
    choices = c('Scatter','Bar'),
   #icon = icon("user"), 
    animation = "tada",
	inline=T
)),
column(4,
switchInput(
   inputId = "plot1Forcast",
   label = "Add Forecast?", 
   labelWidth = "100px"
))
),

plotOutput('plot1OUT')              
			  
                ),
      
      ## Regional Comparison
      tabItem(tabName = "comparison",
              h2("Regional Comparsion", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(id = "tabset3", height = "400px", width = NULL,
                     
                     ## Countries Introduction
                     tabPanel("Similar Island Countries",
                              div(class = "row",
                                  div(class = "col-sm-6",
                                      h3("Fiji"),
                                      p("Fiji and the Cook Islands are both stunning Pacific destinations, but they differ in several aspects. Fiji, known for its diverse islands and vibrant culture, offers a wide range of activities, including adventure sports and luxury resorts. In contrast, the Cook Islands boast a more laid-back atmosphere, with pristine beaches and rich Polynesian heritage. Economically, Fiji has a more diversified economy, while the Cook Islands heavily rely on tourism. Both locations provide unique experiences, but Fiji tends to attract a larger number of tourists due to its extensive infrastructure and offerings."),
                                      img(src = "https://upload.wikimedia.org/wikipedia/commons/b/ba/Flag_of_Fiji.svg", height = "150px")
                                  ),
                                  div(class = "col-sm-6",
                                      h3("Tonga"),
                                      p("Both are Pacific island nations with tropical climates and cultures deeply rooted in the Pacific Islander heritage. Economically, they rely on fisheries, tourism, and external assistance. The key distinctions lie in Tonga's status as a Polynesian kingdom with a unique monarchical system and its relatively higher elevation compared to the Cook  Islands, which has closer ties with the U.S. and is known for its U.S. military testing history. Both nations grapple with the impacts of climate change, but the Cook  Islands' low-lying atolls make it especially susceptible to sea-level rise."),
                                      img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/9/9a/Flag_of_Tonga.svg/2560px-Flag_of_Tonga.svg.png", height = "150px")
                                  )
)
                              ),
                     
                     ## Total Population
                     tabPanel("Metrics Comparison",
					 fluidRow(  
column(6, 
			  prettyRadioButtons(
   inputId = "whatV2",
   label = "What to Plot?", 
    choices = unique(resu$what),
   #icon = icon("user"), 
    animation = "tada",
	inline=T
)),
column(6,
              prettyRadioButtons(
   inputId = "plot2type",
   label = "Type of plot", 
    choices = c('Scatter','Bar'),
   #icon = icon("user"), 
    animation = "tada",
	inline=T
))
),
plotOutput('plot2OUT')
)
                     

              )
              ),
      
      ## SWOT
      tabItem(tabName = "swot",
              h2("Strengths, Weaknesses,", style = "text-align: center; font-style: italic; font-size: 40px"),
              h2("Opportunities, and Threats Analysis", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),

fluidRow(
                box(title = "Strengths", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Natural Attractions", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('With pristine tropical beaches, coral reefs, and beautiful natural landscapes, the Cook Islands are a sought-after tourist destination for travelers worldwide'))),
					fluidRow(box(title = "Stable Political Environment", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('The Cook Islands are a free association with New Zealand, featuring a stable political and legal framework that enhances investor confidence'))),
                    fluidRow(box(title = "Cultural Uniqueness", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('A rich Polynesian cultural heritage, including traditional dance, music, and art, attracts cultural tourists.'))),
                    fluidRow(box(title = "Close Ties with New Zealand", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Citizenship is interconnected with New Zealand, allowing access to trade, education, and healthcare resources supported by New Zealand.')))),
                box(title = "Opportunities", status = "success", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Develop Sustainable Tourism", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('With the increasing global demand for eco-tourism, the Cook Islands can develop environmentally friendly resorts and tourism products that protect natural resources.')
)),
                    fluidRow(box(title = "Expand Fisheries and Marine Economy", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Utilize the vast exclusive economic zone to develop the fisheries and marine resource industries, such as deep-sea mining and aquaculture.')
)),
                    fluidRow(box(title = "Attract International Investment", status = "success", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Government policies encourage foreign investment, aiming to attract more investors through infrastructure development and clean energy projects.')
)),
                    fluidRow(box(title = "Strengthen Economic Cooperation with New Zealand and Australia", status = "success", solidHeader = TRUE, collapsible = TRUE, width = 11, collapsed = TRUE,
                                 tags$p('Leverage the markets of New Zealand and Australia to increase exports of agricultural products and handicrafts.')
))),
              ),
              fluidRow(
                box(title = "Weaknesses", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Economic Monoculture", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('The economy is highly dependent on tourism, lacking industrial diversity, making it vulnerable to fluctuations in the global tourism market.'))),
                    fluidRow(box(title = "Geographical Isolation", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Located in the South Pacific, transportation is inconvenient, resulting in high logistics and supply chain costs.'))),
                    fluidRow(box(title = "Small and Aging Population", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('The limited workforce and outflow of young people restrict economic growth potential.'))),
                    fluidRow(box(title = "Scarcity of Natural Resources", status = "warning", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Limited exploitable resources, small-scale agriculture and manufacturing, and a strong dependence on imports.')))),
                box(title = "Threats", status = "danger", solidHeader = TRUE, collapsible = TRUE,
                    fluidRow(box(title = "Climate Change and Natural Disasters", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Facing climate threats such as rising sea levels and typhoons, which could have severe impacts on tourism and living conditions.')
)),
                    fluidRow(box(title = "Global Economic Uncertainty", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('A slowdown in the international economy or pandemics could lead to a decrease in tourists, significantly affecting the economy.')
)),
                    fluidRow(box(title = "Competition with Other Pacific Tourist Destinations", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('Countries like Fiji and Vanuatu are also developing their tourism industries, increasing competitive pressure.')
)),
                    fluidRow(box(title = "Overtourism and Environmental Degradation", status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 11,
                                 tags$p('An increase in the number of tourists may lead to environmental damage, affecting ecology and long-term sustainability.')
))),
              ),


      ),
      
      ## Reference and Special Thanks
      tabItem(tabName = "thanks",
              h2("Reference & Special Thanks", style = "text-align: center; font-style: italic; font-size: 40px"),
              hr(style = "border-top: 2px solid black; width: 90%;margin-top:10px;margin-bottom:20px"),
              tabBox(id = "tabset4", width = NULL, height = "250px",
                     tabPanel("Data Sources",
                              a("Wikipedia of Cook  Islands", href = "https://en.wikipedia.org/wiki/Cook_Islands")),
                     tabPanel("Technical References",
                              a("Mastering Shiny", href = "https://mastering-shiny.org/index.html"),
                              p(""),
                              a("leaflet for R", href = "https://rstudio.github.io/leaflet/"),
                              p(""),
                              a("Shiny Dashboard", href = "https://rstudio.github.io/shinydashboard/structure.html#tabbox"),
                              p(""),
                              a("ChatGPT", href = "https://chat.openai.com")),
                     tabPanel("Literature References",
                              a("Wikipedia of Cook Islands", href = "https://en.wikipedia.org/wiki/Cook_Islands"),
                              p(""),
                              a("Wikipedia of Figi", href = "https://en.wikipedia.org/wiki/Fiji"),
                              p(""),
                              a("Wikipedia of Tonga", href = "https://en.wikipedia.org/wiki/Tonga"),
                              p(""),
                              a("ChatGPT", href = "https://chat.openai.com")),
                     tabPanel("Speical Thanks",
                              h4("Haviland Wright, Ph.D.", style = "text-align: center"),
))
              )
             )
    
    
  )
)

server <- function(input, output, session){
  
  ## Homepage Date
  output$currentDate <- renderText({
    format(Sys.Date(), "%Y-%m-%d, %A")
  })
  
  ## Leaflet Map
  output$islandmap <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldStreetMap)|>
      addMarkers(lng = -159.785278, lat = -18.857952, popup=HTML("Cook Islands<br/>168.7345 E, 7.23 N")) |>
      setView(lng = -159.785278, lat = -18.857952, zoom = 7) |>
      addCircles(lng = -159.785278, lat = -18.857952, weight = 1, 
                 radius = 50000,
                 color = "black", fillColor = "lightgreen", 
                 fillOpacity = 0.2, popup=HTML("Cook Islands<br/>159.7777° W, 21.2367° S"))
  })

  
###
output$plot1OUT<-renderPlot({
inter<-resu[what==input$whatV & country=='Cook']
inter[,type:='Historical']

if(input$plot1Forcast==TRUE){
interadd<-preds[what==input$whatV & country=='Cook']
interadd[,type:='Forecast']
interm<-inter[.N]
interm[,type:='Forecast']
inter<-rbind(inter,interm,interadd[,.(year,value,what,country,type)])
inter[,year:=as.integer(year)]
}


if(input$plot1type=='Scatter')
{
ggplot(inter,aes(x=year,y=value,colour=type))+
   geom_point()+
   geom_line()+
   theme_bw()+
   scale_x_continuous(breaks=inter$year)+
   scale_colour_manual(values=c(Historical='#01F9C6',Forecast='#FA0032'))+
   labs(x='Year',y=input$whatV,title=paste('Time chart of ',input$whatV,'of Cook Islands'))
} else {
copy(inter)->inter22
inter22<-inter22[!duplicated(year)]
ggplot(inter22,aes(x=year,y=value,fill=type))+
   geom_bar(stat='identity',width=0.5)+
   theme_bw()+
   scale_x_continuous(breaks=inter$year)+
   scale_fill_manual(values=c(Historical='#01F9C6',Forecast='#FA0032'))+
   labs(x='Year',y=input$whatV,title=paste('Time chart of ',input$whatV,'of Cook Islands'))
}
  
   
})


###
###
output$plot2OUT<-renderPlot({
inter2<-resu[what==input$whatV2]
zu<-resu[,CJ(unique(year),unique(country))]
names(zu)<-c('year','country')
inter2[zu,on=c('year','country')]->inter2


if(input$plot2type=='Scatter')
{
ggplot(inter2,aes(x=year,y=value,group=country,colour=country))+
   geom_point()+
   geom_line()+
   theme_bw()+
   scale_x_continuous(breaks=unique(inter2$year))+
   labs(x='Year',y=input$whatV,colour='Islands')+
   scale_y_continuous(trans='log10')
} else {
ggplot(inter2,aes(x=year,y=value,fill=country))+
   geom_bar(stat='identity',position='dodge')+
   theme_bw()+
   scale_x_continuous(breaks=unique(inter2$year))+
   labs(x='Year',y=input$whatV,fill='Islands')+
   scale_y_continuous(trans='log10')
}
  
   
})
}

shinyApp(ui, server)