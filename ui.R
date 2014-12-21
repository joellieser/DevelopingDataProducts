shinyUI(pageWithSidebar( 
    headerPanel(img(src = "ddp.PNG", height = 300, width = 300,align="right")), 
    sidebarPanel(
        br(),
        br(),
        h5("Instructions: Paste the lyrics of a song into the text box below and hit submit. "),
        textInput(inputId="lyrics", label = ""), 
        actionButton("goButton", "Tell Me The Genre")
    ), 
    mainPanel(
        h4("The purpose of this site is to try to identify the genre of a song based on the frequency of certain lyrics."),
        h4("The possible genres are: Hip Hop, Heavy Metal, Folk, Alternative, Punk Rock, Rock, Blues, Dance, Reggae and Country."),
        h5("Lyrics can be found at any number of sites, such as:"),
        a("www.chartlyrics.com",href="http://www.chartlyrics.com/", target="_blank"),
        br(),
        a("www.songlyrics.com",href="http://www.songlyrics.com/", target="_blank"),
        h5("Please select only English lyrics."),
        br(),
        br(),
        br(),
        p('You entered:'), 
        verbatimTextOutput('lyrics'),
        br(),
        h4("The predicted music genre, based on the lyrics above is:"),
        textOutput('genre') 
    ) 
))

