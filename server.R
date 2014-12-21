set.seed(26)
require("caret")
load("rf2.rda")
library(shiny)
library(stringr)
library(data.table)
library(plyr)


shinyServer( 
    function(input, output){
        filterOut <- c("a"      , "about", "after", "all"    , "also" , "an"    , "and"      
                      ,"another", "any"  , "are"  , "do"     , "does" , "each"  , "between" 
                      ,"as"     , "at"   , "be"   , "because", "been" , "before", "being"     
                      ,"both"   , "have" , "he"   , "his"    , "her"  , "hers"  , "just"    
                      ,"but"    , "by"   , "came" , "can"    , "come" , "could" , "did"      
                      ,"else"   , "for"  , "from" , "get"    , "got"  , "had"   , "has"      
                      ,"here"   , "how"  , "we"   , "ll"     , "my"   , "s"     , "m"       
                      ,"n"      , "t"    , "d"    , "like"   , "me"   , "make"  , "many"    
                      ,"i"      , "if"   , "in"   , "into"   , "is"   , "it"    , "its"        
                      ,"might"  , "of"   , "on"   , "only"   , "or"   , "other" , "our"      
                      ,"out"    , "over" , "re"   , "same"   , "than" , "that"  , "the"      
                      ,"should" , "since", "so"   , "some"   , "still", "such"  , "take"     
                      ,"then"   , "there", "these", "they"   , "them" , "this"  , "those"    
                      ,"through", "to"   , "too"  , "up"     , "use"  , "will"  , "wont"    
                      ,"way"    , "well" , "went" , "were"   , "what" , "when"  , "where"    
                      ,"which"  , "while", "go"   , "gonna"  , "good" , "hey"   , "him"     
                      ,"baby"   , "back" , "cause", "don"    , "down" , "ever"  , "every"    
                      ,"know"   , "said" , "say"  , "she"    , "thing", "ve"    , "was"     
                      ,"let"    , "love" , "no"   , "not"    , "now"  , "oh"    , "one"      
                      ,"yeah"   , "yes"  , "with" , "would"  , "you"  , "your"  , "never"
                      ,"again"  , "ain"  , "am"   , "around" , "away" , "day"   , "everybody"
                      ,"feel"   , "keep" , "life" , "little" , "long" , "look"  , "man"
                      ,"night"  , "ooh"  , "right", "see"    , "time" , "want"  , "who"
                      ,"woman")

        prepIp <- function(userIp,filterOut){
            op <- data.frame(lyricsList=' ',stringsAsFactors=FALSE)
            pat <- try(paste0("\\b(", paste0(filterOut, collapse="|"), ")\\b"))    
            goodWords <- try(gsub(pat, "", tolower(userIp)))
            noNewLines <- gsub("\n"," ",goodWords) 
            spaceFunc <- function(x) return(gsub("^ *|(?<= ) | *$", "", noNewLines, perl=T)) 
            noDoubleSpaces <- spaceFunc(noNewLines)
            noPunct <- gsub("[[:punct:]]", "", noDoubleSpaces)
            final <- gsub("  "," ",noPunct) 
            wordsSplit <- unlist(strsplit(final, split=" "))
            myFreq<-table(wordsSplit)
            op$lyricsList <- as.character(substring(paste("", names(myFreq), sep=",", collapse=""),2))
            return(populateRow(op)) 
        }
        
        populateRow <- function(ipList) {
            # the word list below was identified by running frequencies of ~900 songs across 9 music genres and 
            # taking some of the most prevalent ones.  Apologies that some words may be offensive.  
            op <- data.frame(cry= integer(0), dark= integer(0), death= integer(0), die= integer(0),
                  	fear= integer(0), fight= integer(0), fighting= integer(0), hell= integer(0),
                  	power= integer(0), rock= integer(0), roll= integer(0), black= integer(0),
                  	care= integer(0), god= integer(0), soul= integer(0), tonight= integer(0),
                  	bad= integer(0), believe= integer(0), blues= integer(0), chain= integer(0),
                  	gone= integer(0), lord= integer(0), lovin= integer(0), mess= integer(0),
                  	shame= integer(0), working= integer(0), arms= integer(0), heart= integer(0),
                  	loved= integer(0), loving= integer(0), mind= integer(0), old= integer(0),
                  	rain= integer(0), world= integer(0), body= integer(0), boogie= integer(0),
                  	dance= integer(0), dancing= integer(0), groove= integer(0), move= integer(0),
                  	music= integer(0), shake= integer(0), free= integer(0), fuck= integer(0),
                  	future= integer(0), kill= integer(0), money= integer(0), punk= integer(0),
                  	rise= integer(0), smash= integer(0), beat= integer(0), bitch= integer(0),
                  	gangsta= integer(0), goin= integer(0), gotta= integer(0), nigga= integer(0),
                  	rhyme= integer(0), shit= integer(0), stand= integer(0), yo= integer(0),
                  	angel= integer(0), boom= integer(0), darling= integer(0), dreadlocks= integer(0),
                  	jammin= integer(0), poor= integer(0), rights= integer(0), alone= integer(0),
                  	fool= integer(0), please= integer(0), ride= integer(0), touch= integer(0),
                  	songSize = integer(0),stringsAsFactors=FALSE)
            op[1,]<-as.factor(0)
            words <- strsplit(ipList$lyricsList,",")
   	        words <- words[[1]]
	        op$songSize <- length(words)
            for(i in 1:length(words)){
         	    if (words[i] == 'cry')            op$cry <- as.factor(1)
         	    else if (words[i] == 'dark')      op$dark <- as.factor(1)
         	    else if (words[i] == 'death')     op$death <- as.factor(1)
         	    else if (words[i] == 'die')       op$die <- as.factor(1)
         	    else if (words[i] == 'fear')      op$fear <- as.factor(1)
         	    else if (words[i] == 'fight')     op$fight <- as.factor(1)
         	    else if (words[i] == 'fighting')  op$fighting <- as.factor(1)
         	    else if (words[i] == 'hell')      op$hell <- as.factor(1)
         	    else if (words[i] == 'power')     op$power <- as.factor(1)
         	    else if (words[i] == 'rock')      op$rock <- as.factor(1)
         	    else if (words[i] == 'roll')      op$roll <- as.factor(1)
         	    else if (words[i] == 'black')     op$black <- as.factor(1)
         	    else if (words[i] == 'care')      op$care <- as.factor(1)
         	    else if (words[i] == 'god')       op$god <- as.factor(1)
         	    else if (words[i] == 'soul')      op$soul <- as.factor(1)
         	    else if (words[i] == 'tonight')   op$tonight <- as.factor(1)
         	    else if (words[i] == 'bad')       op$bad <- as.factor(1)
         	    else if (words[i] == 'believe')   op$believe <- as.factor(1)
         	    else if (words[i] == 'blues')     op$blues <- as.factor(1)
         	    else if (words[i] == 'chain')     op$chain <- as.factor(1)
         	    else if (words[i] == 'gone')      op$gone <- as.factor(1)
         	    else if (words[i] == 'lord')      op$lord <- as.factor(1)
         	    else if (words[i] == 'lovin')     op$lovin <- as.factor(1)
         	    else if (words[i] == 'mess')      op$mess <- as.factor(1)
         	    else if (words[i] == 'shame')     op$shame <- as.factor(1)
         	    else if (words[i] == 'working')   op$working <- as.factor(1)
         	    else if (words[i] == 'arms')      op$arms <- as.factor(1)
         	    else if (words[i] == 'heart')     op$heart <- as.factor(1)
         	    else if (words[i] == 'loved')     op$loved <- as.factor(1)
         	    else if (words[i] == 'loving')    op$loving <- as.factor(1)
         	    else if (words[i] == 'mind')      op$mind <- as.factor(1)
         	    else if (words[i] == 'old')       op$old <- as.factor(1)
         	    else if (words[i] == 'rain')      op$rain <- as.factor(1)
         	    else if (words[i] == 'world')     op$world <- as.factor(1)
         	    else if (words[i] == 'body')      op$body <- as.factor(1)
         	    else if (words[i] == 'boogie')    op$boogie <- as.factor(1)
         	    else if (words[i] == 'dance')     op$dance <- as.factor(1)
         	    else if (words[i] == 'dancing')   op$dancing <- as.factor(1)
         	    else if (words[i] == 'groove')    op$groove <- as.factor(1)
         	    else if (words[i] == 'move')      op$move <- as.factor(1)
         	    else if (words[i] == 'music')     op$music <- as.factor(1)
         	    else if (words[i] == 'shake')     op$shake <- as.factor(1)
         	    else if (words[i] == 'free')      op$free <- as.factor(1)
         	    else if (words[i] == 'fuck')      op$fuck <- as.factor(1)
         	    else if (words[i] == 'future')    op$future <- as.factor(1)
         	    else if (words[i] == 'kill')       op$kill <- as.factor(1)
         	    else if (words[i] == 'money')      op$money <- as.factor(1)
         	    else if (words[i] == 'punk')       op$punk <- as.factor(1)
         	    else if (words[i] == 'rise')       op$rise <- as.factor(1)
         	    else if (words[i] == 'smash')      op$smash <- as.factor(1)
         	    else if (words[i] == 'beat')       op$beat <- as.factor(1)
         	    else if (words[i] == 'bitch')      op$bitch <- as.factor(1)
         	    else if (words[i] == 'gangsta')    op$gangsta <- as.factor(1)
         	    else if (words[i] == 'goin')       op$goin <- as.factor(1)
         	    else if (words[i] == 'gotta')      op$gotta <- as.factor(1)
         	    else if (words[i] == 'nigga')      op$nigga <- as.factor(1)
         	    else if (words[i] == 'rhyme')      op$rhyme <- as.factor(1)
         	    else if (words[i] == 'shit')       op$shit <- as.factor(1)
         	    else if (words[i] == 'stand')      op$stand <- as.factor(1)
         	    else if (words[i] == 'yo')         op$yo <- as.factor(1)
         	    else if (words[i] == 'angel')      op$angel <- as.factor(1)
         	    else if (words[i] == 'boom')       op$boom <- as.factor(1)
         	    else if (words[i] == 'darling')    op$darling <- as.factor(1)
         	    else if (words[i] == 'dreadlocks') op$dreadlocks <- as.factor(1)
          	    else if (words[i] == 'jammin')     op$jammin <- as.factor(1)
          	    else if (words[i] == 'poor')       op$poor <- as.factor(1)
         	    else if (words[i] == 'rights')     op$rights <- as.factor(1)
         	    else if (words[i] == 'alone')      op$alone <- as.factor(1)
         	    else if (words[i] == 'fool')       op$fool <- as.factor(1)
         	    else if (words[i] == 'please')     op$please <- as.factor(1)
         	    else if (words[i] == 'ride')       op$ride <- as.factor(1)
         	    else if (words[i] == 'touch')      op$touch <- as.factor(1)
    	    }
	        op[,1:72] <- as.factor(as.character(op[,1:72]))
	        return(op)
        }
        output$lyrics <- reactive({
                             if(input$goButton > 0) {
                                  isolate(input$lyrics)
                             }
                          })
        output$genre <- reactive({
           if(input$goButton > 0) {
               x <- prepIp(input$lyrics,filterOut)
               finalPredict <- rf2$predict
           }
        })
    } 
)
