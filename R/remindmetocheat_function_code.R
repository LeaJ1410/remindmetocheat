## Assignment 3.2R Solutions - 2022 Programming in Psychological Science
#
# Record of Revisions
#
# Date            Programmer    
# ====         ================           
# 28-Jan-22       Lea Jordan
#                  13660258

#remind_me() 
remind_me <- function() {
  course <- c(rep("Programming", 3), "Stats 1", "Multi-Level")
  evaluation <- c("Assignment 3", "Assignment 4", "Exam", "Exam", "Exam")
  dates <- c("2022-28-01", "2022-02-02", "2022-04-02", "2022-03-03", "2022-01-07")
  deadlines_df <- data.frame(course, evaluation, dates)
  return(deadlines_df)
}
remind_me()

#cheat()
cheat <- function(question){
  q1 <- function() {
    boxplot(rnorm(100, 10, 2))
  }
  q2 <- function() {
    library(readr)
    schiphol_temp <- read_csv("https://raw.githubusercontent.com/hannesrosenbusch/schiphol_class/master/schiphol_data.csv")
    schiphol_temp
    colnames(schiphol_temp)
    plot(x = schiphol_temp$DATE, y = schiphol_temp$TAVG) 
  }
  q3 <- function() {
    library("ggplot2")
    library("titanic") ##install.packages("titanic")
    data("titanic_train")
    table(titanic_train$Survived, titanic_train$Sex)
    ggplot(titanic_train, aes(Sex)) + 
      geom_bar(aes(fill = factor(Survived, labels = c("dead","alive"))), na.rm = FALSE, width = 0.5)+
      labs(fill ="How did it go?")+
      ylab("count")+
      xlab("Sex")+
      labs(fill="Suvived")
  }
  q4 <- function() {
    ggplot(itanic_train, aes(Sex)) + 
      geom_bar(aes(fill = factor(Survived, labels = c("dead","alive"))), 
               na.rm = FALSE, width = 0.5)+
      theme_minimal()+
      ylab("count")+
      xlab("Sex")+
      labs(fill="Suvived")
  }
  q5 <- function() {
    x11()
    plot(mtcars$cyl, mtcars$hp)
    mtcars
    x11()
    ggplot(mtcars, aes(cyl, hp)) +
      geom_point(size = 3, shape = 19)+ 
      xlab("Cylinder")+                 
      ylab("Horsepower")+
      theme_minimal()  
  }
  q6 <- function() {
    orange <- Orange
    orange
    library("dplyr")
    orange_max_circumference <- orange %>% 
      group_by(Tree) %>% 
      top_n(1, circumference) %>% 
      select(Tree, circumference) %>% 
      unique 
    library("ggplot2")
    barplot1 <- ggplot(orange_max_circumference, aes(factor(Tree, levels = c("1","2","3","4","5")),circumference)) + 
      geom_bar(stat="identity")+
      ylab("max_circumference")+
      xlab("Tree")
    x11()
    barplot1
  }
  q7 <- function() {
    x11()
    ggplot(Orange, aes(age,circumference))+
      geom_smooth(method = "lm", se = TRUE)
  }
  q8 <- function() {
    library(ggplot2)
    library(patchwork)
    # Create second plot:
    linesplot1 <- ggplot(Orange, aes(x = age, 
                                     y = circumference, 
                                     color = factor(Tree,levels = c("1", "2", "3", "4", "5")))) + 
                  geom_line()+ 
                  scale_color_discrete(name ="Tree")
    # Put them together:
    x11()
    barplot1 + linesplot1
  }
  q9 <- function() {
    library(ggstatsplot)
    ToothGrowth
    x11()
    ggbetweenstats(data = ToothGrowth, x = supp, y = len, 
                   xlab = "Supplement",
                   ylab = "Teeth length", 
                   title = "Comparison of Teeth Growth across Supplements",
                   outlier.tagging = TRUE)
  }
  q10 <- function() {
    library(plotly)
    library(tidyverse)
    iris2 <- iris %>% arrange(desc(Petal.Length))
    fig1 <- plot_ly(iris2,x = ~Petal.Length,y = ~Petal.Width,z = ~Sepal.Width,color = ~Species)
    fig1
  }
  q11 <- function() {
    library(gganimate)
    library(coronavirus)
    library(plotly)
    library(tidyverse)
    library(dplyr)
    # Importing data 
    covid19_df<- refresh_coronavirus_jhu()
    # creating subset of data for NL
    covidNL <- covid19_df %>%
      filter(location == "Netherlands") %>%
      mutate(value = ifelse(data_type == "cases_new", value/10, value))
    # Creating the plot
    covid19NL_fig <- ggplot(covidNL, aes(x = date, y = value, color = data_type))+
      geom_line()+
      theme_minimal()+
      scale_color_discrete(rm(),labels = c("cases/10", "deaths"))+
      labs(title = "Covid-19 in NL")+
      transition_reveal(date)     # Add animation
    # Animation
    library(gifski) 
    # I needed to add this as I always got the warning message: 
    # file_renderer failed to copy frames to the destination directory
    animate(covid19NL_fig, renderer = gifski_renderer())
  }
  q12 <- function() {
    library(quantmod)
    getSymbols("QQQ", auto.assign = TRUE)
    #[1] "QQQ"
    str(QQQ) # The structure of the object getSymbols created
    head(QQQ) # The first few rows of QQQ
    # We can use:
    plot(QQQ$QQQ.Open, main = "QQQ.Open", ylab = "Stock price")
    # or
    plot(QQQ[,1], main = "QQQ.Open", ylab = "Stock price" )
    # The plots look so fancy because the data is an xts object
  }
  q13 <- function() {
    library(ggplot2)
    plot2021 <- function(s){
      xt <- getSymbols(s, from = "2021-01-01" , to = "2021-12-31", auto.assign = FALSE) 
      plot(xt[,1],ylab = "Stock price", main = s)
    }
    plot2021("QQQ")
  }
  q14 <- function() {
    # For Q3.10 this should look like this: 
    library(plotly)
    library(tidyverse)
    iris2 <- iris %>% arrange(desc(Petal.Length)) #Pipe can stay in one line as it is short
    fig1 <- plot_ly(iris2, 
                    x = ~Petal.Length, # Each argument in a new line 
                    y = ~Petal.Width,  # as code is too long otherwise
                    z = ~Sepal.Width,  # and spaces should be added after commas.
                    color = ~Species) 
    fig1 
    
    # For Q.3.13 a better style should look like this:
    plot2021 <- function(s) { # Adding a space before curly brackets
      xt <- getSymbols(s, from = "2021-01-01", # Each argument in new line
                       to = "2021-12-31",      # No space before comma
                       auto.assign = FALSE) # Put this in the next line so it does 
      plot(xt[, 1], ylab = "Stock price", main = s) # Add a space after the commas
    }
    plot2021("QQQ")
  }
  q15 <- function() {
    w = "apple"         # 1: You should assign variables with <- not =
    v = function(x){
      y <- strsplit(x, " ", ) # 2: There should be no comma after " "
      v <- 0
      for(z in unlist(y)){if(z == w){v = v +1}} 
      if(v > 0) return(T) 
    }
    v("i bought two bananas and an apple")
    # All the function does is to return TRUE when using apple as an argument
    # 3: The code flow is bad as the code blocks are not structured well 
    # { should be the last character on line
    # } should be the first character on line
    # Thus, this code makes it difficult to see the hierarchy of code
    # 4: The name of the function is not informative and can be easily confused 
  }
  q16 <- function() {
    #New code
    my_matrix <- matrix(1:9, 3, , TRUE)*matrix(1:3, 3, 3)
    # or
    my_matrix <- matrix(c(1:3, 4:6*2, 7:9*3), 3, , TRUE)
  }
  q17 <- function() {
    recursive_function2 <- function(x) {
      y <- 1
      if (x == 0) {
        print(1)
      } else {
        for(i in 1:x) {
          y <- y*((1:x)[i])
        }
        print(y) 
      }
    }
    recursive_function2(10)
  }
  q18 <- function() {
    # devtools::install_github("sctyner/memer")
    # Yes you can post it on Slack
    library(memer)
    meme_list()
    meme_get("AnakinPadmeRight") %>%
    meme_text_anakin("Then I assigned a new variable.", "With <- right?")
  }
  if (question == 1) {
    print(q1)
  } else if (question == 2) {
    print(q2)
  } else if (question == 3) {
    print(q3)
  } else if (question == 4) {
    print(q4)
  } else if (question == 5) {
    print(q5)
  } else if (question == 6) {
    print(q6)
  } else if (question == 7) {
    print(q7)
  } else if (question == 8) {
    print(q8)
  } else if (question == 9) {
    print(q9)
  } else if (question == 10) {
    print(q10)
  } else if (question == 11) {
    print(body(q11))
  } else if (question == 12) {
    print(q12)
  } else if (question == 13) {
    print(q13)
  } else if (question == 14) {
    print(q14)
  } else if (question == 15) {
    print(q15)
  } else if (question == 16) {
    print(q16)
  } else if (question == 17) {
    print(q17)
  } else if (question == 18) {
    print(q18)
  } else {
    print("Sorry, argument needs to be a number from 1 to 18")
  }
}
cheat(12)


