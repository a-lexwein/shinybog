# library(V8)
# library(shiny)
# library(dplyr)
# library(ggplot2)
# library(purrr)
# library(rvest)
# library(dplyr)
# library(purrr)
# library(tidyr)
# library(triebeard)
# library(shinyjs)
# library(stringr)

require(V8)
require(shiny)
require(dplyr)
require(ggplot2)
require(purrr)
require(rvest)
require(dplyr)
require(purrr)
require(tidyr)
require(triebeard)
require(shinyjs)
require(stringr)


### Load English Word List into a prefix tree for fast lookup.
words <- read.table('twl06.txt', stringsAsFactors = FALSE)$V1 %>% paste0('$')
word_trie <- trie(keys = words, values = words)
isEnglish <- function(str) {
  str <- toupper(str) %>% paste0('$')
  !is.na(prefix_match(word_trie, str)[[1]])
}


## JQuery code to handle word list.
# addWordToDive adds a span of class = score containing text = word to #found_word
jsCode <- '
shinyjs.addWordToDiv = function(params) {
var defaultParams = {
word : "",
score : null
};
params = shinyjs.getParams(params, defaultParams);
var $el = $( "<span></span>" ).addClass( params.score ).text( params.word + " ");
$( "#found_words" ).append( $el )
}

shinyjs.clearDiv = function() {
$( "#found_words" ).empty();
}
'


### Pull Letter Frequencies from html table Wikipedia.
url <- "http://en.wikipedia.org/wiki/Letter_frequency"

scrape <- read_html(url) %>%
  html_nodes('table') %>%
  html_table()

# turned the scraped table into a data frame.  mutate_each callback turns "%45" to 0.45
freq <- scrape[4] %>%
  data.frame() %>%
  mutate_each(funs(as.numeric(gsub("%.*", "", .))/100), vars = -Letter)

names(freq) <- names(freq) %>% gsub("\\..*",'' , .)

## if a new letter gets added to the wiki page, things could get screwy
## this collapses similar characters into english alphabet. There might bad assumptions for certain languages.
freq <- freq %>%
  mutate(LetterReal = Letter, rownum = row_number()) %>%
  mutate(Letter = case_when(
    .$rownum <= 26 ~ .$LetterReal,
    .$rownum %in% seq(27,35) ~ 'a',
    .$rownum %in% seq(36,39) ~ 'c',
    .$rownum %in% seq(40,41) ~ 'd',
    .$rownum %in% seq(42,47) ~ 'e',
    .$rownum %in% seq(48,59) ~ 'g',
    .$rownum %in% seq(50,50) ~ 'h',
    .$rownum %in% seq(51,55) ~ 'i',
    .$rownum %in% seq(56,57) ~ 'j',
    .$rownum %in% seq(58,60) ~ 'n',
    .$rownum %in% seq(61,65) ~ 'o',
    .$rownum %in% seq(66,66) ~ 'r',
    .$rownum %in% seq(67,71) ~ 's',
    .$rownum %in% seq(72,73) ~ 't',
    .$rownum %in% seq(74,79) ~ 'u',
    .$rownum %in% seq(80,80) ~ 'y',
    .$rownum %in% seq(81,83) ~ 'z')
  )

freqout <- freq %>%
  select(-LetterReal, rownum) %>%
  group_by(Letter) %>%
  summarise_all(sum) %>%
  slice(1:26) %>%
  gather() %>%
  mutate(value = ifelse(!is.na(value), value, 0))


## generate an m*n board
get_grid <- function(n,m) {
  ## test for n,m between 1 and 10
  grid <- data.frame(data.frame(x= integer(0), y= integer(0), stringsAsFactors = FALSE))
  for(i in seq(1,n)) {
    for(j in seq(1,m)) {
      grid <- rbind(grid, c(i,j))
    }
  }
  names(grid) <- c("x", "y")
  grid
}


### helper functions building up to is_on_board
default_node <- list(x = -1, y = -1)
node <- default_node

neighbors <- function(df, node) {
  a <- node$x
  b <- node$y
  df %>%
    filter( !(x == a & y == b),
            x >= a - 1,
            x <= a + 1,
            y >= b - 1,
            y <= b + 1)
}

remove_node_from_board <- function(board, node) {
  a <- node$x
  b <- node$y
  filter(board, !(board$x == a & board$y == b))
}

links_on_board <- function(board, node, word) {
  a <- node$x
  b <- node$y
  head <- substring(word, 1, 1)
  tail <- substring(word, 2)
  
  if (node$x == default_node$x) {
    return(filter(board, board$letter == head))
  }
  
  neighbors(board, node) %>%
    filter(letter == head)
}

# W Chang: https://rpubs.com/wch/200398
f_pmap_aslist <- function(df) {
  pmap(as.list(df), list)
}

is_on_board_inside <- function(board, node, word) {
  a <- node$x
  b <- node$y
  head <- substring(word, 1, 1)
  tail <- substring(word, 2)
  
  links <- links_on_board(board, node, head)
  if (nrow(links) == 0) {
    return(FALSE)
  }
  
  if (nchar(tail) == 0) {
    return(TRUE)
  }
  f_pmap_aslist(links) %>%
    map(function(row) {
      new_board <- remove_node_from_board(board, row)
      is_on_board_inside(new_board, row, tail)
    })
}

# probably a way to get recursive function to output the right type, but this reducer does the trick.
is_on_board <- function(board, node, word) {
  sum(unlist(is_on_board_inside(board, node, word))) > 0
}

# scoring function, currently just (word, board) -> string, but can be fleshed out
score <- function(word, board) {
  if(!is_on_board(board, default_node, word)) {
    return('off');
  }
  if_else(isEnglish(word), 'good', 'bad')
}

lets <- LETTERS
# TODO need to handle "Qu" eventually
# lets[17] <- "Qu"


## Text size:
text_sizer <- function(i,j) {
  if (i <= 6) {
    row_size <- 36
  } else if (i <= 10) {
    row_size <- 24
  } else if (i <= 12) {
    row_size <- 18
  } else if (i <= 13) {
    row_size <- 16
  } else if (i <= 14) {
    row_size <- 14
  } else if (i <= 15) {
    row_size <- 12
  } else if (i <= 20) {
    row_size <- 10
  } else if (i <= 25) {
    row_size <- 18
  } else if (i <= 30) {
    row_size <- 5
  } else {
    row_size <- 3
  }
  
  if (j <= 3) {
    col_size <- 36
  } else if (j <= 5) {
    col_size <- 24
  } else if (j <= 6) {
    col_size <- 20
  } else if (j <= 7) {
    col_size <- 17
  } else if (j <= 8) {
    col_size <- 15
  } else if (j <= 9) {
    col_size <- 14
  } else if (j <= 10) {
    col_size <- 12
  } else if (j <= 11) {
    col_size <- 10
  } else if (j <= 12) {
    col_size <- 9
  } else if (j <= 14) {
    col_size <- 8
  } else if (j <= 15) {
    col_size <- 7
  } else if (j <= 17) {
    col_size <- 6
  } else if (j <= 20) {
    col_size <- 5
  } else if (j <= 25) {
    col_size <- 4
  } else {
    col_size <- 3
  }
  min(col_size, row_size)
}


# front-end code
ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  
  # CSS 
  tags$head(
    tags$style(HTML("
                    .good {
                    color: green;
                    }
                    .bad {
                    color: red;
                    }
                    .off {
                    color: blue;
                    }
                    "))
    ),
  # Application title
  titlePanel("Shiny Bog"),
  
  # Options for board
  sidebarLayout(
    sidebarPanel(numericInput("j", label = h3("Rows:"), value = 5),
                 numericInput("i", label = h3("Columns"), value = 5),
                 selectInput("select", label = h3("Letter Distribution"),
                             choices = c('English', 'French', 'German', 'Spanish', 'Portuguese', 'Esperanto', 'Italian',
                                         'Turkish', 'Swedish', 'Polish', 'Dutch', 'Danish', 'Icelandic', 'Finnish'),
                             selected = 'English'),
                 actionButton("buttonNewBoard", "New Board"),
                 
                 p("by Alex Wein, code available here")           
    ),
    # Show a plot of the generated distribution
    mainPanel(
      #game board
      plotOutput("gg"),
      #word input
      textInput("wordInput", "Enter Words"),
      #submit button
      actionButton("buttonSubmitWord", "Click Here or Press Enter"),
      #list of submitted words
      tags$div(id="found_words"),
      #jquery to enable enter for action button
      tags$script(HTML("$('#wordInput').keypress(function(e) {
                       if (e.which === 13) {
                       $('#buttonSubmitWord').click();
                       }
                       });
                       $('#buttonSubmitWord').click(() => setTimeout(() => $('#wordInput').val('')));"))
      )
      )
      )


server <- function(input, output) {
  board <- data.frame()
  i <- 5
  j <- 5
  language <- 'English'
  letsize <- 24
  
  # Whenever a new word is submitted, score the word and add it to the word list div.
  observeEvent(input$buttonSubmitWord, {
    word <- input$wordInput %>% str_trim %>% str_to_upper
    js$addWordToDiv(word, score(word, board))
    
  })
  # New Board button is pressed: reset word list and update reactive input
  observeEvent(input$buttonNewBoard, {
    js$clearDiv()
  })
  
  # generating the chart from input
  output$gg <- renderPlot({
    isolate({
      i <- input$i
      j <- input$j
      lang <- input$select
    })
    input$buttonNewBoard
    
    freq2 <- freqout %>% filter(key == lang)
    
    letter_freq <- data.frame(letter = lets, weight = as.numeric(freq2$value))
    
    # puts words onto the grid
    get_board <- function(grid) {
      grid_size <- nrow(grid)
      lets <- sample_n(letter_freq, grid_size, replace = TRUE, weight = weight)
      grid %>% bind_cols(lets)
    }
    
    
    board <<- get_grid(i,j) %>%
      get_board
    board %>%
      ggplot(aes(x = x, y = y, label = letter)) +
      
      # background diagonals and grid.
      geom_abline(slope = 1, intercept = seq(-20,20)) + #
      geom_abline(slope = -1, intercept = seq(-20,20)) +
      geom_hline(yintercept = seq(1,j)) +
      geom_vline(xintercept = seq(1,i)) +
      
      geom_point(size = text_sizer(i,j) + 2.5, color = '#ffffcc', shape = 15) +
      geom_text(size = text_sizer(i,j), color = '#0c2c84') +
      coord_fixed(xlim = c(.5,i+.5), ylim = c(.5,j+.5)) +
      theme_minimal()
    
  })
  
}

shinyApp(ui = ui, server = server)