# Assuming we are already in "CapStone" directory

# Quiz 1

question1_2 <- function() {
    # Question 1: The en_US.blogs.txt  file is how many megabytes?
    blog_info <- file.info("Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
    print(blog_info)

    # Question 2: The en_US.twitter.txt has how many lines of text?
    n1 <- countLines("Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
    print(n1)
}

# What is the length of the longest line seen in any of the three en_US data
# sets?
question3 <- function() {

    longestLine <- 0
    filestoread <- c("Coursera-SwiftKey/final/en_US/en_US.twitter.txt",
                     "Coursera-SwiftKey/final/en_US/en_US.blogs.txt",
                     "Coursera-SwiftKey/final/en_US/en_US.news.txt")

    maxLineLn <- 0
    linesChunk <- 20000 # 20K at a time
    
    for (fl2read in filestoread) {
        print(paste("File to read: ", fl2read))
        
        totalLines <- countLines(fl2read)
        linesRead <- 0

        # 1. Open file
        fhandle <- file(fl2read, open = "r")
        
        # 2. Read n lines at a time and find the max length
        while (linesRead < totalLines) {
            templines <- readLines(fhandle, n = linesChunk)
            linesRead <- linesRead + linesChunk
            
            lenLines <- nchar(templines)
            tempmax <- max(lenLines)
            
            # If the current max len is greater than previously read lines
            if (tempmax > maxLineLn) {
                maxLineLn <- tempmax
                maxLineLnFile <- fl2read
            }
            
            #Debug prints
            print(paste("Lines Read so far: ", linesRead, " Out of: ", totalLines))
            print(paste("Current Max: ", maxLineLn))
        }
        
        print(paste("Longest line in the file: ", maxLineLnFile,
                    " is: ", maxLineLn))

        # Close the connection/file
        close(fhandle)
    }

    print(paste("Max Length of the line from all 3 files: ", maxLineLn))
    print(paste("From the file: ", maxLineLnFile))
}

# In the en_US twitter data set, if you divide the number of lines where the
# word "love" (all lowercase) occurs by the number of lines the word "hate" 
# (all lowercase) occurs, about what do you get?
question4 <- function() {

    filetoread <- "Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
                     
    linesChunk <- 20000 # 20K at a time
    linesRead  <- 0
    hatecount  <- 0
    lovecount  <- 0
    
    fhandle <- file(filetoread, open = "r")
    totalLines <- countLines(filetoread)
    
    while (linesRead < totalLines) {
        templines <- readLines(fhandle, n = linesChunk)
        linesRead <- linesRead + linesChunk
        
        # Find the number of occurences of word "love"
        lovefnd <- grepl("love", templines)
        # Find the number of occurences of word "hate"
        hatefnd <- grepl("hate", templines)
        
        lovecount <- lovecount + sum(lovefnd == TRUE)
        hatecount <- hatecount + sum(hatefnd == TRUE)
    }
    close(fhandle)
    
    anslovebyhate <- lovecount / hatecount
    
    print(lovecount)
    print(hatecount)
    print(paste("Love by Hate: ", round(anslovebyhate, 2)))
}

# The one tweet in the en_US twitter data set that matches the word
# "biostats" says what?
question5 <- function() {
    
    filetoread <- "Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
    
    linesChunk <- 20000 # 20K at a time
    linesRead  <- 0

    fhandle <- file(filetoread, open = "r")
    totalLines <- countLines(filetoread)
    
    while (linesRead < totalLines) {
        templines <- readLines(fhandle, n = linesChunk)
        linesRead <- linesRead + linesChunk
        
        # Find the line containing "biostats"
        fndbiostats <- grep("biostats", templines, ignore.case = TRUE)
        
        if (length(fndbiostats) > 0) {
            idxbiostats <- fndbiostats
            strbio <- templines[fndbiostats]
            print(paste("Found: ", strbio, " at: ",
                        linesRead+idxbiostats))
        }
    }
    close(fhandle)
    print(paste("Found \"biostats\": ", strbio))
}

# How many tweets have the exact characters "A computer once beat me at 
# chess, but it was no match for me at kickboxing". (I.e. the line matches those characters exactly.)
question6 <- function() {
    
    filetoread <- "Coursera-SwiftKey/final/en_US/en_US.twitter.txt"
    
    linesChunk <- 20000 # 20K at a time
    linesRead  <- 0
    count      <- 0
    
    fhandle <- file(filetoread, open = "r")
    totalLines <- countLines(filetoread)
    
    while (linesRead < totalLines) {
        templines <- readLines(fhandle, n = linesChunk)
        linesRead <- linesRead + linesChunk
        
        # Find the line containing "biostats"
        expmatch <- grepl("A computer once beat me at chess, but it was no match for me at kickboxing",
                            templines)
        
        count <- count + sum(expmatch == TRUE)

    }
    close(fhandle)
    print(paste("# of Match: ", count))
}
