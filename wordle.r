library(crayon)
library(dplyr)
library(dictionaRy)
library(words)

# Global Variables
menuSeparator <<- magenta("-------------------------------------\n")

chooseRandomWord <- function() {
    # From the package words, extract any word of length chosen by the user
    # Implement for varying length (PENDING)
    wordToGuess <- ""
    lengthOfTheWord = 0
    while (!(lengthOfTheWord %in% c(2:15))) {
        cat("Enter the difficulty level (2 - 15): \n")
        cat(menuSeparator)
        lengthOfTheWord = as.numeric(readLines("stdin", 1))
    }
    while (TRUE) {
        # Get word equal to the difficulty level chosen by the user
        # Break from the loop only when the random word contains unique letters
        wordToGuess <- words[words$word_length == lengthOfTheWord,][sample(nrow(words[words$word_length == lengthOfTheWord, ]))[1],1]
        wordToGuessVector <- unlist(strsplit(wordToGuess, ""))
        if (length(wordToGuessVector) == length(unique(wordToGuessVector)))
            break
    }
    print(wordToGuess)
    return(wordToGuess)
}

resetGame <- function() {
    # Reset the game
    # Using super assignment operator for making the variables global
    wordToGuess <<- chooseRandomWord()
    minimumTriesAllowed <<- 6
    usedAlphabets <<- c()
}

usedAlphabetsDisplaying <- function() {
    # Printing the distinct used alphabets
    cat(bgYellow(black("\n Used Alphabets: ")), toupper(sort(usedAlphabets)), "\n")
}

usedAlphabetsUpdating <- function(userInput) {
    # Method to keep track of alphabets used (avoiding repetition)
    splitStringVector <- unlist(strsplit(userInput, ""))
    for (i in splitStringVector) {
        if (i %in% usedAlphabets)
            next
        else
            usedAlphabets <<- append(usedAlphabets, i)
    }
}

showProgress <- function(userInput) {
    # Green color - If the position of input character is correct
    # Yellow color - If the position of the input character is partially character
    userInputLetters <- unlist(strsplit(userInput, ""))
    wordToGuessLetters <- unlist(strsplit(wordToGuess, ""))
    index = 1

    while (index <= length(wordToGuessLetters)) {
        if (userInputLetters[index] %in% wordToGuessLetters){
            if (userInputLetters[index] == wordToGuessLetters[index]) {
                userInputLetters[index] = green(userInputLetters[index])
            }
            else
                userInputLetters[index] = yellow(userInputLetters[index])
        }
        index = index + 1
    }
    cat(userInputLetters)
}

showDetailsOfWord <- function() {
    # Method to show definition of the correct word
    word_info <- define(wordToGuess)
    cat(wordToGuess, " information \n")
    cat(menuSeparator)
    cat("Definition \n")
    print(word_info %>% select(definition))
    cat("\n")
    cat(menuSeparator)
}

playGame <- function() {
    # Method to play game
    resetGame()
    while (minimumTriesAllowed != 0) {
        cat("\n Minimum tries left: ", minimumTriesAllowed, "\n")
        cat("\n Guess the word: \n")
        userInput <- tolower(readLines("stdin", 1))
        if (userInput == wordToGuess) {
            cat("Congratulations! You cracked the puzzle!")
            showDetailsOfWord()
            break
        } else if (minimumTriesAllowed >= 1) {
            minimumTriesAllowed = minimumTriesAllowed - 1
            # Actual Game Logic
            usedAlphabetsUpdating(userInput)
            usedAlphabetsDisplaying()
            showProgress(userInput)
        } else {
            cat("Oops! You couldn't guess the word")
            cat("The word was: ", wordToGuess)
            showDetailsOfWord()
        }
    }    
}

while (TRUE) {
    # Menu
    cat(bgGreen(black("WORDLE! - Developed using R Programming \n")))
    cat(blue("1. Play Game \n"))
    cat(red("2. Exit \n"))
    cat(menuSeparator)
    userChoice = 0
    cat("Enter choice: \n")
    while (!(userChoice %in% c(1, 2))) {
        cat(menuSeparator)
        userChoice <- readLines("stdin", 1)
    }
    if (userChoice == 1) {
        cat("Lets Play! \n")
        cat(menuSeparator)
        playGame()
    } else {
        cat("Thank you for playing. See you soon! \n")
        cat(menuSeparator)
        break
    }
}