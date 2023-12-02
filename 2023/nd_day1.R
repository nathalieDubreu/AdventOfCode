source("src/functions.R")
source("src/fonctionsND.R")

name <- "nathalieDubreu"
year <- 2023
day <- 1
input <- aoc_get_inputfile(day, year, name)

day <- inputToStrings(input)

## Star 1

## Données test
dayTest <- c(
  "1abc2",
  "pqr3stu8vwx",
  "a1b2c3d4e5f",
  "treb7uchet"
)

star1 <- function(day) {
  result <- 0
  for (ligne in day) {
    if (ligne != "") {
      result <- result + as.numeric(paste(stri_extract_first(ligne, regex = "[1-9]"), stri_extract_last(ligne, regex = "[1-9]"), sep = ""))
    }
  }
  print(result)
}

star1(dayTest)
star1(day)
## 55029


## Star 2

## Donnees test
dayATest <- c(
  "two1nine",
  "eightwothree",
  "abcone2threexyz",
  "xtwone3four",
  "4nineeightseven2",
  "zoneight234",
  "7pqrstsixteen"
)

## Verrues dès le Day 1... Lettres en commun à traiter...
dayA <- str_replace_all(day, "oneight", "oneeight")
dayA <- str_replace_all(dayA, "twone", "twoone")
dayA <- str_replace_all(dayA, "threeight", "threeeight")
dayA <- str_replace_all(dayA, "fiveight", "fiveeight")
dayA <- str_replace_all(dayA, "sevenine", "sevennine")
dayA <- str_replace_all(dayA, "eighthree", "eightthree")
dayA <- str_replace_all(dayA, "eightwo", "eighttwo")
dayA <- str_replace_all(dayA, "nineight", "nineeight")

recupererChiffre <- function(string) {
  correspondance <- c(
    "one" = 1, "two" = 2, "three" = 3, "four" = 4, "five" = 5,
    "six" = 6, "seven" = 7, "eight" = 8, "nine" = 9
  )
  chiffre <- ifelse(nchar(string) > 1,
    correspondance[string],
    as.numeric(string)
  )
  return(chiffre)
}

star2 <- function(dayA) {
  result <- 0
  regEx <- "one|two|three|four|five|six|seven|eight|nine|[1-9]"
  for (ligne in dayA) {
    if (ligne != "") {
      first <- recupererChiffre(stri_extract_first(ligne, regex = regEx))
      last <- recupererChiffre(stri_extract_last(ligne, regex = regEx))
      result <- result + as.numeric(paste(first, last, sep = ""))
    }
  }
  print(result)
}

star2(dayATest)
star2(dayA)
# 55686
