## Tyler Brandt, PS 4

# Getting Started

# The function Monte_Hall plays a condensed version of the Monte_Hall game.
# There are no arguments to the function.
# The function will generate two numbers between 1 and 3, inclusive.
# If those two numbers are the same, Monte_Hall returns TRUE. Otherwise, it returns FALSE.
Monte_Hall <- function(){
  door1 <- sample(1:3,1)
  door2 <- sample(1:3, 1)
  if (door1 == door2){
    x <- TRUE
  } else {
    x <- FALSE
  }
  return (x)
}

Monte_Hall()
# Should return a TRUE if these samples are equal and
# a false if they are not

# 1. Create new class
setClass(Class = "door",
         representation = representation(
           chosenDoor = "numeric",
           carDoor = "numeric",
           switch = "logical",
           winner = "logical"
         ),
         prototype = prototype(
           chosenDoor = NULL,
           carDoor = NULL,
           switch = NULL,
           winner = NULL
         ))

# chosenDoor is 1,2, or 3. carDoor is 1,2, or 3. switch is T or F
setValidity("door", function(object){
  if (!(object @ chosenDoor %in% c(1,2,3))){
    return ("@chosenDoor must be either 1, 2, or 3")
  }
  if (!(object @ carDoor %in% c(1,2,3))){
    return ("@carDoor must be either 1, 2, or 3")
  }
  if (!(object @ switch %in% c(T,F))){
    return ("@switch must be either TRUE or FALSE")
  }
})

# 2. Create PlayGame Function

# The PlayGame function takes in a valid door object as an argument.
# It will then play the Monte Hall game. The chosenDoor and carDoor will be randomly assigned.
# The function will then remove one of the doors that is neither the carDoor nor the chosenDoor.
# If switch == TRUE, then chosenDoor will switch to the other door that remains.
# If switch == FALSE, then chosenDoor will stay the same.
# The function assigns the winner slot based on whether or not chosenDoor == carDoor at the end of the game.
# The entire door object will be returned after the game's completion.
setGeneric("PlayGame",
           function(object){
             standardGeneric("PlayGame")
           })

setMethod("PlayGame", "door",
          function(object){
          random1 <- sample(1:3,1)
          object @ carDoor <- random1
          random2 <- sample(1:3,1)
          if (object @ switch == FALSE){
            object @ chosenDoor <- random2
          } else if (object @ switch == TRUE){
            possible_doors <- c(1,2,3)
            possible_goats <- subset(possible_doors, possible_doors != object @ carDoor)
            goat_door <- sample(subset(possible_goats, possible_goats != object @ chosenDoor),1)
            possible_doors <- subset(possible_doors, possible_doors != goat_door)
            new_door <- subset(possible_doors, possible_doors != object @ chosenDoor)
            object @ chosenDoor <- new_door
          }
          if (object @ chosenDoor == object @ carDoor){
            object @ winner <- TRUE
          } else {
            object @ winner <- FALSE
          }
          return (object)
          })

generic_door <- new("door", chosenDoor = sample(1:3,1), carDoor = sample(1:3,1), switch = FALSE, winner = FALSE)
PlayGame(generic_door)

# Simulation

# switch == FALSE simulation
false_door <- new("door", chosenDoor = sample(1:3,1), carDoor = sample(1:3,1), switch = FALSE, winner = FALSE)
false_results <- NULL
for (i in 1:1000){
  false_results <- c(false_results, PlayGame(false_door) @ winner)
}
