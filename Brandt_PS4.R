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
           switch = "logical"
         ),
         prototype = prototype(
           chosenDoor = NULL,
           carDoor = NULL,
           switch = NULL
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

# Door object must include a value for switch. All other values will end up being generated
# as part of the function.
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
            possible_doors <- subset(possible_doors, possible_doors != object @ carDoor)
            object @ chosenDoor <- sample(possible_doors, 1)
          }
          if (object @ chosenDoor == object @ carDoor){
            object @ winner <- TRUE
          } else {
            object @ winner <- FALSE
          }
          })

generic_door <- new("door")
PlayGame(generic_door)

debug(PlayGame)
PlayGame(generic_door)
undebug(PlayGame)
