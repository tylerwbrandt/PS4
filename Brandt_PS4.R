## Tyler Brandt, PS 4

# Getting Started

# The function Monte_Hall plays a condensed version of the Monte_Hall game.
# There are no arguments to the function.
# The function will generate two numbers between 1 and 3, inclusive.
# If those two numbers are the same, Monte_Hall returns TRUE. Otherwise, it returns FALSE.
Monte_Hall <- function(){
  doorthing1 <- sample(1:3,1)
  doorthing2 <- sample(1:3, 1)
  if (doorthing1 == doorthing2){
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
setClass(class = "door",
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


