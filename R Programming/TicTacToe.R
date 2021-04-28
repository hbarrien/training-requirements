# TicTacToe.R
#
# Creation date:
#   2021-04-12
#
# Author:
#   Herbert Barrientos (hpbarr@gmail.com)
#
# Version:
#   1.0
#
# Description:
#   Implements the tic-tac-toe game, whereby the computer
#   (identified as ME) plays against a human opponent
#   (identified as THEY). Initially, THEY is prompted if
#   they want to play first.
#
#   The first two moves are crucial for establishing a
#   playing strategy. This applies when the ME plays
#   first, as well as when it plays second. For this
#   reason, these two moves are controlled in sequence.
#
#   Starting on the third move, the process controls who
#   plays next. The winner is eventually reported, just
#   as when a draw is reached. At this point, THEY is
#   prompted is they would like to play again. If so,
#   the game starts anew.
#
#   Strategy:
#   1. As stated before, the first two moves are crucial,
#      regardless of whether ME plays first or second.
#
#   2. Starting on the third move, ME will apply the
#      following strategies:
#      winGame: ME's next move will be a win.
#
#      block: ME blocks a threat from THEY.
#
#      createThreat: ME's next move creates a simple
#      threat or a fork.
#
#      playAnywhere: when all strategies have failed,
#      ME plays the next move by choosing a cell position
#      at random.


# #######################
# ###### LIBRARIES ######
# #######################
library(data.table)


# #######################
# ###### CONSTANTS ######
# #######################
# cell positions
P0 <- 0  # invalid
P1 <- 1  # upper left-hand corner
P2 <- 2  # upper edge
P3 <- 3  # upper right-hand corner
P4 <- 4  # left center edge
P5 <- 5  # the center
P6 <- 6  # right center edge
P7 <- 7  # lower left-hand corner
P8 <- 8  # lower edge
P9 <- 9  # lower right-hand corner

CORNERS <- c(P1, P3, P7, P9)
EDGES   <- c(P2, P4, P6, P8)

# incidence structure
R1 <- c(P1,P2,P3)
R2 <- c(P4,P5,P6)
R3 <- c(P7,P8,P9)

C1 <- c(P1,P4,P7)
C2 <- c(P2,P5,P8)
C3 <- c(P3,P6,P9)

D1 <- c(P1,P5,P9)
D2 <- c(P3,P5,P7)

INCIDENCE_STRUCT     <- list(R1, R2, R3, C1, C2, C3, D1, D2)
LEN_INCIDENCE_STRUCT <- length(INCIDENCE_STRUCT)

# intersections
INTERSECTIONS <- list(
  list(R1,C1),
  list(R1,C2),
  list(R1,C3),
  list(R1,D1),
  list(R1,D2),

  list(R2,C1),
  list(R2,C2),
  list(R2,C3),
  list(R2,D1),
  list(R2,D2),

  list(R3,C1),
  list(R3,C2),
  list(R3,C3),
  list(R3,D1),
  list(R3,D2),

  list(C1,D1),
  list(C1,D2),

  list(C2,D1),
  list(C2,D2),

  list(C3,D1),
  list(C3,D2),

  list(D1,D2)
)

# playing symbols
X <- "X"
O <- "O"

# display elements
BOARD_HDIV <- "-----------"
BOARD_VDIV <- "|"
NEWLINE    <- "\n"
EMPTY      <- " "

# players
THEY <- 0  # the human opponent
ME   <- 1  # the computer

# prompts
PROMPT_PLAY_FIRST <- "Would you like to play first? [Y/N]: "
PROMPT_PLAY_AGAIN <- "Would you like to play again? [Y/N]: "
PROMPT_YOUR_TURN  <- "Your turn: "

# messages
MSG_I_WON   <- "I won!!"
MSG_YOU_WON <- "You won! Congratulations!!"

# error messages
ERROR_MSG_BAD_INPUT_CONFIRMATION <- "Bad input. Please respond with 'Y' or 'N'."
ERROR_MSG_BAD_INPUT_POS_NUMBER   <- "Bad input. Please provide a valid cell number."
ERROR_MSG_BAD_INPUT_POS_TAKEN    <- "Bad input. The selected position is already taken. Please choose another one"

ERROR_MSG_0 <- "Unable to initialize control variables."
ERROR_MSG_1 <- "Invalid state of variable sequenceX."
ERROR_MSG_2 <- "Invalid state of variable sequenceX."
ERROR_MSG_3 <- "Invalid state of variable sequenceO."
ERROR_MSG_4 <- "Invalid state of variable sequenceX."
ERROR_MSG_5 <- "Invalid state of variable sequenceO."
ERROR_MSG_6 <- "An error occurred. Sorry for the inconvenience."

# yes/no responses
YES <- "Y"
NO  <- "N"


# #######################
# ###### VARIABLES ######
# #######################
gameBoard        <- NULL
nextPlayer       <- NULL
availableCorners <- NULL
availableEdges   <- NULL
sequenceX        <- NULL
sequenceO        <- NULL


# #######################
# ###### FUNCTIONS ######
# #######################

# ###### AUXILIARY ######
# getConnection
#
# Description:
#   Returns a connection for manual user input.
#
# Arguments:
#   None.
#
# Returns:
#   NULL   : an error occurred
#   stdin(): if R has been started in interactive mode (e.g., a GUI console)
#   "stdin": if R has been started in a terminal (e.g., a command line window)
#
# Precondition:
#
# Postcondition:
#   A connection has been delivered
#
getConnection <- function() {

  tryCatch({

    if (interactive())
      return(stdin())

    return("stdin")

  }, error = function(e) {

    # do nothing
  })

  return(NULL)

}  # END getConnection


# ###### INIT ######
# initGameBoard
#
# Description:
#   Creates a 3 x 3 data.table with all cells having the EMPTY value.
#   Next, for nomenclatural consistency, the columns are renamed the
#   same as the row names. The created data.table is assigned to global
#   variable gameBoard.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : the postcondition holds
#
# Precondition:
#
# Postcondition:
#   Global variable gameBoard has been instantiated and initialized
#
initGameBoard <- function() {

  retVal <- TRUE

  tryCatch({

    gameBoard <<- data.table(c(EMPTY, EMPTY, EMPTY),
                             c(EMPTY, EMPTY, EMPTY),
                             c(EMPTY, EMPTY, EMPTY))

    colnames(gameBoard) <<- c("1", "2", "3")

  }, error = function(e) {

    retVal <<- FALSE
  })

  return(retVal)

}  # END initGameBoard


# initNextPlayer
#
# Description:
#   Sets global variable nextPlayer by determining who
#   starts playing first.
#
# Arguments:
#   opponentResponse: opponent's preference when prompted
#   if they want to start the game.
#
# Returns:
#   FALSE: the precondition is not met
#   TRUE : the postcondition holds
#
# Precondition:
#   Argument opponentResponse is boolean
#
# Postcondition:
#   Global variable nextPlayer has been set
#
initNextPlayer <- function(opponentResponse) {

  if (is.na(opponentResponse) || !is.logical(opponentResponse))
    return(FALSE)

  nextPlayer <<- ifelse(opponentResponse, THEY, ME)

  return(TRUE)

}  # END initNextPlayer


# initAvailableCorners
#
# Description:
#   Initializes global variable availableCorners with all table corners.
#
# Arguments:
#   None.
#
# Returns:
#   TRUE: the postcondition holds
#
# Precondition:
#
# Postcondition:
#   Global variable availableCorners has been instantiated and initialized
#
initAvailableCorners <- function() {

  availableCorners <<- CORNERS

  return(TRUE)

}  # END initAvailableCorners


# initAvailableEdges
#
# Description:
#   Initializes global variable availableEdges with all table edges.
#
# Arguments:
#   None.
#
# Returns:
#   TRUE: the postcondition holds
#
# Precondition:
#
# Postcondition:
#   Global variable availableEdges has been instantiated and initialized
#
initAvailableEdges <- function() {

  availableEdges <<- EDGES

  return(TRUE)

}  # END initAvailableEdges


# initSequences
#
# Description:
#   Initializes the playing sequence of both opponents
#   by creating empty vectors.
#
# Arguments:
#   None.
#
# Returns:
#   TRUE: the postcondition holds
#
# Precondition:
#
# Postcondition:
#   1. Global variable sequenceX has been initialized
#   2. Global variable sequenceO has been initialed
#
initSequences <- function() {

  sequenceX <<- c()
  sequenceO <<- c()

  return(TRUE)

}  # END initSequences


# initGame
#
# Description:
#   Instantiates and initializes global variables that
#   maintain game state.
#
# Arguments:
#   None.
#
# Returns:
#
# Precondition:
#
# Postcondition:
#
initGame <- function() {

  if (!(initGameBoard() && initAvailableCorners() && initAvailableEdges() && initSequences()))
    stop(ERROR_MSG_0)

}  # END initGame


# ###### SETTERS ######
# setGameBoardPosValue
#
# Description:
#   Sets a cell, defined by argument pos, in global variable
#   gameBoard, with the character given by argument symbol.
#
# Arguments:
#   pos   : integer
#   symbol: character
#
# Returns:
#   FALSE: the precondition was not met, or an error occurred
#   TRUE : the postcondition holds
#
# Precondition:
#   (gameBoard != NULL) && (P1 <= pos <= P9) &&
#   ((symbol == X) || (symbol == O))
#
# Postcondition:
#   ((Global variable gameBoard has been updated) &&
#    ((pos is a corner) --> global variable availableCorners has been updated) &&
#    ((pos is an edge)  --> global variable availableEdges has been updated) &&
#    ((global variable sequenceX has been updated) ||
#     (global variable sequenceO has been updated)))
#
setGameBoardPosValue <- function(pos, symbol) {

  if (is.null(gameBoard) || !(isPosValid(pos) && isSymbolValid(symbol)))
    return(FALSE)

  posCoords <- getPosCoords(as.integer(pos))
  if (is.null(posCoords)) return(FALSE)

  i <- posCoords[1]
  j <- posCoords[2]

  gameBoard[i, j] <<- symbol

  updateAvailableCorners(pos)
  updateAvailableEdges(pos)

  if(symbol == X) updateSequenceX(pos)
  else updateSequenceO(pos)

  return(TRUE)

}  # END setGameBoardPosValue


# setNextPlayer
#
# Description:
#   Updates global variable nextPlayer in a toggle manner.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: the precondition is not met
#   TRUE : the postcondition holds
#
# Precondition:
#   (nextPlayer != NULL) &&
#   ((nextPlayer == THEY) || (nextPlayer == ME))
#
# Postcondition:
#   Global variable nextPlayer has been updated
#
setNextPlayer <- function() {

  if (is.null(nextPlayer) || ((nextPlayer != THEY) && (nextPlayer != ME)))
    return(FALSE)

  nextPlayer <<- ifelse(nextPlayer == THEY, ME, THEY)

  return(TRUE)

}  # END setNextPlayer


# updateAvailableCorners
#
# Description:
#   If argument pos is a corner, this program updates
#   global variable availableCorners.
#
# Arguments:
#   pos: integer or string
#
# Returns:
#   FALSE: the precondition does not hold
#   TRUE : the postcondition holds
#
# Precondition:
#   ((P1 <= pos <= P9) && (pos is a corner))
#
# Postcondition:
#   Global variable availableCorners was updated
#
updateAvailableCorners <- function(pos) {

  if (!(isPosValid(pos) && isPosACorner(pos)))
    return(FALSE)

  # convert to integer, in case it's a string
  pos <- as.integer(pos)

  availableCorners <<- availableCorners[availableCorners != pos]

  return(TRUE)

}  # END updateAvailableCorners


# updateAvailableEdges
#
# Description:
#   If argument pos is an edge, this program updates
#   global variable availableEdges.
#
# Arguments:
#   pos: integer or string
#
# Returns:
#   FALSE: the precondition does not hold
#   TRUE : the postcondition holds
#
# Precondition:
#   ((P1 <= pos <= P9) && (pos is an edge))
#
# Postcondition:
#   Global variable availableEdges was updated
#
updateAvailableEdges <- function(pos) {

  if (!(isPosValid(pos) && isPosAnEdge(pos)))
    return(FALSE)

  # convert to integer, in case it's a string
  pos <- as.integer(pos)

  availableEdges <<- availableEdges[availableEdges != pos]

}  # END updateAvailableEdges


# updateSequenceX
#
# Description:
#   Appends argument pos to global variable sequenceX.
#
# Arguments:
#   pos: integer or string
#
# Returns:
#   FALSE: the precondition does not hold
#   TRUE : the postcondition holds
#
# Precondition:
#   (P1 <= pos <= P9)
#
# Postcondition:
#   Global variable sequenceX has been updated
#
updateSequenceX <- function(pos) {

  if (!isPosValid(pos))
    return(FALSE)

  # convert pos to integer, in case it's a string
  pos <- as.integer(pos)

  sequenceX <<- c(sequenceX, pos)

  return(TRUE)

}  # END updateSequenceX


# updateSequenceO
#
# Description:
#   Appends argument pos to global variable sequenceO.
#
# Arguments:
#   pos: integer or string
#
# Returns:
#   FALSE: the precondition does not hold
#   TRUE : the postcondition holds
#
# Precondition:
#   (P1 <= pos <= P9)
#
# Postcondition:
#   Global variable sequenceO has been updated
#
updateSequenceO <- function(pos) {

  if (!isPosValid(pos))
    return(FALSE)

  # convert pos to integer, in case it's a string
  pos <- as.integer(pos)

  sequenceO <<- c(sequenceO, pos)

  return(TRUE)

}  # END updateSequenceO


# ###### GETTERS ######
# getGameBoardPosValue
#
# Description:
#   Returns the symbol currently set to the cell, in global
#   variable gameBoard, given by argument pos.
#
# Arguments:
#   pos: integer
#
# Returns:
#   NULL: the precondition is not met, or an error occurred
#   character: either EMPTY, X, or O
#
# Precondition:
#   (gameBoard != NULL) && (P1 <= pos <= P9)
#
# Postcondition:
#   A symbol has been delivered
#
getGameBoardPosValue <- function(pos) {

  if (is.null(gameBoard) || !isPosValid(pos))
    return(NULL)

  posCoords <- getPosCoords(pos)
  if (is.null(posCoords)) return(NULL)

  i <- posCoords[1]
  j <- posCoords[2]

  return(as.character(gameBoard[i, j, with=FALSE]))

}  # END getGameBoardPosValue


# getNumberedBoard
#
# Description:
#   Returns a textual representation of the game board with numbered cells.
#
# Arguments:
#   None.
#
# Returns:
#   Non-empty string
#
# Precondition:
#
# Postcondition:
#   A textual representation of the game board has been delivered
#
getNumberedBoard <- function() {

  r1 <- paste0(EMPTY, P1, EMPTY, BOARD_VDIV, EMPTY, P2, EMPTY, BOARD_VDIV, EMPTY, P3)
  r2 <- paste0(EMPTY, P4, EMPTY, BOARD_VDIV, EMPTY, P5, EMPTY, BOARD_VDIV, EMPTY, P6)
  r3 <- paste0(EMPTY, P7, EMPTY, BOARD_VDIV, EMPTY, P8, EMPTY, BOARD_VDIV, EMPTY, P9)

  return(paste0(r1, NEWLINE, BOARD_HDIV, NEWLINE, r2, NEWLINE, BOARD_HDIV, NEWLINE, r3))

}  # END getNumberedBoard


# getIntersects
#
# Description:
#   Given a cell number provided by argument pos, this function returns
#   all gameBoard rows, columns, and diagonals that have the cell number
#   in common.
#
# Arguments:
#   pos: integer
#
# Returns:
#   NULL: the precondition is not met
#   List: the postcondition holds
#
# Precondition:
#   (gameBoard != NULL) && (P1 <= pos <= P9)
#
# Postcondition:
#   A non-empty list has been delivered
#
getIntersects <- function(pos) {

  if (is.null(gameBoard) || !isPosValid(pos))
    return(NULL)

  intersects <- list()

  for (elt in INTERSECTIONS) {

    if (is.element(pos, intersect(elt[[1]], elt[[2]]))) {

      intersects[[length(intersects)+1]] <- elt[[1]]
      intersects[[length(intersects)+1]] <- elt[[2]]

    }  # END if

  }  # END for

  if (length(intersects) == 0)
    return(NULL)

  return(unique(intersects))

}  # END getIntersects


# getPosCoords
#
# Description:
#   Given a cell number provided by argument pos, this function returns the
#   corresponding row and column coordinates in global variable gameBoard.
#
# Arguments:
#   pos: integer
#
# Returns:
#   NULL: the precondition is not met, or an error occurred
#   A vector containing row and column coordinates
#
# Precondition:
#   (P1 <= pos <= P9)
#
# Postcondition:
#   The coordinates for argument pos have been delivered
#
getPosCoords <- function(pos) {

  if (!isPosValid(pos))
    return(NULL)

  switch(pos,
         P1 = {return(c(1,1))},
         P2 = {return(c(1,2))},
         P3 = {return(c(1,3))},
         P4 = {return(c(2,1))},
         P5 = {return(c(2,2))},
         P6 = {return(c(2,3))},
         P7 = {return(c(3,1))},
         P8 = {return(c(3,2))},
         P9 = {return(c(3,3))}
  )

  # at this point, an error has occurred
  return(NULL)

}  # END getPosCoords


# getPosFromSequenceX
#
# Description:
#   Responds a cell number located at position idx
#   in global variable sequenceX.
#
# Arguments:
#   idx: integer
#
# Returns:
#   P0: the precondition does not hold
#   integer > P0: the postcondition holds
#
# Precondition:
#   (|sequenceX| > 0) && (1 <= idx |sequenceX|)
#
# Postcondition:
#   A cell number has been delivered
#
getPosFromSequenceX <- function(idx) {

  lseqX <- length(sequenceX)

  if (is.null(idx) || is.na(idx) || !is.numeric(idx) ||
      (lseqX == 0) || (idx < 1) || (idx > lseqX))
    return(P0)

  pos <- sequenceX[idx]
  return(pos)

}  # END getPosFromSequenceX


# getPosFromSequenceO
#
# Description:
#   Responds a cell number located at position idx
#   in global variable sequenceO.
#
# Arguments:
#   idx: integer
#
# Returns:
#   P0: the precondition does not hold
#   integer > P0: the postcondition holds
#
# Precondition:
#   (|sequenceO| > 0) && (1 <= idx |sequenceO|)
#
# Postcondition:
#   A cell number has been delivered
#
getPosFromSequenceO <- function(idx) {

  lseqX <- length(sequenceO)

  if (is.null(idx) || is.na(idx) || !is.numeric(idx) ||
      (lseqX == 0) || (idx < 1) || (idx > lseqX))
    return(P0)

  pos <- sequenceO[idx]
  return(pos)

}  # END getPosFromSequenceO


# getIncidenceStructElementAt
#
# Description:
#   Responds a vector, located at position idx in INCIDENCE_STRUCT,
#   containing row, column, or diagonal cell positions.
#
# Arguments:
#   idx: integer
#
# Returns:
#   NULL: the precondition does not hold
#   non-empty vector: the postcondition holds
#
# Precondition:
#   (|INCIDENCE_STRUCT| > 0) && (1 <= idx |INCIDENCE_STRUCT|)
#
# Postcondition:
#   A vector has been delivered
#
getIncidenceStructElementAt <- function(idx) {

  lIncidenceStr <- length(INCIDENCE_STRUCT)

  if (is.null(idx) || is.na(idx) || !is.numeric(idx) ||
      (lIncidenceStr == 0) || (idx < 1) || (idx > lIncidenceStr))
    return(NULL)

  return(INCIDENCE_STRUCT[[idx]])

}  # END getIncidenceStructElementAt


# getOppositeCorner
#
# Description:
#   Responds the opposite corner cell number
#   of argument pos.
#
# Arguments:
#   pos: integer or string
#
# Returns:
#   P0: the precondition does not hold
#   integer > 0:
#
# Precondition:
#   (pos in {P1, P3, P7, P9})
#
# Postcondition:
#   A corner cell number has been delivered
#
getOppositeCorner <- function(pos) {

  if (!isPosValid(pos) || !isPosACorner(pos))
    return(P0)

  # convert to integer, in case it's a string
  pos <- as.integer(pos)

  if (pos == P1) return(P9)
  if (pos == P3) return(P7)
  if (pos == P7) return(P3)
  if (pos == P9) return(P1)

}  # END getOppositeCorner


# ###### VALIDATION ######
# isPosValid
#
# Description:
#   Validates argument pos. In essence, pos is valid
#   when (P1 <= pos <= P9)
#
#   Note: (1.0 == 1) since they are both whole values.
#   For example, if the input is 1.0, it will be treated
#   as 1. Likewise, 2.0 will be treated as 2, and so on
#   until 9.0. This validation does not apply to string
#   values such as "1.0", or "2.0", however.
#
# Arguments:
#   pos: may be a number or a string
#
# Returns:
#   FALSE: Argument pos is invalid
#   TRUE : Otherwise
#
# Precondition:
#
# Postcondition:
#
isPosValid <- function(pos) {

  isValid <- TRUE

  tryCatch({

    if (is.null(pos) || is.na(pos) || !grepl("[1-9]", pos) || (nchar(pos) > 1))
    return(FALSE)

  }, error = function(e) {

    isValid <<- FALSE
  })

  if (!isValid) return(FALSE)

  pos <- as.integer(pos)
  return((pos >= P1) && (pos <= P9))

}  # END isPosValid


# isSymbolValid
#
# Description:
#   Validates argument symbol. In essence, symbol is valid
#   when ((symbol == X) || (symbol == O))
#
# Arguments:
#   symbol: string
#
# Returns:
#   FALSE: Argument symbol is invalid
#   TRUE : Otherwise
#
# Precondition:
#
# Postcondition:
#
isSymbolValid <- function(symbol) {

  isValid <- TRUE

  tryCatch({

    if (is.null(symbol) || is.na(symbol) || ((symbol != X) && (symbol != O)))
      return(FALSE)

  }, error = function(e) {

    isValid <<- FALSE
  })

  if (!isValid) return(FALSE)

  return(TRUE)

}  # END isSymbolValid


# isConfirmationValid
#
# Description:
#   Validates argument value. In essence, value is valid
#   when ((value == "Y") || (value == "y") ||
#         (value == "N") || (value == "n"))
#
# Arguments:
#   value: string
#
# Returns:
#   FALSE: Argument value is not valid
#   TRUE : Otherwise
#
# Precondition:
#
# Postcondition:
#
isConfirmationValid <- function(value) {

  if (is.null(value) || is.na(value) || !is.character(value))
    return(FALSE)

  return((nchar(value) == 1) && grepl("[YyNn]", value))

}  # END isConfirmationValid


# ###### TESTING ######
# isPosACorner
#
# Description:
#   Determines if argument pos is a corner position
#   within global variable gameBoard.
#
# Arguments:
#   pos: may be a number or a string
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: pos is not a corner position
#   TRUE : Otherwise
#
# Precondition:
#   (P1 <= pos <= P9)
#
# Postcondition:
#
isPosACorner <- function(pos) {

  if (!isPosValid(pos))
    return(NULL)

  # convert to integer, in case it's a string
  pos <- as.integer(pos)

  return(is.element(pos, CORNERS))

}  # END isPosACorner


# isPosAnEdge
#
# Description:
#   Determines if argument pos is an edge position
#   within global variable gameBoard.
#
# Arguments:
#   pos: may be a number or a string
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: pos is not an edge position
#   TRUE : Otherwise
#
# Precondition:
#   (P1 <= pos <= P9)
#
# Postcondition:
#
isPosAnEdge <- function(pos) {

  if (!isPosValid(pos))
    return(NULL)

  # convert to integer, in case it's a string
  pos <- as.integer(pos)

  return(is.element(pos, EDGES))

}  # END isPosAnEdge


# isPosTheCenter
#
# Description:
#   Determines if argument pos is the center position
#   within global variable gameBoard.
#
# Arguments:
#   pos: may be a number or a string
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: pos is not the center position
#   TRUE : Otherwise
#
# Precondition:
#   (P1 <= pos <= P9)
#
# Postcondition:
#
isPosTheCenter <- function(pos) {

  if (!isPosValid(pos))
    return(NULL)

  # convert to integer, in case it's a string
  pos <- as.integer(pos)

  return(pos == P5)

}  # END isPosTheCenter


# isPosTaken
#
# Description:
#   Responds whether a cell in global variable gameBiard,
#   whose number is given by argument pos, has been taken
#   or not. In esence, not taken means that the cell's
#   symbol is EMPTY.
#
# Arguments:
#   pos: may be integer or string
#
# Returns:
#   NULL : the precondition does not hold, or an error occurred
#   FALSE: the cell referenced by pos has not been taken
#   TRUE : the cell has been taken
#
# Precondition:
#   (P1 <= pos <= P2)
#
# Postcondition:
#
isPosTaken <- function(pos) {

  if (!isPosValid(pos))
    return(NULL)

  value <- getGameBoardPosValue(pos)
  if (is.null(value)) return(NULL)

  return(value != EMPTY)

}  # END isPosTaken


# isWinFor
#
# Description:
#   Determines if a player, represented by argument
#   symbol, has won the game.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: the game is not in a state of win
#   TRUE : the game is in a state of win
#
# Precondition:
#   ((symbol == X) || (symbol == O))
#
# Postcondition:
#   the state of the game has been reported
#
isWinFor <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  for (idx in 1:LEN_INCIDENCE_STRUCT) {

    elt <- getIncidenceStructElementAt(idx)

    if ((getGameBoardPosValue(elt[1]) == symbol) &&
        (getGameBoardPosValue(elt[2]) == symbol) &&
        (getGameBoardPosValue(elt[3]) == symbol))
      return(TRUE)

  }  # END for

  return(FALSE)

}  # END isWinFor


# isGameBoardFull
#
# Description:
#   Determines if all cells in global variable gameBoard
#   have benn taken, i.e., no cells with value EMPTY exist.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: gameBoard is not full
#   TRUE : gameBoard is full
#
# Precondition:
#
# Postcondition:
#
isGameBoardFull <- function() {

  boardFull <- TRUE

  for (elt in INCIDENCE_STRUCT) {

    if (!(isPosTaken(elt[1]) &&
          isPosTaken(elt[2]) &&
          isPosTaken(elt[3]))) {

      boardFull <- FALSE
      break()

    }  # END if

  }  # END for

  return(boardFull)

}  # END isGameBoardFull


# whosNext
#
# Description:
#   Responds the next player.
#
# Arguments:
#   None.
#
# Returns:
#   THEY: when it's the human opponent's turn to play
#   ME  : when it's the computer's turn to play
#
# Precondition:
#
# Postcondition:
#
whosNext <- function() {

  return(nextPlayer)

}  # END whosNext


# ###### INPUT ######
# promptConfirm
#
# Description:
#   Prompts the user for a yes/no response. Upon bad input,
#   this program will indefinitely insist until a valid answer
#   is received.
#
# Arguments:
#   promptMsg: string containing the message displayed to the user
#
# Returns:
#   NULL: precondition does not hold, or an error occurred
#   Character: the postcondition holds
#
# Precondition:
#   (promptMsge is a non-empty string) && (connection is valid))
#
# Postcondition:
#   A valid user response (YES || NO) has been delivered
#
promptConfirm <- function(promptMsg) {

  if (is.null(promptMsg) || is.na(promptMsg) || !is.character(promptMsg) ||
      nchar(promptMsg) < 1)
    return(NULL)

  con <- getConnection()
  if (is.null(con)) return(NULL)

  tryCatch({

    while (TRUE) {

      cat(paste0(NEWLINE, promptMsg))
      answer <- readLines(con = con, n = 1)

      if (isConfirmationValid(answer))
        break()

      # display error message
      cat(paste0(NEWLINE, ERROR_MSG_BAD_INPUT_CONFIRMATION, NEWLINE, NEWLINE))

    }  # END if

    return(toupper(answer))

  }, error = {

    # do nothing
  })

  # at this point, an error occurred
  return(NULL)

}  # END promptConfirm


# promptToPlay
#
# Description:
#   Prompts the user for a cell number. Upon bad input, this
#   program will indefinitely insist until a valid answer is
#   received.
#
# Arguments:
#   None.
#
# Returns:
#   NULL: precondition does not hold, an error occurred
#   Integer: the postcondition holds
#
# Precondition:
#   (connection is valid)
#
# Postcondition:
#   A valid and untaken user response (P1 || P2 || ... || P9)
#   has been delivered
#
promptToPlay <- function() {

  con <- getConnection()
  if (is.null(con)) return(NULL)

  tryCatch({

    while (TRUE) {

      cat(NEWLINE, PROMPT_YOUR_TURN)
      answer <- readLines(con = con, n = 1)

      if (!isPosValid(answer)) {

        cat(paste0(NEWLINE, ERROR_MSG_BAD_INPUT_POS_NUMBER, NEWLINE, NEWLINE))
        next()

      }  # END if

      answer <- as.integer(answer)

      if (isPosTaken(answer)) {

        cat(paste0(NEWLINE, ERROR_MSG_BAD_INPUT_POS_TAKEN, NEWLINE, NEWLINE))
        next()

      }  # END if

      break()

    }  # END while

    return(answer)

  }, error = {

    # do nothing
  })

  # at this point, an error occurred
  return(NULL)

}  # END promptToPlay


# ###### OUTPUT ######
# displayIntro
#
# Description:
#   Prints an introduction to the game to the current connection.
#   The introduction includes a numbered game board for user
#   reference.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : success
#
# Precondition:
#
# Postcondition:
#   An introductory message to the user has been delivered
#
displayIntro <- function() {

  tryCatch({

    cat(paste0(NEWLINE, "Let's play some tic-tac-toe!", NEWLINE, NEWLINE,
               "Instructions:", NEWLINE,
               "The game is played by two opponents on a 3 x 3 board.", NEWLINE,
               "Each cell on the board has a number as illustrated", NEWLINE,
               "below. The opponents decide who plays first. To play,", NEWLINE,
               "select a cell number and type it when prompted. The", NEWLINE,
               "selected cell must be empty. For the first player,", NEWLINE,
               "the symbol printed on the selected cell is an 'X';", NEWLINE,
               "for the second player it's an 'O'. A player wins", NEWLINE,
               "when they get three of their symbols in a row, that", NEWLINE,
               "is, either horizontally, vertically, or diagonally.", NEWLINE, NEWLINE,
               getNumberedBoard(), NEWLINE, NEWLINE, NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END displayIntro


# displayGameBoard
#
# Description:
#   Prints the current state of global variable gameBoard to the current connection.
#   The gameBoard data.table is printed in "user-friendly" format for easy reading.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : success
#
# Precondition:
#
# Postcondition:
#   The current state of global variable gameBoard has been delivered
#
displayGameBoard <- function() {

  tryCatch({

    r1 <- paste0(EMPTY, gameBoard[1,1], EMPTY, BOARD_VDIV, EMPTY, gameBoard[1,2], EMPTY, BOARD_VDIV, EMPTY, gameBoard[1,3])
    r2 <- paste0(EMPTY, gameBoard[2,1], EMPTY, BOARD_VDIV, EMPTY, gameBoard[2,2], EMPTY, BOARD_VDIV, EMPTY, gameBoard[2,3])
    r3 <- paste0(EMPTY, gameBoard[3,1], EMPTY, BOARD_VDIV, EMPTY, gameBoard[3,2], EMPTY, BOARD_VDIV, EMPTY, gameBoard[3,3])

    cat(paste0(r1, NEWLINE, BOARD_HDIV, NEWLINE, r2, NEWLINE, BOARD_HDIV, NEWLINE, r3, NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END displayGameBoard


# displayGameState
#
# Description:
#   Prints the current state of the game to the current connection.
#   That is, the numbered board, and the current state of the global
#   variable gameBoard.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : success
#
# Precondition:
#
# Postcondition:
#   The current state of the game has been delivered
#
displayGameState <- function() {

  tryCatch({

    displayGameBoard()
    cat(paste0(NEWLINE, "Cell numbers:", NEWLINE, getNumberedBoard(), NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END displayGameState


# displayWinner
#
# Description:
#   Prints the winner to the current connection.
#
# Arguments:
#   Who won: string or integer
#
# Returns:
#   FALSE: precondition does not hold, or an error occurred
#   TRUE : postcondition holds
#
# Precondition:
#   (whoWon != NULL) && ((whoWon == ME) || (whoWon == THEY))
#
# Postcondition:
#   A message indicating the winner has been delivered
#
displayWinner <- function(whoWon) {

  if (is.null(whoWon) || is.na(whoWon) || !grepl("[0-1]", whoWon) ||
      (nchar(whoWon) > 1) || ((whoWon != ME) && (whoWon != THEY)))
    return(FALSE)

  tryCatch({

    winner <- ifelse(whoWon == THEY, MSG_YOU_WON, MSG_I_WON)
    cat(paste0(NEWLINE, winner, NEWLINE, NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END displayWinner


# displayMyTurn
#
# Description:
#   Displays a message to the user indicating that
#   it's the computer's turn to play.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : success
#
# Precondition:
#
# Postcondition:
#
displayMyTurn <- function() {

  tryCatch({

    cat(paste0(NEWLINE, "My turn...", NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END displayMyTurn


# displayDraw
#
# Description:
#   Displays a message to the user indicating that
#   the game ended in a draw.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : success
#
# Precondition:
#
# Postcondition:
#
displayDraw <- function() {

  tryCatch({

    cat(paste0(NEWLINE, "It's a draw!", NEWLINE, NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END displayDraw


# sayGoodbye
#
# Description:
#   Displays a goodbye message to the user.
#
# Arguments:
#   None.
#
# Returns:
#   FALSE: an error occurred
#   TRUE : success
#
# Precondition:
#
# Postcondition:
#
sayGoodbye <- function() {

  tryCatch({

    cat(paste0(NEWLINE, "Thanks for playing. Bye!", NEWLINE, NEWLINE))

    return(TRUE)

  }, error = {

    # do nothing
  })

  return(FALSE)

}  # END sayGoodbye


# ###### STRATEGIES ######
# takeCorner
#
# Description:
#   Take the first empty corner.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL: precondition does not hold, or an error occurred
#   P0  : all corners were already taken, so no corner taken
#   pos : postcondition holds
#
# Precondition:
#   ((symbol == X) || (symbol == O))
#
# Postcondition:
#   (pos != P0) --> (global variables have been updated.
#                    see: setGameBoardPosValue(...))
#
takeCorner <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  tryCatch({

    while (TRUE) {

      if (length(availableCorners) == 0)
        return(P0)

      pos <- ifelse(length(availableCorners) > 1, sample(availableCorners, 1), availableCorners[1])

      if (!isPosTaken(pos)) {

        setGameBoardPosValue(pos, symbol)
        break()

      }  # END if

    }  # END while

    return(pos)

  }, error = {

    # do nothing
  })

  # at this point, an error occurred
  return(NULL)

}  # END takeCorner


# takeEdge
#
# Description:
#   Take the first empty edge.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL: precondition does not hold, or an error occurred
#   P0  : all edges were already taken, so no edge taken
#   pos : postcondition holds
#
# Precondition:
#   ((symbol == X) || (symbol == O))
#
# Postcondition:
#   (pos != P0) --> (global variables have been updated.
#                    see: setGameBoardPosValue(...))
#
takeEdge <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  tryCatch({

    while (TRUE) {

      if (length(availableEdges) == 0)
        return(P0)

      pos <- ifelse(length(availableEdges) > 1, sample(availableEdges, 1), availableEdges[1])

      if (!isPosTaken(pos)) {

        setGameBoardPosValue(pos, symbol)
        break()

      }  # END if

    }  # END while

    return(pos)

  }, error = {

    # do nothing
  })

  # at this point, an error occurred
  return(NULL)

}  # END takeEdge


# firstMoveWhenX
#
# Description:
#   First move when ME is the first player.
#   Note: this function to be called ONLY
#   once and as the first move.
#
# Arguments:
#   None.
#
# Returns:
#   Value returned by takeCorner()
#
# Precondition:
#
# Postcondition:
#   global variables have been updated.
#   see: setGameBoardPosValue(...))
#
firstMoveWhenX <- function() {

  pos <- takeCorner(X)
  return(pos)

}  # END firstMoveWhenX


# firstMoveWhenO
#
# Description:
#   First move when ME is the second player.
#   Note: this function to be called ONLY
#   once and as the first move.
#
# Arguments:
#   None.
#
# Returns:
#   integer > P0: the postcondition holds
#
# Precondition:
#
# Postcondition:
#   (pos != P0) --> (global variables have been updated.
#                    see: setGameBoardPosValue(...))
#
firstMoveWhenO <- function() {

  pos <- getPosFromSequenceX(1)

  # check for error condition
  if (pos == P0) stop(ERROR_MSG_1)

  # 1. take the center if a corner has been taken
  if (isPosACorner(pos)) {

    setGameBoardPosValue(P5, O)
    return(P5)

  }  # END if

  # 2. take a corner if the center has been taken
  if (isPosTheCenter(pos)) {

    pos <- takeCorner(O)
    return(pos)

  }  # END if

  # 3. at this point, an edge must have been taken.
  # take the center, or a corner next to the X
  PLAY_CENTER <- 1
  PLAY_CORNER <- 2
  play <- sample(c(PLAY_CENTER, PLAY_CORNER), 1)

  if (play == PLAY_CENTER) {

    setGameBoardPosValue(P5, O)
    return(P5)

  }  # END if

  # take an adjacent corner
  if (isPosTaken(P2)) {

    pos <- sample(c(P1, P3), 1)

  } else if (isPosTaken(P4)) {

    pos <- sample(c(P1, P7), 1)

  } else if (isPosTaken(P6)) {

    pos <- sample(c(P3, P9), 1)

  } else {

    pos <- sample(c(P7, P9), 1)

  }  # END if

  setGameBoardPosValue(pos, O)

  return(pos)

}  # END firstMoveWhenO


# secondMoveWhenX
#
# Description:
#   Second move when ME is the first player.
#   Note: this function to be called ONLY
#   once and as the second move.
#
# Arguments:
#   None.
#
# Returns:
#   P0: a move pattern was not detected
#   integer > P0: the position played by ME
#
# Precondition:
#
# Postcondition:
#   (pos != P0) --> (global variables have been updated.
#                    see: setGameBoardPosValue(...))
#
secondMoveWhenX <- function() {

  firstX <- getPosFromSequenceX(1)
  firstO <- getPosFromSequenceO(1)

  # check for error condition
  if (firstX == P0) stop(ERROR_MSG_2)
  if (firstO == P0) stop(ERROR_MSG_3)

  # Pattern 1. ME started with corner and THEY responded with center:
  #    take opposite corner
  if (isPosACorner(firstX) && isPosTheCenter(firstO)) {

    pos <- getOppositeCorner(firstX)

    setGameBoardPosValue(pos, X)
    return(pos)

  }  # END if

  # Pattern 2. ME started with corner and THEY responded with corner:
  #    take a corner
  if (isPosACorner(firstX) && isPosACorner(firstO)) {

    pos <- takeCorner(X)
    return(pos)

  }  # END if

  # Pattern 3. ME started with corner and THEY responded with edge:
  #    take the center
  if (isPosACorner(firstX) && isPosAnEdge(firstO)) {

    setGameBoardPosValue(P5, X)
    return(P5)

  }  # END if

  # unknown pattern
  return(P0)

}  # END secondMoveWhenX


# secondMoveWhenO
#
# Description:
#   Second move when ME is the second player.
#   Note: this function to be called ONLY
#   once and as the second move.
#
# Arguments:
#   None.
#
# Returns:
#   P0: a move pattern to be handled by other functions
#   integer > P0: the position played by ME
#
# Precondition:
#   (pos != P0) --> (global variables have been updated.
#                    see: setGameBoardPosValue(...))
#
# Postcondition:
#
secondMoveWhenO <- function() {

  firstX  <- getPosFromSequenceX(1)
  secondX <- getPosFromSequenceX(2)
  firstO  <- getPosFromSequenceO(1)

  # check for error condition
  if (firstX  == P0) stop(ERROR_MSG_4)
  if (secondX == P0) stop(ERROR_MSG_4)
  if (firstO  == P0) stop(ERROR_MSG_5)

  # 1. THEY started with corner and ME responded with center.
  #    THEY take opposite corner: take an edge
  if (isPosACorner(firstX) && isPosTheCenter(firstO) && (getOppositeCorner(firstX) == secondX)) {

    pos <- takeEdge(O)
    return(pos)

  }  # END if

  # all other possibilities can be handled by
  # the blocking or threat creating functions
  return(P0)

}  # END secondMoveWhenO


# winGame
#
# Description:
#   The computer player, represented by argument symbol,
#   wins the game if on its next move, the game is set
#   to a win state.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: the computer player did not manage to win
#   TRUE : the computer player won the game
#
# Precondition:
#  ((symbol == X) || (symbol == O))
#
# Postcondition:
#   (return value == TRUE) --> global variables have
#   been updated
#
winGame <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  win <- FALSE

  for (elt in INCIDENCE_STRUCT) {

    Xs <- c()
    Os <- c()
    emptys <- c()

    for (pos in elt) {

      value <- getGameBoardPosValue(pos)

      if (value == X) Xs <- c(Xs, pos)
      else if (value == O) Os <- c(Os, pos)
      else emptys <- c(emptys, pos)

    }  # END for

    if (((symbol == X) && (length(Xs) == 2) && (length(emptys) == 1)) ||
        ((symbol == O) && (length(Os) == 2) && (length(emptys) == 1))) {

      win <- TRUE
      setGameBoardPosValue(emptys[1], symbol)
      break()

    }  # END if

  }  # END for

  return(win)

}  # END winGame


# block
#
# Description:
#   The computer player, represented by argument symbol,
#   blocks a potential win from the human opponent.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: the computer player did not block a win move
#   TRUE : the computer player blocked a win move
#
# Precondition:
#  ((symbol == X) || (symbol == O))
#
# Postcondition:
#   (return value == TRUE) --> global variables have
#   been updated
#
block <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  block <- FALSE

  for (elt in INCIDENCE_STRUCT) {

    Xs <- c()
    Os <- c()
    emptys <- c()

    for (pos in elt) {

      value <- getGameBoardPosValue(pos)

      if (value == X) Xs <- c(Xs, pos)
      else if (value == O) Os <- c(Os, pos)
      else emptys <- c(emptys, pos)

    }  # END for

    if (((symbol == X) && (length(Os) == 2) && (length(emptys) == 1)) ||
        ((symbol == O) && (length(Xs) == 2) && (length(emptys) == 1))) {

      block <- TRUE
      setGameBoardPosValue(emptys[1], symbol)
      break()

    }  # END if

  }  # END for

  return(block)

}  # END block


# createThreat
#
# Description:
#   The computer player, represented by argument symbol,
#   creates a threat for the human opponent.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: the computer player did not create a threat
#   TRUE : the computer player created a threat
#
# Precondition:
#  ((symbol == X) || (symbol == O))
#
# Postcondition:
#   (return value == TRUE) --> global variables have
#   been updated
#
createThreat <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  threats <- data.table(pos=as.integer(), freq=as.integer())

  # get a set of available positions, starting with those
  # that offer the greatest number of intersects
  spots <- c()
  if (!isPosTaken(P5)) spots <- c(spots, P5)
  spots <- c(spots, availableCorners, availableEdges)

  createdThreat <- FALSE

  for (spot in spots) {

    # get a copy of the gameBoard in its current state
    gameState <- gameBoard

    # get the table coordinates of the current spot
    coords    <- getPosCoords(spot)

    # insert symbol in the gameBoard copy
    i <- coords[1]
    j <- coords[2]
    gameState[i, j] <- symbol

    # having the gameBoard copy with a new state, determine
    # how many threats spot may create
    intersects <- getIntersects(spot)
    threatFreq <- 0

    for (currIntersect in intersects) {

      symbolFreq <- 0
      emptyFreq  <- 0
      for (pos in currIntersect) {

        posCoords <- getPosCoords(pos)
        x <- posCoords[1]
        y <- posCoords[2]

        if (gameState[x, y, with=FALSE] == symbol) {
          symbolFreq <- (symbolFreq + 1)
        } else if (gameState[x, y, with=FALSE] == EMPTY) {
          emptyFreq <- (emptyFreq + 1)
        }

      }  # END for

      if ((symbolFreq > 1) && (emptyFreq == 1))
        threatFreq <- (threatFreq + 1)

    }  # END for

    # if the spot has created at least one threat, register
    # it the threats control table
    if (threatFreq > 0)
      threats <- rbind(threats, list(pos=spot, freq=threatFreq))

  }  # END for

  # if there is at least one threat, get the pos having the
  # greatest number of them, and update gameBoard
  if (nrow(threats) > 0) {

    maxThreat <- threats[which.max(threats$freq),]
    setGameBoardPosValue(maxThreat$pos, symbol)
    createdThreat <- TRUE

  }  # END if

  return(createdThreat)

}  # END createThreat


# playAnywhere
#
# Description:
#   The computer player, represented by argument symbol, makes a
#   random move, if and only if, there is at least one available
#   position in global variable gameBoard.
#
# Arguments:
#   symbol: character
#
# Returns:
#   NULL : the precondition does not hold
#   FALSE: the computer player did not make a random move
#   TRUE : the computer player made a random move
#
# Precondition:
#  ((symbol == X) || (symbol == O))
#
# Postcondition:
#   (return value == TRUE) --> global variables have
#   been updated
#
playAnywhere <- function(symbol) {

  if (!isSymbolValid(symbol))
    return(NULL)

  isCenterTaken   <- isPosTaken(P5)
  areThereCorners <- (length(availableCorners) > 0)
  areThereEdges   <- (length(availableEdges) > 0)

  if (isCenterTaken && !areThereCorners && !areThereEdges)
    return(FALSE)

  availSpots <- c()
  if (!isCenterTaken) availSpots  <- c(availSpots, P5)
  if (areThereCorners) availSpots <- c(availSpots, availableCorners)
  if (areThereEdges) availSpots   <- c(availSpots, availableEdges)

  pos <- ifelse(length(availSpots) > 1, sample(availSpots, 1), availSpots[1])

  setGameBoardPosValue(pos, symbol)

  return(TRUE)

}  # END playAnywhere


# ###### MAIN ######
# playGame
#
# Description:
#   Game execution driver.
#
# Arguments:
#   None.
#
# Returns:
#   None.
#
# Precondition:
#
# Postcondition:
#
playGame <- function() {

  if (!displayIntro())
    stop(ERROR_MSG_6)

  # used to determine when to display the game state to the user
  gameRepeats <- 0

  repeat {

    theySymbol <- NULL
    meSymbol   <- NULL

    initGame()

    if (is.null(opponentWantsToPlayFirst <- promptConfirm(PROMPT_PLAY_FIRST)) ||
        !initNextPlayer(opponentWantsToPlayFirst == YES))
      stop(ERROR_MSG_6)

    # first and second moves when the opponent plays first (X)
    if (whosNext() == THEY) {

      # display the game state to the user only after the first completed game
      if (gameRepeats > 0) {

        if (!displayGameState())
          stop(ERROR_MSG_6)

      }  # END if

      theySymbol <- X
      meSymbol   <- O

      if (is.null(pos <- promptToPlay()) ||
          !(setGameBoardPosValue(pos, theySymbol) && displayGameBoard()))
        stop(ERROR_MSG_6)

      setNextPlayer()
      displayMyTurn()
      firstMoveWhenO()
      setNextPlayer()

      if (!displayGameState() || is.null(pos <- promptToPlay()) ||
          !setGameBoardPosValue(pos, theySymbol) || !displayGameBoard())
        stop(ERROR_MSG_6)

      setNextPlayer()
      displayMyTurn()

      if (secondMoveWhenO() == P0) {

        if (!block(meSymbol)) {

          if (!createThreat(meSymbol))
            playAnywhere(meSymbol)

        }  # END if

      }  # END if

    } else {

      # first and second moves when the opponent plays second (O)
      meSymbol   <- X
      theySymbol <- O

      displayMyTurn()
      firstMoveWhenX()
      setNextPlayer()

      if (!displayGameState() || is.null(pos <- promptToPlay()) ||
          !setGameBoardPosValue(pos, theySymbol) || !displayGameBoard())
        stop(ERROR_MSG_6)

      setNextPlayer()
      displayMyTurn()

      if (secondMoveWhenX() == P0)
        if (!createThreat(meSymbol)) playAnywhere(meSymbol)

    }  # END if

    if (!displayGameState())
      stop(ERROR_MSG_6)

    setNextPlayer()
    whoWon <- NULL

    # continue game: this is the third move for both players
    while (TRUE) {

      if (whosNext() == THEY) {

        if (is.null(pos <- promptToPlay()) ||
            !(setGameBoardPosValue(pos, theySymbol) && displayGameBoard()))
          stop(ERROR_MSG_6)

        if (isWinFor(theySymbol))
          whoWon <- THEY

      } else {

        displayMyTurn()

        if (winGame(meSymbol)) {
          whoWon <- ME

        } else {

          if (!block(meSymbol)) {

            if (!createThreat(meSymbol))
              playAnywhere(meSymbol)

          }  # END if

        }  # END if

        if (!displayGameState())
          stop(ERROR_MSG_6)

      }  # END if

      # determine if there is a winner
      if (!is.null(whoWon)) {

        displayWinner(whoWon)
        break()

      } else {

        if (isGameBoardFull()) {

          displayDraw()
          break()

        }  # END if

      }  # END if

      # at this point, no one has yet won. set the next player
      # in order to continue with the game
      setNextPlayer()

    }  # END while

    # at this point, the game has concluded. ask the human opponent
    # if they would like to play again
    if (is.null(playAgain <- promptConfirm(PROMPT_PLAY_AGAIN)))
      stop(ERROR_MSG_6)

    if (playAgain == NO)
      break()

    gameRepeats <- (gameRepeats + 1)

  }  # END repeat

  # set function's return value to variable x, in order to avoid
  # displaying the value on screen
  x <- sayGoodbye()

}  # END playGame


# ###### EXECUTE ######
playGame()
