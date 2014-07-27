## makeCacheMatrix takes an input matrix and gets and sets
## the matrix and it's inverse
## the cacheSolve checks to see if an inverse has already been
## solved and if so returns the previously solved for solution
## if not it solves for it and then writes the inverse matrix
## to the makeCacheMatrix

## Write a short comment describing this function

makeCacheMatrix <- function(origMatrix = matrix())
{
##Function input is a matrix
##Output is a series of functions
##Get and set input matrix
	
	inverseMatrix <- NULL
	
	setOrigMatrix <- function(funcInput)
	{
		origMatrix <<- funcInput
		inverseMatrix <<- NULL
	}
	
	getOrigMatrix <- function()
	{
		origMatrix
	}
	
	setInverseMatrix <- function(invertedMatrixOfOrigMatrix)
	{
		inverseMatrix <<- invertedMatrixOfOrigMatrix
	}
	
	getInverseMatrix <- function()
	{
		inverseMatrix
	}
	
	list(setOrigMatrix = setOrigMatrix, getOrigMatrix = getOrigMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(inputList, inputMatrix)
{
##function inputs are list of functions and an input matrix
##if the input matrix is the same  as the cached matrix and
##the inverse matrix of the cached matrix has been previously solved
##then it returns the cached matrix else it solves and returns the 
##inverse matrix and caches it
	invertedMatrix <- inputList$getInverseMatrix()
	matrixToInvert <- inputList$getOrigMatrix()
	
	if(!is.null(invertedMatrix) && identical(matrixToInvert, inputMatrix))
	{
		message("getting cached data")
		return(invertedMatrix)
	}
	else
	{
		matrixToInvert <- inputList$getOrigMatrix()
		output <- solve(matrixToInvert)
		inputList$setInverseMatrix(output)
		return(output)
	}
}
