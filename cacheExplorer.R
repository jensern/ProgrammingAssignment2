## makeCacheMatrix() is a function that accepts an INPUT argument as a Matrix
## it then generates 4 functions called SET(), GET(), SETINV(), GETINV()
## it RETURNS a list which contain factors that point to these 4 functions
## it automatically stores the matrix value as argument x, and is accessible with the name $GET()
## the initial memory state for the inverse value stored in variable inv_mat is NULL
## SET() and SETINV() utilizes a special operator '<<-' to re-assign value to variable inv_mat declared in a different environment

makeCacheMatrix <- function(x = matrix()) {

        ##initialize carrier variable for matrix inverse to NULL in primary environment. Note assignment operator "<-"
        inv_mat<-NULL
        
        ## Define function set for SET, GET, SET Inverse, GET Inverse
        ##*************************************************************
        ## SET() applies the "<<-" operator to re-write matrix definition, x and modifies(clears) any stored inverse value
        
        SET<-function(y){
                x<<-y
                inv_mat<<-NULL
        }
        
        ## GET() retrieves the value of 'x' which is the matrix passed into the main function as an argument
        
        GET<-function(){
                x
        }
        
        ## SETINV() applies the <<- operator to re-assign the value of inv_mat which is declared 1 level above this function
        
        SETINV<-function(calculated_inverse){
                inv_mat<<-calculated_inverse
        }
        
        ## GETINV() retrieves the value of 'inv_mat' which supposedly stores the inverse of the matrix passed as argument x
        
        GETINV<-function(){
                inv_mat
        }
        
        ## List() function defines the name/factor - value assignment. In this case we assign factors to the previously called functions SET, GET, SETINV, GETINV
        list(SET=SET,GET=GET,SETINV=SETINV,GETINV=GETINV)
        
}


## cacheSolve() accepts INPUT that contains the factors $GETINV(), $GET(), $SETINV()
## $GET INV(), $GET(), $SETINV() are factors for functions defined in makeCacheMatrix()
## Therefore makeCacheMatrix needs to be initialized before cacheSolve can be executed
## cacheSolve() first checks for any existing value stored in the variable inv_mat defined in makeCacheMatrix() using $GETINV()
### IF a value is present, it RETURNs that value and breaks the function
### ELSE it uses  $GET() to retrieve the matrix value, computes the Inverse using solve(), and then assigns the value of inv_mat using $SETINV(), and further RETURNs the value inv_mat

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv_mat<-x$GETINV()
        if(!is.null(inv_mat)){
                message("Please hold on while we retrieve the Inverse Matrix results from Cache")
                return(inv_mat)
        }
        
        else{
                matrix_value<-x$GET()
                inv_mat<-solve(matrix_value)
                x$SETINV(inv_mat)
                inv_mat
        }
        
}


