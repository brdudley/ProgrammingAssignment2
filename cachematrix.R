## The functions in the file implement a matrix object that can cache its
## computed inverse and a wrapper of the built-in solve to leverage the 
## cached value.
##
## Computing a matrix inverse can be a costly operation. These functions 
## can provide efficiency by allowing the reuse of the inverse in repeated
## calculations.



## The makeCacheMatrix returns a list object that encapsulates a matrix which 
## can cache its inverse, and methods to access both of them.

makeCacheMatrix <- function(x = matrix()) {
        ## Check the argument for validity. It must be a square, numeric or 
        ## complex matrix.
        if(class(x) != "matrix")
                stop("Expecting a matrix.")
        if(class(x[1]) != "integer" && class(x[1]) != "numeric"
           && class(x[1]) != "complex" )
                stop("Matrix must be numeric or complex.")
        if(ncol(x)==0 || ncol(x)!=nrow(x))
                stop("Matrix must be square.")
        
        ## Variables to store the inverse and the arguments used to 
        ## compute it.
        inverse<-NULL
        inverseArgs<-NULL
        
        ## Define functions to manipulate the matrix.
        
        ## Set will modify the matrix and clear cached values.
        ## Clearing the cached value when a new matrix is set ensures
        ## that the cached inverse goes with the proper matrix.
        set<-function(y){
                ## Clear the cache and arguments.
                inverse<<-NULL
                inverseArgs<-NULL
                
                ## Store the new matrix in the enclosing environment.
                x<<-as.matrix(y)
        }
        
        ## Get returns the stored matrix.
        get<-function(){
                return(x)
        }
        
        ## setSolve caches the value of the solve function for the 
        ## matrix and the arguments used.
        setSolve<-function(y,arglist){
                ## Save the arguments in the enclosing environment.
                inverseArgs<<-arglist
                
                ## Save the inverse in the enclosing environment.
                inverse<<-y
        }
        
        ## getSolve returns the cached matrix inverse if applicable or NULL.
        ## The matrix inverse must have been previously computed with the 
        ## same arguments on the currently stored matrix. 
        getSolve<-function(arglist=getArgs()){
                ## Ensure that any stored inverse used the same arguments.
                if(!is.null(inverseArgs) && identical(inverseArgs,arglist))
                        inverse
                else
                        NULL
        }
        
        ## getArgs returns the arguments for the cached inverse. It
        ## is a convenience method.
        getArgs<-function(){
                return(inverseArgs)
        }
        
        ## Return the matrix object as a list including access functions.
        ## Values are stored in the enclosing environment.
        list(   set=set, 
                get=get,
                setSolve=setSolve,
                getSolve=getSolve,
                getArgs=getArgs)
}



## This function returns the inverse of a CacheMatrix object by
## either getting the cached value or computing the value and saving
## it in the cache.
##
## The cached inverse must use the same arguments.

cacheSolve <- function(x, ...) {
        ## Check the primary parameter to ensure it is
        ## a CacheMatrix identified by a list with certain functions.
        if( class(x) != "list")
                stop( "Invalid class for parameter x:", class(x))

        ## Must have the functions used in this function.
        if( is.null(x$getSolve) || is.null(x$setSolve) || is.null(x$get) )
                stop( "Parameter x is not a CacheMatrix.")

        ## Grab the call and its arguments. This will help
        ## ensure the cached value is appropriate.
        call<-match.call()

        ## Return a matrix that is the inverse of 'x'

        ## Try to get the cached inverse.
        inverse<-x$getSolve(call)
        
        ## When the cached value is NULL, compute and store the inverse.
        if(is.null(inverse)){
                inverse<-solve(x$get(),...)
                x$setSolve(inverse,call)
        }else{
                ## Inform the user that the cache is being used.
                print( "Returning cached inverse.")
        }
        
        # Return the inverse/inverse.
        inverse
}
