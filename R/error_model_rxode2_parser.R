
a <- rxode2::rxResidualError

getErrorModelTokens <- function() {
  retValue <- c(
    'ADD',
    'PROP',
    'COMBINED1',
    'COMBINED2'
  )
  return(retValue)
}

#' Rxode2 error model lexer.
#' 
#' @field tokens list of tokens
#' @field debug logical value to enable/disable debug messages
#' @field t_ignore function to ignore whitespace
#' @description Rxode2ErrorModelLexer class to handle the lexer for Rxode2 error model
#' @return a lexer object
#' @export
Rxode2ErrorModelLexer <- R6::R6Class(
  classname = "Lexer",
  public = list(
    #' @description tokens list of tokens
    tokens = getErrorModelTokens(),
    
    #' @description debug logical value to enable/disable debug messages
    debug = TRUE,
    
    #' @description t_ADD function to handle ADD token
    #' @param re regular expression to match ADD token
    #' @param t token object
    t_ADD = function(re='add\\s*\\([^\\)]*\\)', t) {
      if (self$debug) message("Found add: ", t$value)
      return(t)
    },
    
    #' @description t_PROP function to handle PROP token
    #' @param re regular expression to match PROP token
    #' @param t token object
    t_PROP = function(re='prop\\s*\\([^\\)]*\\)', t) {
      if (self$debug) message("Found prop: ", t$value)
      return(t)
    },
    
    #' @description t_COMBINED1 function to handle COMBINED1 token
    #' @param re regular expression to match COMBINED1 token
    #' @param t token object
    t_COMBINED1 = function(re='combined1\\s*\\(\\s*\\)', t) {
      if (self$debug) message("Found combined1: ", t$value)
      return(t)
    },
    
    #' @description t_COMBINED2 function to handle COMBINED2 token
    #' @param re regular expression to match COMBINED2 token
    #' @param t token object
    t_COMBINED2 = function(re='combined2\\s*\\(\\s*\\)', t) {
      if (self$debug) message("Found combined2: ", t$value)
      return(t)
    },
    
    #' @description t_ignore function to ignore whitespace
    t_ignore = "\t \\+",
    
    #' @description t_newline function to handle newlines
    #' @param re regular expression to match newlines
    #' @param t token object
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    
    #' @description t_error function to handle errors
    #' @param t token object
    t_error = function(t) {
      t$lexer$skip(1)
      return(t)
    }
  )
)

#' Rxode2 error model parser.
#' 
#' @field tokens list of tokens
#' @description Rxode2ErrorModelParser class to handle the parser for Rxode2 error model
#' @return a parser object
#' @export
Rxode2ErrorModelParser <- R6::R6Class(
  classname = "Parser",
  public = list(
    #' @description tokens list of tokens
    tokens = getErrorModelTokens(),
    
    #' @description function to handle merged error model
    #' @param doc doc argument
    #' @param p parser object
    p_merged_error_model = function(doc='merged_error_model : error_model
                         | merged_error_model error_model', p) {
      if (p$length() == 2) {
        p$set(1, p$get(2))
      } else {
        errorModel1 <- p$get(2)
        errorModel2 <- p$get(3)
        retValue <- Rxode2ErrorModel()
        retValue@add <- c(errorModel1@add, errorModel2@add)
        retValue@prop <- c(errorModel1@prop, errorModel2@prop)
        retValue@combined1 <- any(c(errorModel1@combined1, errorModel2@combined1))
        retValue@combined2 <- any(c(errorModel1@combined2, errorModel2@combined2))
        retValue@endpoint <- c(errorModel1@endpoint, errorModel2@endpoint)
        p$set(1, retValue)
      }
    },
    
    #' @description function to handle error model
    #' @param doc doc argument
    #' @param p parser object
    p_error_model = function(doc='error_model : add
                         | prop
                         | combined1
                         | combined2', p) {
      p$set(1, p$get(2))
    },
    
    #' @description function to handle add
    #' @param doc doc argument
    #' @param p parser object
    p_add = function(doc='add : ADD', p) {
      variable <- trimws(extractValueInParentheses(p$get(2)))
      errorModel <- Rxode2ErrorModel(add=variable)
      p$set(1, errorModel)
    },
    
    #' @description function to handle prop
    #' @param doc doc argument
    #' @param p parser object
    p_prop = function(doc='prop : PROP', p) {
      variable <- trimws(extractValueInParentheses(p$get(2)))
      errorModel <- Rxode2ErrorModel(prop=variable)
      p$set(1, errorModel)
    },
    
    #' @description function to handle combined1
    #' @param doc doc argument
    #' @param p parser object
    p_combined1 = function(doc='combined1 : COMBINED1', p) {
      errorModel <- Rxode2ErrorModel(combined1=TRUE)
      p$set(1, errorModel)
    },
    
    #' @description function to handle combined2
    #' @param doc doc argument
    #' @param p parser object
    p_combined2 = function(doc='combined2 : COMBINED2', p) {
      errorModel <- Rxode2ErrorModel(combined2=TRUE)
      p$set(1, errorModel)
    },

    #' @description function to handle errors
    #' @param doc doc argument
    #' @param p parser object
    p_error = function(p) {
      #browser()
      if(is.null(p)) print("Syntax error at EOF")
      else           print(sprintf("Syntax error at '%s'", p$value))
    }
  )
)
