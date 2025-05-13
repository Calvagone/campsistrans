
getParserTokens <- function() {
  retValue <- c(
    'EQUATION',
    'ODE',
    'COMMENT',
    'LINE_BREAK',
    # 'IF_SINGLE_LINE',
    'IF',
    'ELSE_IF',
    'ELSE',
    'CONDITION',
    'IF_CONTENT',
    'UNKNOWN_STATEMENT'
  )
  return(retValue)
}

getComment <- function(x) {
  hasComment <- hasComment(x) 
  comment <- as.character(NA)
  if (hasComment) {
    comment <- extractRhs(x, split="#") %>% trimws()
  }
  return(comment)
}

getExpr <- function(x) {
  hasComment <- hasComment(x) 
  if (hasComment) {
    return(extractLhs(x, split="#") %>% trimws())
  } else {
    return(x) 
  }
}

removeBracketsAndTrim <- function(x) {
  x <- substr(x, 2, nchar(x) - 1)
  x <- trimws(x)
  return(x)
}

#' Rxode2 lexer.
#' 
#' @field tokens list of tokens
#' @field debug logical value to enable/disable debug messages
#' @field t_EQUATION function to handle EQUATION token
#' @field t_ODE function to handle ODE token
#' @field t_LINE_BREAK function to handle LINE_BREAK token
#' @field t_COMMENT function to handle COMMENT token
#' @field t_IF_SINGLE_LINE function to handle IF_SINGLE_LINE token
#' @field t_IF function to handle IF token
#' @field t_ELSE_IF function to handle ELSE_IF token
#' @field t_ELSE function to handle ELSE token
#' @field t_CONDITION function to handle CONDITION token
#' @field t_IF_CONTENT function to handle IF_CONTENT token
#' @field t_UNKNOWN_STATEMENT function to handle UNKNOWN_STATEMENT token
#' @field t_ignore function to ignore whitespace
#' @field t_newline function to handle newlines
#' @field t_error function to handle errors
#' @description Rxode2Lexer class to handle the lexer for RxODE2  
#' @return a lexer object
#' @export
Rxode2Lexer <- R6::R6Class(
  classname = "Lexer",
  public = list(
    #' @description tokens list of tokens
    tokens = getParserTokens(),
    
    #' @description debug logical value to enable/disable debug messages
    debug = TRUE,
    
    #' @description t_EQUATION function to handle EQUATION token
    #' @param re regular expression to match EQUATION token
    #' @param t token object
    t_EQUATION = function(re='[a-zA-Z_][a-zA-Z0-9_\\.]*\\s*=\\s*[^\n]+', t) {
      # Not that the dot is accepted for the nonmem2rx importer to work properly
      if (self$debug) message("Found equation: ", t$value)
      return(t)
    },
    
    #' @description t_ODE function to handle ODE token
    #' @param re regular expression to match ODE token
    #' @param t token object
    #' 
    t_ODE = function(re='d/dt\\s*\\(.*\\)\\s*=\\s*[^\n]+', t) {
      if (self$debug) message("Found ODE: ", t$value)
      return(t)
    },
    
    #' @description t_LINE_BREAK function to handle LINE_BREAK token
    #' @param re regular expression to match LINE_BREAK token
    #' @param t token object
    t_LINE_BREAK = function(re='\n[ ]*\n', t) {
      if (self$debug) message("Found line break: ", t$value)
      return(t)
    },
    
    #' @description t_COMMENT function to handle COMMENT token
    #' @param re regular expression to match COMMENT token
    #' @param t token object
    t_COMMENT = function(re='\\s*#.*[^\n]+', t) {
      if (self$debug) message("Found comment: ", t$value)
      return(t)
    },

    #' @description t_IF function to handle IF token
    #' @param re regular expression to match IF token
    #' @param t token object
    t_IF = function(re='if', t) {
      if (self$debug) message("Found if: ", t$value)
      return(t)
    },
    
    #' @description t_ELSE_IF function to handle ELSE_IF token
    #' @param re regular expression to match ELSE_IF token
    #' @param t token object
    t_ELSE_IF = function(re='else\\s+if', t) {
      if (self$debug) message("Found else-if: ", t$value)
      return(t)
    },
    
    #' @description t_ELSE function to handle ELSE token
    #' @param re regular expression to match ELSE token
    #' @param t token object
    t_ELSE = function(re='else', t) {
      if (self$debug) message("Found else: ", t$value)
      return(t)
    },
    
    #' @description t_CONDITION function to handle CONDITION token
    #' @param re regular expression to match CONDITION token
    #' @param t token object
    t_CONDITION = function(re='\\((?>[^()]|(?R))*\\)', t) {
      if (self$debug) message("Found condition: ", t$value)
      return(t)
    },
    
    #' @description t_IF_CONTENT function to handle IF_CONTENT token
    #' @param re regular expression to match IF_CONTENT token
    #' @param t token object
    t_IF_CONTENT = function(re='\\{(?>[^{}]|(?R))*\\}', t) {
      if (self$debug) message("Found if content: ", t$value)
      return(t)
    },
    
    #' @description t_UNKNOWN_STATEMENT function to handle UNKNOWN_STATEMENT token
    #' @param re regular expression to match UNKNOWN_STATEMENT token
    #' @param t token object
    t_UNKNOWN_STATEMENT = function(re='.*[^\n]+', t) {
      if (self$debug) message("Found unknown statement: ", t$value)
      return(t)
    },
    #' @description t_ignore function to ignore whitespace
    t_ignore = "\t ",
    
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

#' Rxode2 parser.
#' 
#' @field tokens list of tokens
#' @field p_model_statements function to handle model_statements
#' @field p_model_statement function to handle model_statement
#' @field p_complex_if_else_statement function to handle complex_if_else_statement
#' @field p_simple_if_statement function to handle simple_if_statement
#' @field p_if_statement function to handle if_statement
#' @field p_equation function to handle equation
#' @field p_ode function to handle ode
#' @field p_line_break function to handle line_break
#' @field p_comment function to handle comment
#' @field p_unknown_statement function to handle unknown_statement
#' @field p_error function to handle errors
#' @description Rxode2Parser class to handle the parser for RxODE2
#' @return a parser object
#' @export
Rxode2Parser <- R6::R6Class(
  classname = "Parser",
  public = list(
    #' @description tokens list of tokens
    tokens = getParserTokens(),
    
    #' @description function to handle model_statements
    #' @param doc doc argument
    #' @param p parser object
    p_model_statements = function(doc='model_statements : model_statement
                         | model_statements model_statement', p) {
      if (p$length() == 2) {
        p$set(1, p$get(2))
      } else {
        p$set(1, append(p$get(2), p$get(3)))
      }
    },
    
    #' @description function to handle model_statement
    #' @param doc doc argument
    #' @param p parser object
    p_model_statement = function(doc='model_statement : complex_if_else_statement
                         | equation 
                         | ode
                         | line_break
                         | comment
                         | simple_if_statement
                         | unknown_statement', p) {
        p$set(1, list(p$get(2)))
    },
    
    #' @description function to handle complex_if_else_statement
    #' @param doc doc argument
    #' @param p parser object
    p_complex_if_else_statement = function(doc='complex_if_else_statement : if_statement
                         | complex_if_else_statement if_statement', p) {
      if (p$length() == 2) {
        ifStatement <- p$get(2)
        complexIfElseStatement <- ComplexIfElseStatement()
        complexIfElseStatement@list <- list(ifStatement)
        p$set(1, complexIfElseStatement)
      } else {
        complexIfElseStatement <- p$get(2)
        complexIfElseStatement@list <- append(complexIfElseStatement@list, list(p$get(3)))
        p$set(1, complexIfElseStatement)
      }
    },
    
    #' @description function to handle simple_if_statement
    #' @param doc doc argument
    #' @param p parser object
    p_simple_if_statement = function(doc='simple_if_statement : IF CONDITION EQUATION', p) {
      p$set(1, buildIfStatement(condition=p$get(3), content=paste0("{", p$get(4), "}"), type="simple_if"))
    },
    
    #' @description function to handle if_statement
    #' @param doc doc argument
    #' @param p parser object
    p_if_statement = function(doc='if_statement : IF CONDITION EQUATION
                            | IF CONDITION IF_CONTENT
                            | ELSE_IF CONDITION IF_CONTENT
                            | ELSE IF_CONTENT', p) {
      if (p$get(2) == 'if') {
        if (startsWith(trimws(p$get(4)), "{")) {
          p$set(1, buildIfStatement(condition=p$get(3), content=p$get(4), type="if"))
        } else {
          p$set(1, buildIfStatement(condition=p$get(3), content=paste0("{", p$get(4), "}"), type="if"))
        }
      } else if (p$get(2) == 'else if') {
        p$set(1, buildIfStatement(condition=p$get(3), content=p$get(4), type="else if"))
      } else if (p$get(2) == 'else') {
        p$set(1, buildIfStatement(condition="()", content=p$get(3), type="else"))
      }
    },
    
    #' @description function to handle equation
    #' @param doc doc argument
    #' @param p parser object
    p_equation = function(doc='equation : EQUATION', p) {
      lhs <- extractLhs(p$get(2)) %>% trimws()
      rhs <- extractRhs(p$get(2)) %>% trimws()
      equation <- Equation(lhs=lhs, rhs=getExpr(rhs), comment=getComment(rhs))
      p$set(1, equation)
    },
    
    #' @description function to handle ode
    #' @param doc doc argument
    #' @param p parser object
    p_ode = function(doc='ode : ODE', p) {
      lhs <- extractLhs(p$get(2)) %>% trimws()
      rhs <- extractRhs(p$get(2)) %>% trimws()
      ode <- Ode(lhs=sprintf("%s", extractTextBetweenBrackets(lhs)), rhs=getExpr(rhs), comment=getComment(rhs))
      p$set(1, ode)
    },
    
    #' @description function to handle line_break
    #' @param doc doc argument
    #' @param p parser object
    p_line_break = function(doc='line_break : LINE_BREAK', p) {
      p$set(1, LineBreak())
    },
    
    #' @description function to handle comment
    #' @param doc doc argument
    #' @param p parser object
    p_comment = function(doc='comment : COMMENT', p) {
      p$set(1, Comment(getComment(p$get(2))))
    },

    #' @description function to handle unknown_statement
    #' @param doc doc argument
    #' @param p parser object
    p_unknown_statement = function(doc='unknown_statement : UNKNOWN_STATEMENT', p) {
      p$set(1, UnknownStatement(line=getExpr(p$get(2)), comment=getComment(p$get(2))))
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

buildIfStatement <- function(condition, content, type) {
  condition <- removeBracketsAndTrim(condition)
  content <- removeBracketsAndTrim(content)

  if (type == "simple_if") {
    lhs <- extractLhs(content) %>% trimws()
    rhs <- extractRhs(content) %>% trimws()
    equation <- Equation(lhs=lhs, rhs=getExpr(rhs), comment=getComment(rhs))
    retValue <- new("if_statement", condition=condition, equation=equation, comment=as.character(NA))
  } else {
    lexer  <- rly::lex(Rxode2Lexer)
    parser <- rly::yacc(Rxode2Parser)
    
    statements <- ModelStatements()
    statements@list <- parser$parse(content, lexer)
    
    if (type == "if") {
      retValue <- new("extended_if_statement", condition=condition, statements=statements)
    } else if (type == "else if") {
      retValue <- new("else_if_statement", condition=condition, statements=statements)
    } else if (type == "else") {
      retValue <- new("else_statement", condition=condition, statements=statements)
    } else {
      stop("Unknown type of statement")
    }
  } 
  return(retValue)
}
