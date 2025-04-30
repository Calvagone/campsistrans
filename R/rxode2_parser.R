
getParserTokens <- function() {
  retValue <- c(
    'EQUATION',
    'ODE',
    'COMMENT',
    'LINE_BREAK',
    'IF_SINGLE_LINE',
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

buildIfStatement <- function(condition, content, type) {
  condition <- removeBracketsAndTrim(condition)
  content <- removeBracketsAndTrim(content)
  lhs <- extractLhs(content) %>% trimws()
  rhs <- extractRhs(content) %>% trimws()
  equation <- Equation(lhs=lhs, rhs=getExpr(rhs), comment=getComment(rhs))
  if (type == "if") {
    retValue <- new("if_statement", condition=condition, equation=equation, comment=as.character(NA))
  } else if (type == "else if") {
    retValue <- new("else_if_statement", condition=condition, equation=equation, comment=as.character(NA))
  } else if (type == "else") {
    retValue <- new("else_statement", condition=condition, equation=equation, comment=as.character(NA))
  }
  return(retValue)
}

#' Rxode2 lexer.
#' 
#' @export
Rxode2Lexer <- R6::R6Class(
  classname = "Lexer",
  public = list(
    tokens = getParserTokens(),
    debug = TRUE,
    t_EQUATION = function(re='[a-zA-Z_][a-zA-Z0-9_]*\\s*=\\s*[^\n]+', t) {
      if (self$debug) message("Found equation: ", t$value)
      return(t)
    },
    t_ODE = function(re='d/dt\\s*\\(.*\\)\\s*=\\s*[^\n]+', t) {
      if (self$debug) message("Found ODE: ", t$value)
      return(t)
    },
    t_LINE_BREAK = function(re='\n[ ]*\n', t) {
      if (self$debug) message("Found line break: ", t$value)
      return(t)
    },
    t_COMMENT = function(re='\\s*#.*[^\n]+', t) {
      if (self$debug) message("Found comment: ", t$value)
      return(t)
    },
    t_IF_SINGLE_LINE = function(re='if\\s*\\((?>[^()]|(?R))*\\).*[^\n]+', t) {
      if (self$debug) message("Found if single line: ", t$value)
      return(t)
    },
    t_IF = function(re='if', t) {
      if (self$debug) message("Found if: ", t$value)
      return(t)
    },
    t_ELSE_IF = function(re='else\\s+if', t) {
      if (self$debug) message("Found else-if: ", t$value)
      return(t)
    },
    t_ELSE = function(re='else', t) {
      if (self$debug) message("Found else: ", t$value)
      return(t)
    },
    t_CONDITION = function(re='\\((?>[^()]|(?R))*\\)', t) {
      if (self$debug) message("Found condition: ", t$value)
      return(t)
    },
    t_IF_CONTENT = function(re='\\{(?>[^{}]|(?R))*\\}', t) {
      if (self$debug) message("Found if content: ", t$value)
      return(t)
    },
    t_UNKNOWN_STATEMENT = function(re='.*[^\n]+', t) {
      if (self$debug) message("Found unknown statement: ", t$value)
      return(t)
    },
    t_ignore = "\t ",
    t_newline = function(re='\\n+', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      return(NULL)
    },
    t_error = function(t) {
      t$lexer$skip(1)
      return(t)
    }
  )
)

#' Rxode2 parser.
#' 
#' @export
Rxode2Parser <- R6::R6Class(
  classname = "Parser",
  public = list(
    tokens = getParserTokens(),
    
    p_model_statements = function(doc='model_statements : model_statement
                         | model_statements model_statement', p) {
      if (p$length() == 2) {
        p$set(1, p$get(2))
      } else {
        p$set(1, append(p$get(2), p$get(3)))
      }
    },
    
    p_model_statement = function(doc='model_statement : complex_if_statement
                         | equation 
                         | ode
                         | line_break
                         | comment
                         | if_single_line
                         | unknown_statement', p) {
        p$set(1, list(p$get(2)))
    },
    
    p_complex_if_statement = function(doc='complex_if_statement : if_statement
                         | complex_if_statement if_statement', p) {
      if (p$length() == 2) {
        ifStatement <- p$get(2)
        complexIfStatement <- ComplexIfStatement()
        complexIfStatement@list <- list(ifStatement)
        p$set(1, complexIfStatement)
      } else {
        complexIfStatement <- p$get(2)
        complexIfStatement@list <- append(complexIfStatement@list, list(p$get(3)))
        p$set(1, complexIfStatement)
      }
    },
    
    p_if_statement = function(doc='if_statement : IF CONDITION IF_CONTENT
                            | ELSE_IF CONDITION IF_CONTENT
                            | ELSE IF_CONTENT', p) {
      if (p$get(2) == 'if') {
        p$set(1, buildIfStatement(condition=p$get(3), content=p$get(4), type="if"))
      } else if (p$get(2) == 'else if') {
        p$set(1, buildIfStatement(condition=p$get(3), content=p$get(4), type="else if"))
      } else if (p$get(2) == 'else') {
        p$set(1, buildIfStatement(condition="()", content=p$get(3), type="else"))
      }
    },
    
    p_equation = function(doc='equation : EQUATION', p) {
      lhs <- extractLhs(p$get(2)) %>% trimws()
      rhs <- extractRhs(p$get(2)) %>% trimws()
      equation <- Equation(lhs=lhs, rhs=getExpr(rhs), comment=getComment(rhs))
      p$set(1, equation)
    },
    
    p_ode = function(doc='ode : ODE', p) {
      lhs <- extractLhs(p$get(2)) %>% trimws()
      rhs <- extractRhs(p$get(2)) %>% trimws()
      ode <- Ode(lhs=sprintf("%s", extractTextBetweenBrackets(lhs)), rhs=getExpr(rhs), comment=getComment(rhs))
      p$set(1, ode)
    },
    
    p_line_break = function(doc='line_break : LINE_BREAK', p) {
      p$set(1, LineBreak())
    },
    
    p_comment = function(doc='comment : COMMENT', p) {
      p$set(1, Comment(getComment(p$get(2))))
    },
    
    p_if_single_line = function(doc='if_single_line : IF_SINGLE_LINE', p) {
      ifStatement <- campsismod:::parseIfStatement(getExpr(p$get(2)), comment=getComment(p$get(2)))
      p$set(1, ifStatement)
    },
    
    p_unknown_statement = function(doc='unknown_statement : UNKNOWN_STATEMENT', p) {
      p$set(1, UnknownStatement(line=getExpr(p$get(2)), comment=getComment(p$get(2))))
    },
    
    p_error = function(p) {
      #browser()
      if(is.null(p)) print("Syntax error at EOF")
      else           print(sprintf("Syntax error at '%s'", p$value))
    }
  )
)
