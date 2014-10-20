


#' set variables and knitr hooks for dealing with homeworks
#' 
#' This should be called in an introductory R block something like this:
#' which currently is not shown well.  Have to figure out
#' how to do displayed code with roxygen.
#' 
#' \code{```{r setup, echo=FALSE, include=FALSE}}
#' 
#' \code{rrhw::init_homework("Homework Set 3")}
#' 
#' \code{```}
#' 
#' in an R code block at the 
#' top of the Rmd file you are using this in.
#' @param homework_name The name you are giving to the homework set 
#' @export
init_homework <- function(homework_name = "Unset") {
  
  # set some necessary variables in the GlobalEnv
  assign("rr_homework_name", homework_name, envir = .GlobalEnv)
  assign("rr_question_number", 0, envir = .GlobalEnv)
  assign("rr_in_rr_question_block", FALSE, envir = .GlobalEnv)
  
  # create an environment that we will be able to write results to from submit_answer
  assign("rrhw_env", new.env(parent = emptyenv()), .GlobalEnv)
  
  # now set the hooks that we will want

  # this is the tag for any block that has a question in it. It makes a subheading right above
  # it and increments counters and makes chunk names available.
  knit_hooks$set(rr.question = function(before, options, envir) {
    if (before) {
      ## code to be run before a chunk
      if(grepl("^unnamed-chunk", options$label)[1]) stop("You have to name the chunk if it is an rr.question chunk")
      assign("rr_question_chunk_name", options$label, envir = .GlobalEnv)
      assign("rr_question_number", rr_question_number + 1, envir = .GlobalEnv)
      assign("rr_in_rr_question_block", TRUE, envir = .GlobalEnv) # tells us if we are in an rr.question block
      return(
        paste(
          "\n\n### ", 
          rr_homework_name[1], ", #", 
          rr_question_number, ": \"" , rr_question_chunk_name, "\"   ", 
          "{#", rr_question_chunk_name, "}", sep="" ))
    } else {
      assign("rr_in_rr_question_block", FALSE, envir = .GlobalEnv) # when we leave, set this back to 0
    }
  })
  
}



#' workhorse function that processes homeworks
#' 
#' This function is something that the students submit their 
#' answers in, i.e. they write their answers as a (possibly compound)
#' expression which is an argument to this function. Note that you 
#' must put the curly braces around everthing.
#' @export
submit_answer <- function(x) {
  
  ### I commented this out because it causes problems with "purling"
  # stop if this is in a chunk that does not have option rr.question==TRUE
  #if(!rr_in_rr_question_block) {
  #  stop("submit_answer can only be used in a code block with option rr.question==TRUE.  ")
  #}
  
  
  ret <- list()
  ret$S_github <- rr_github_name # the S is for student
  ret$S_pull_request_time <- rr_pull_request_time 
  ret$H_name <- rr_homework_name  # the H is for homework
  ret$Q_name <- rr_question_chunk_name
  ret$Q_number <- rr_question_number
  ret$Q_value <- eval.parent(substitute(x))
  y <- deparse(substitute(x))
  y <- y[-c(1,length(y))] # pull off the curly braces
  ret$Q_expr_str <- paste(y, collapse="\n")
  
  # now put that in the rrhw environment
  assign(rr_question_chunk_name, ret, envir = rrhw_env)
  
  # and return the value
  if(!is.null(ret$Q_value)) {
    return(ret$Q_value)
  } else {
    return(invisible(NULL))
  }
  
  
}



#' convert a list like that assigned from submit_answer to a single row of a data frame
#' 
#' There is some nonstandard stuff here because the Q_value can be
#' a vector, so I make it a list and use I().
#' @export
answer_list_to_data_frame <- function(x) {
  val <- which(names(x)=="Q_value")
  y <- data.frame(x[-val], stringsAsFactors = FALSE) # make a data frame without the value field
  z <- cbind(y, Q_value = I(x[val]))
  z
}

#' find directory level which has the file top_file
#' 
#' This will go up 0, 1, 2, 3, 4, and 5, directories looking
#' for the file top_file.  It returns the least high up directory
#' as a relative path (like "../../").  This is useful for
#' finding where the .Rproj file is so you can define paths relative
#' to that when inserting child documents into knitr docs, etc.
#' Throws an error if it doesn't find a directory with top_file in it.
#' @export
prj_dir_containing <- function(top_file) {
  uppaths=c("./", "../", "../../", "../../../", "../../../../", "../../../../../")
  res <- sapply(uppaths, function(x) any(dir(path=x)==top_file))
  res <- res[res == TRUE]
  
  if(!length(res)) stop(paste("Can't find the directory with", 
                                 top_file, "in it from", getwd(), collapse=" "))
  
  names(res)[names(res) == "./"] <- ""  # if it is in the current directory, just call it "" rather than ./
  
  names(res)[1]
}