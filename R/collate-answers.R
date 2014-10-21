

#' collate answers given in a bunch of pull requests
#' 
#' Given a lot of pull requests for a particular homework and the
#' name of the homework file, this function cycles over the pull
#' requests and for each 1) checks out the commit associated with the
#' pull request, 2) makes a temporary file that is like the committed
#' homework, but it includes some additional
#' variable assignments in it and some code at the bottom, 3) renders
#' that file to an html that goes into the /tmp directory (might not
#' work on a PC), 4) saves the resulting answers in a data frame
#' that gets saved by saveRDS in /tmp with the name 
#' githubuser-homework_basename (whatever those are for the particular
#' commit that we are doing this for), 5) at the end it goes back
#' and rbinds all those data frames together and returns them. 
#' We have to go through all this rigamarole because knitr knits stuff
#' in a totally separate R process.  
#' @param pr  The data frame of pull requests like that produced by 
#' \code{\link{grab_open_pull_requests}}.  You should be sure to restrict it
#' to just the lines that relate to the particular homework you want to be
#' squashing out.
#' @param hw_path  The path to the homework set to be collating results for.
#' @param out_dir  The directory to write all the (temporary) output to.
#' @param InsertPattern The text pattern (regex) in the hw_path file at which
#' you want to insert (+/- the InsertDrift lines) the variable assignments for rr_github_name and rr_commit.
#' @param InsertDrift The number of lines up or down from the InsertPattern line
#' that you want to actually to insert the lines.
#' @export
#' @examples
#' # imagine we are in the rep-res-course repository at the top level
#' pr <- grab_open_pull_requests()
#' pr <- pr[pr$branch == "ex-test", ] # restrict focus to just those on the ex-test branches
#' ans <- collate_hw_answers(pr, hw_path = "exercises/trial_homework.rmd", InsertPattern = "```\\{r insert-ids,", InsertDrift = 1) #}
#' View(ans)
collate_hw_answers <- function(
  pr,
  hw_path,
  out_dir = ".rr_tmp",
  InsertPattern,
  InsertDrift) {
  
  if(!file.exists(out_dir)) {
    dir.create(out_dir)
  }
  
  for(i in 1:nrow(pr)) {
    gh_user <- pr$user[i]
    gh_commit <- substr(pr$commit[i], 1, 8)
    rrhw::checkout_hw_branch(gh_commit)
    fname <- file.path(out_dir, paste(gh_user, basename(hw_path), sep="-"))
    
    message(paste("Rendering", fname))
    
    # insert the variable assignment lines
    flines <- readLines(hw_path)
    insert_at <- grep(InsertPattern, flines) + InsertDrift
    insert_what <- paste("\n\n```{r, include=FALSE}\n rr_github_name <- \"", 
                         gh_user, "\"; rr_commit <- \"", gh_commit, "\"\n```\n",
                         sep="" )
    flines[insert_at] <- paste(flines[insert_at], insert_what, sep="")
    
    # append the Code block that saves to RDS
    flines[length(flines)] <- paste(flines[length(flines)],
                      "\n\n```{r, include=FALSE}\nreslist <- as.list(rrhw_env); ",
                      "zz <- plyr::ldply(.data = reslist, .fun = answer_list_to_data_frame); ",
                      "saveRDS(zz, file = \"", 
                      gsub("\\.rmd$", ".rds", basename(fname)), "\"",
                      ")\n```\n\n", sep="")
    
    # write that file out to the out-dir
    cat(flines, file = fname, sep="\n")
    
    rmarkdown::render(input = fname, output_format = "html_document", envir = .GlobalEnv)
    
  }
  
  # finally, for a last hurrah, rbind the individual data frames together
  # and order things nicely and drop the first column and then we are good to go.
  rlist <- lapply(unique(pr$user), function(x) {
    readRDS(file = file.path(out_dir, paste(x, gsub("\\.rmd$", ".rds", basename(hw_path)), sep="-")))
  })
  ret <- do.call(rbind, rlist)
  ret <- ret[order(ret$Q_number, ret$S_github), -1]
  rownames(ret) <- NULL
  ret
  
}

