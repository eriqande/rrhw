
#' simple function to parse the git-pulls output
git_pulls_to_df <- function(y) {
  z <- y[length(y)]
  zsp <- strsplit(z, ":")
  
  data.frame(pr_num = y[1], 
             pr_date = y[2], 
             user = zsp[[1]][1], 
             branch = zsp[[1]][2],
             stringsAsFactors = FALSE)
}

#' return a data frame of pull requests on the repository
#' 
#' This uses git-pulls
#' @param state  Return open, closed, or all pull requests?
#' @param branch_p pattern that the branch names should match. This
#' will filter all pull requests to belong to a certain branch name
#' @export
pull_requests <- function(branch_p = character(0)) {
  
  system("git-pulls update")
  pip <- pipe("git-pulls list open")
  open <- readLines(con = pip)[-1]
  close(pip)
  open <- cbind(state = "open", 
                plyr::ldply(strsplit(open, " +"), git_pulls_to_df),
                stringsAsFactors = FALSE)
  
  pip <- pipe("git-pulls list closed")
  closed <- readLines(con = pip)[-1]
  close(pip)
  closed <- cbind(state = "closed", 
                plyr::ldply(strsplit(closed, " +"), git_pulls_to_df),
                stringsAsFactors = FALSE)
  
  ret <- rbind(closed, open)
  if(length(branch_p>0)) {
    ret <- ret[grepl(pattern = branch_p, ret$branch), ]
  }
  ret
}



#' Parse the "git-pulls show" output and return a row of a data frame
#' 
#' This also checks to see which files have changed from the merge-base
#' @param N is the pull request number.
git_pulls_show_to_df <- function(N) {
  pip  <- pipe(paste("git-pulls show", N))
  y <- readLines(pip)
  close(pip)
  commit <- strsplit(y[grepl("^cmd: git", y)], "\\.\\.\\.")[[1]][2]
  
  pip <- pipe(paste("git merge-base master", commit))
  merge_base <- scan(pip, what = "character")
  close(pip)
  
  pip <- pipe(paste("git diff --stat ", merge_base, "...", commit))
  diffstat <- readLines(pip)
  close(pip)
  
  if(length(diffstat)<=1) {
    NumFiles <- 0
    Files <- NA
  } else {
    NumFiles <- length(diffstat) - 1
    Files <- sapply(strsplit(diffstat[-length(diffstat)], " +"), function(x) x[2])
  }
  ret <- data.frame(commit = commit,
             num_files = NumFiles,
             files = I(list(Files)),
             stringsAsFactors = FALSE)
  ret
}

#' get all the open pull requests for a branch matching a given pattern
#' 
#' @export
#' @examples
#' grab_open_pull_requests("^ex-test$")
grab_open_pull_requests <- function(branch_p = character(0)) {
  pulls <- pull_requests(branch_p = branch_p)
  pulls <- pulls[pulls$state=="open", ]
  extras <- plyr::ldply(pulls$pr_num[pulls$state=="open"], function(x) git_pulls_show_to_df(x))
  cbind(pulls, extras)
}



