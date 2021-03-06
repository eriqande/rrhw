% Generated by roxygen2 (4.0.1): do not edit by hand
\name{prj_dir_containing}
\alias{prj_dir_containing}
\title{find directory level which has the file top_file}
\usage{
prj_dir_containing(top_file)
}
\description{
This will go up 0, 1, 2, 3, 4, and 5, directories looking
for the file top_file.  It returns the least high up directory
as a relative path (like "../../").  This is useful for
finding where the .Rproj file is so you can define paths relative
to that when inserting child documents into knitr docs, etc.
Throws an error if it doesn't find a directory with top_file in it.
}

