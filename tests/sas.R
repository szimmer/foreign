library("foreign")
(dd <- data.frame(name  = c("apple", "banana", "carrot", NA),
                  gender= c("male", "female", "male", "female"), stringsAsFactors = FALSE))
##   name gender
## 1    a   male
## 2    b female
## 3    c   male
## 4 <NA> female
setwd(tempdir())
tfSi <- "temp_for_sas_import.txt"
ti   <- "temp_import.sas"
write.foreign(dd, datafile = tfSi, codefile = ti, package = "SAS")
file.show(tfSi) # the NA is shown as <empty>
writeLines(sasCodes <- readLines(ti))
## This failed in foreign <= 0.8-71 :
stopifnot(identical(" name $ 6",
                    grep(" name ", sasCodes, value=TRUE)))

## 'label = TRUE' uses the "label" attributes of the variables as the SAS
## variable labels; an NA or blank "label" attribute is ignored
dl <- data.frame(x = 1:3, y = c("a", "b", "c"), z = c(2.5, 3.5, 4.5))
attr(dl$x, "label") <- "First variable"
attr(dl$y, "label") <- NA_character_
attr(dl$z, "label") <- "   "
write.foreign(dl, datafile = tfSi, codefile = ti, package = "SAS", label = TRUE)
writeLines(sasCode3 <- readLines(ti))
stopifnot(identical('LABEL  x = "First variable" ;',
                    grep("^LABEL", sasCode3, value=TRUE)))

## the default, 'label = FALSE', ignores the "label" attributes
write.foreign(dl, datafile = tfSi, codefile = ti, package = "SAS")
writeLines(sasCode4 <- readLines(ti))
stopifnot(length(grep("^LABEL", sasCode4)) == 0L)

## 'label = TRUE' when no variable has a "label" attribute
dl2 <- data.frame(a = c("x", "y"), b = 1:2)
write.foreign(dl2, datafile = tfSi, codefile = ti, package = "SAS", label = TRUE)
writeLines(sasCode5 <- readLines(ti))
stopifnot(length(grep("^LABEL", sasCode5)) == 0L)

## This site was unresponsive in Jan 2014
if(!nzchar(Sys.getenv("R_FOREIGN_FULL_TEST"))) q("no")
tfile <- "int1982ag.zip"
download.file("ftp://cusk.nmfs.noaa.gov/mrfss/intercept/ag/int1982ag.zip",
              tfile, quiet=TRUE, mode="wb")
zip.file.extract("int1982ag.xpt", tfile)
dfs <- read.xport("int1982ag.xpt")
foo <- dfs$I3_19822
nrow(foo)
stopifnot(nrow(foo) == 3650)
