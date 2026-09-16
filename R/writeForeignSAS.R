### This file is part of the 'foreign' package for R.

# Copyright (c) 2004-2015  R Development Core Team
# Enhancements Copyright (c) 2006 Stephen Weigand

#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

# Generate valid SAS string literals 
squote <- function(x) paste0("'", gsub("'","''",x), "'")

# Check if valid SAS name
nvalid <- function(x) grepl("^[0-9]",x) | !grepl("[^a-zA-Z_0-9]",x)

# Convert names to name literals
nliteral <- function(x) ifelse(nvalid(x),x,paste0(squote(x), "n"))

# Convert names into valid SAS names
make.SAS.names <- function(names,validvarname = c("ANY", "V7", "V6")){
  validvarname <- match.arg(validvarname)
  nmax <- if(validvarname == "V6") 8L else 32L
  
  x <- names
  if (validvarname == "ANY") {
    x <- trimws(x,which="right")
  } else {
    x <- gsub("[^a-zA-Z0-9_]", "_", x)
    x <- sub("^([0-9])", "_\\1", x)
  }
  
  # Avoid variable names SAS uses in data steps
  x <- sub(paste0("^("
                  ,"_n_|_iorc_|_error_|_infile_|_file_"
                  ,"|_all_|_char_|_numeric_|_character_"
                  ,")$")
           ,"_\\1",x,ignore.case = TRUE)
  
  # Make sure names are unique 
  if (anyDuplicated(tolower(x))) {
    fix <- make.unique(tolower(x),sep="_")
    x <- paste0(substr(x,1,nchar(x)),substring(fix,nchar(x)+1))
  }
  
  # Try to abbreviate to fit in nmax bytes
  x <- abbreviate(x, minlength = nmax)
  
  if (any(nchar(x, type="bytes") > nmax) || anyDuplicated(tolower(x)))
    stop(gettextf("Cannot uniquely abbreviate the variable names to %d or fewer characters", nmax), domain = NA)
  
  x
}

# Convert names into valid SAS names
make.SAS.memnames <- function(names,validmemname = c('COMPATIBLE', 'EXTEND', 'V6')){
  validmemname <- match.arg(validmemname)
  nmax <- if(validmemname == "V6") 8L else 32L
  
  x <- names
  # Avoid leading/trailing space in memname
  x <- trimws(x,which="both")
  if (validmemname == "EXTEND") {
    # Replace characters not valid for extended memname
    x <- gsub('[/\\*?"<>|:-]',"_",x)
  } else {
    x <- gsub("[^a-zA-Z0-9_]", "_", x)
    x <- sub("^([0-9])", "_\\1", x)
  }
  
  # Avoid _NULL_ as membname
  x <- sub('^(_null_)$','_\\1',x,ignore.case=TRUE)
  
  # Make sure names are unique 
  if (anyDuplicated(tolower(x))) {
    fix <- make.unique(tolower(x),sep="_")
    x <- paste0(substr(x,1,nchar(x)),substring(fix,nchar(x)+1))
  }
  
  # Try to abbreviate to fit in nmax bytes
  x <- abbreviate(x, minlength = nmax)
  
  if (any(nchar(x, type="bytes") > nmax) || anyDuplicated(tolower(x)))
    stop(gettextf("Cannot uniquely abbreviate the member names to %d or fewer characters", nmax), domain = NA)
  
  x
}

# Convert names into valid SAS format names
make.SAS.formats <- function(names, validfmtname = c("LONG", "FAIL", "WARN")){
  validfmtname <- match.arg(validfmtname)
  nmax <- if(validfmtname == "LONG") 32L else 8L
  
  x <- gsub("[^a-zA-Z0-9_]", "_", names)
  x <- sub("^([0-9])", "_\\1", x)
  x <- sub("([0-9])$", "\\1F", x) 
  
  # Make sure names are unique 
  if (anyDuplicated(tolower(x))) {
    fix <- make.unique(tolower(x),sep="_")
    x <- paste0(substr(x,1,nchar(x)),substring(fix,nchar(x)+1))
  }  
  # Make sure names do not end in digit
  x <- sub("([0-9])$", "\\1F", x) 
  
  x <- abbreviate(x, minlength = nmax)
  
  if(any(nchar(x, type="bytes") > nmax) || any(duplicated(x)))
    stop("Cannot uniquely abbreviate format names to conform to ",nmax,
         " character limit and not ending in a digit")
  x
}

writeForeignSAS <- function(df, datafile, codefile, dataname = "rdata",
                               validvarname = c("ANY", "V7", "V6"),
                               validfmtname = c("LONG", "FAIL", "WARN"),
                               validmemname = c("COMPATIBLE","EXTEND","V6"),
                               libpath = NULL, libref = "ROutput"
) {
  validvarname <- match.arg(validvarname)
  validfmtname <- match.arg(validfmtname)
  validmemname <- match.arg(validmemname)
  
  # If in-line data requested 
  in.line <- tolower(datafile) %in% c('datalines','cards')
  
  # Check variable types 
  factors <- vapply(df, is.factor, NA)
  strings <- vapply(df, is.character, NA)
  logicals <- vapply(df, is.logical, NA)
  dates <- vapply(df, inherits, NA, "Date")
  datetimes <- vapply(df, inherits, NA, "POSIXt")
  times <- vapply(df, inherits, NA, "times")
  xdates <- vapply(df, inherits, NA, c("dates", "date"))
  
  # Make valid memname 
  dataname <- nliteral(make.SAS.memnames(dataname,validmemname=validmemname))
  
  # Generate vectors needed for generating SAS code 
  labels <- names <- names(df)
  informats <- formats <- fmtnames <- character(length(names))
  SASnames <- make.SAS.names(names, validvarname = validvarname)
  fmtnames[factors] <- make.SAS.formats(SASnames[factors], validfmtname = validfmtname)
  labels[SASnames == labels] <- ""
  informats[dates | xdates] <- 'YYMMDD.'
  informats[datetimes] <- 'ANYDTDTM.'
  informats[times] <- 'TIME.'
  formats[dates | xdates] <- 'DATE9.'
  formats[times] <- 'TOD8.'
  formats[datetimes] <- 
    paste0("DATETIME",sum(19,getOption("digits.secs"),na.rm=TRUE)
           ,".",getOption("digits.secs")
    )
  formats[factors] <- paste0(fmtnames[factors],'.')
  
  # Make copy of df and apply any needed corrections
  dfn <- df
  dfn[factors | logicals] <- lapply(dfn[factors | logicals], as.numeric)
  
  # Convert date and dates class variables to Date values 
  dfn[xdates] <- lapply(dfn[xdates],as.POSIXct) |> lapply(as.Date)
  
  # Find maximum lengths of string variables
  lengths <- vapply(lapply(df[strings], nchar, type = "bytes")
                    , max, 0L, 1L, na.rm = TRUE)
  
  if (! in.line) {  
    # Write CSV file
    write.table(dfn, file = datafile, row.names = FALSE, col.names = FALSE,
                sep = ",", quote = TRUE, na = "", qmethod = "double")
    
    # Length of longest line in bytes, cannot be less than 1
    lrecl <- max(1L,nchar(readLines(datafile),type="bytes")) 
  }
  
  # Write codefile comments
  cat("* Written by R;\n", file = codefile)
  cat("* ", deparse(sys.call(-2L)), ";\n\n",file = codefile, append = TRUE)
  
  # Write PROC FORMAT step, if needed    
  if (any(factors)) {
    cat("PROC FORMAT;\n", file=codefile, append=TRUE)
    # Convert levels into #='factor', then prefix fmtname and append ';'
    cat(paste0("VALUE ",fmtnames[factors],'\n'
               ,sapply(
                 lapply(
                   lapply(df[factors],levels)
                   ,function(x) {paste(seq_along(x),"=",squote(x))})
                 ,paste,collapse="\n")
               ,"\n;\n")
        ,sep="\n",file=codefile,append = TRUE)
    cat("RUN;\n\n",file=codefile,append = TRUE)
  }
  
  # Write LIBNAME statement, if needed
  if (!is.null(libpath)) {
    cat("libname", libref, squote(libpath), ";\n", file = codefile,
        append = TRUE, sep = " ")
    dataname <- paste0(libref, ".", dataname)
  }
  
  # Write DATA statement 
  cat("DATA" , dataname, ";\n\n", file = codefile,
      append = TRUE, sep = " ")
  
  # Write INFILE statement
  cat(paste("INFILE"
            ,ifelse(in.line,"DATALINES",squote(datafile))
            ,"DSD TRUNCOVER"
            ,ifelse(in.line,"",paste("LRECL=",lrecl))
            ,";"
  ),sep = "\n", file = codefile ,append = TRUE
  )
  
  # Write INPUT statement  
  cat("INPUT\n", file = codefile, append = TRUE)
  cat(paste0("  ",nliteral(SASnames),
             ifelse(strings,paste0(" :$",lengths,"."),"")),
      sep = "\n", file = codefile, append = TRUE)
  cat(";\n\n", file = codefile, append = TRUE)
  
  # Write LABEL statement
  if (any(labels != "")) {
    cat("LABEL",paste(nliteral(SASnames[labels != ""])
                      ,"=",squote(labels[labels != ""])
                      ,collapse = "\n")
        ,";\n"
        ,sep = "\n", file = codefile, append = TRUE)
  }
  
  # Write INFORMAT statement(s)
  if (any(informats != "")) {
    cat("INFORMAT",paste(SASnames[informats != ""]
                         ,informats[informats != ""]
                         ,collapse = "\n")
        ,";\n"
        ,sep="\n", file=codefile, append = TRUE)
  }
  
  # Write FORMAT statement(s)
  if (any(formats != "")) {
    cat("FORMAT",paste(SASnames[formats != ""]
                       ,formats[formats != ""]
                       ,collapse = "\n")
        ,";\n"
        ,sep="\n", file=codefile, append = TRUE)
  }
  
  if (in.line) {
    # Write in-line data 
    cat("DATALINES4;\n", file=  codefile, append = TRUE)
    write.table(dfn, file = codefile, row.names = FALSE, col.names = FALSE
                ,sep = ",", quote = TRUE, na = "", qmethod = "double"
                ,append = TRUE
    )
    cat(";;;;\n", file=  codefile, append = TRUE)
  } else {
    # Write RUN statement to end DATA step
    cat("RUN;\n", file=  codefile, append = TRUE)
  }
}