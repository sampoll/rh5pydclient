
#' @export
`[.h5pyd._hl.files.File` <- function(obj, x)  {
     y <- obj$`__getitem__`(x)
}

#' @export
`[.h5pyd._hl.group.Group` <- function(obj, x)  {
     y <- obj$`__getitem__`(x)
}

#' @export
`[.h5pyd._hl.dataset.Dataset` <- function(obj, x)  {
     y <- obj$`__getitem__`(x)
}


#' @export
`[<-.h5pyd._hl.dataset.Dataset` <- function(obj, x, value)  {
     obj$`__setitem__`(x, value)
}


#' initPython
#'
#' Initialize access to \code{reticulate} package and check
#' for installed modules \code{h5pyd} and \code{numpy}.
#'
#' @param path The path to the Python executable to be used
#' 
#' @examples
#' initPython(path='/anaconda/bin/python')
#' 
#' @importFrom reticulate use_python 
#' @importFrom reticulate py_module_available
#' @importFrom reticulate py_available 
#' @importFrom reticulate py_config
#' @export
initPython = function(path='')  {
  if (!requireNamespace("reticulate"))
    stop("can't attach R package reticulate")
  if (path != '')  
    Sys.setenv('RETICULATE_PYTHON'=path)
  if (!py_available(initialize=TRUE))
    stop("can't find python")
  if (!py_module_available("h5pyd"))
    stop("python module h5pyd is not installed")
  if (!py_module_available("numpy"))
    stop("python module numpy is not installed")

  cfg <- py_config()
  print(paste0("using Python executable ", cfg$python))
}

#' initConn
#'
#' Initialize connection to h5pyd server
#'
#' @param domain H5File domain (see documentation)
#' @param mode File mode ('r' for read-only, 'r+' for read-write, 'w' for write) 
#' Note: 'w' obliterates everything in the file
#' @param endpoint endpoint
#' @return object of type \code{h5pyd_connection}
#' 
#' @examples
#' conn <- initConn(domain='/home/stvjc/tenx_full.h5', mode='r', endpoint='http://52.4.181.237:5101') 
#' 
#' @importFrom reticulate import 
#' @importFrom reticulate import_builtins
#' @export
initConn <- function(domain, mode = 'r', endpoint)  {
  h5py <- import("h5pyd", convert = FALSE)
  np <- import("numpy", convert = FALSE)
  h5file <- h5py$File(domain, mode, endpoint=endpoint)  # TODO: check for not-found
  return(list(h5file=h5file, mode=mode, status="open"))
}

#' closeConn
#'
#' Close connection to h5pyd server
#'
#' @param conn object of type \code{h5pyd_connection}
#' 
#' @examples
#' closeConn(conn) 
#' 
#' @importFrom reticulate import 
#' @export
closeConn <- function(conn)  {
  conn$h5file$close()
  conn$status = "closed"
}

#' getDataset
#'
#' Acquire a handle to a (read-only) h5pyd \code{Dataset} object
#'
#' @param conn Object of type \code{h5pyd_connection} (from \code{initConn})
#' @param path Full path to the dataset to open 
#' @return object of type \code{Dataset}
#'
#' @examples
#' D <- getDataset(conn, 'newassay001')
#' @export
getDataset <- function(conn, path)  {
  f <- conn$h5file
  ds <- f[path]
}

#' createGroup
#'
#' Create a new group in a h5pyd \code{File} object
#'
#' @param conn Object of type \code{h5pyd_connection} (from \code{initConn})
#' @param path Full path to the \code{Group} to open 
#'
#' @examples
#' \dontrun{
#' createGroup(conn, '/newgrp')
#' }
#' @export
createGroup <- function(conn, path)  {
  if (conn$status != "open")
    error("File not open")
  if (conn$mode == "r")
    error("Unable to create group, file open readonly")
  conn$h5file$create_group(path)
}

#' createDataset
#'
#' Create a new dataset in a h5pyd \code{File} object
#'
#' @param conn Object of type \code{h5pyd_connection} (from \code{initConn})
#' @param path Full path to the dataset to open 
#' @param shape List of integer dimensions (explicitly integer, use L or as.integer)
#' @param type Valid numpy type ('int32', 'int64', 'float32', 'float64')
#'
#' @examples
#' \dontrun{
#' createDataset(conn, '/newgrp/newdset', list(10L, 10L), 'float32')
#' }
#'
#' @importFrom reticulate tuple
#' @export
createDataset <- function(conn, path, shape, dtype='int32')  {
  if (conn$status != "open")
    error("File not open")
  if (conn$mode == "r")
    error("Unable to create group, file open readonly")
  t <- tuple(shape)
  conn$h5file$create_dataset(name=path, shape=t, dtype=dtype)
}

#' getSubmatrix
#'
#' Fetch data from \code{Dataset} object
#'
#' @param dset Object of type \code{Dataset}
#' @param indices Character vector of slices. Must be in format 'start:stop:step' 
#' each integer-valued.  (R-conventions)
#'
#' @examples
#' A <- getSubmatrix(D, c('200:300:1', '400:500:1'))
#'
#' @importFrom reticulate py_to_r
#' @importFrom reticulate tuple
#' @importFrom reticulate import 
#' @importFrom reticulate import_builtins
#' @export
getSubmatrix <- function(dset, indices)  {
  sh <- py_to_r(dset$shape)
  if (length(sh) != length(indices))  {
    warn(paste0("invalid number of indices ",len(sh), " != ", len(indices)))
    return(NULL)
  }

  slices <- c()
  for (i in 1:length(indices))  {
    sl <- ind2slc(indices[i])
    slices <- c(slices, sl)
  }

  t <- tuple(slices)    # note: reticulate::tuple not builtins$tuple
  npdata <- dset[t]     
  py_to_r(npdata)
}

#' setSubmatrix
#'
#' Store data to \code{Dataset} object
#'
#' @param dset Object of type \code{Dataset}
#' @param indices Character vector of slices. Must be in format 'start:stop:step' 
#' @param mm R array of data to store
#' each integer-valued.  (R-conventions)
#'
#' @examples
#' setSubmatrix(D, c('1:2:1', '1:2:1', '1:2:1'), array(c(4, 14, 7, 17, 5, 15, 9, 19), dim=c(2, 2, 2)))
#'
#' @importFrom reticulate py_to_r
#' @importFrom reticulate tuple
#' @importFrom reticulate import 
#' @export
setSubmatrix <- function(dset, indices, mm)  {
  sh <- py_to_r(dset$shape)
  if (length(sh) != length(indices))  {
    warn(paste0("invalid number of indices ",len(sh), " != ", len(indices)))
    return(NULL)
  }

  slices <- c()
  for (i in 1:length(indices))  {
    sl <- ind2slc(indices[i])
    slices <- c(slices, sl)
  }

  # TODO: Check dimensions of slices and mm are compatible

  np <- import("numpy", convert = FALSE)

  t <- tuple(slices)    # note: reticulate::tuple not builtins$tuple
  dt <- py_to_r(dset$dtype$name)          # numpy datatype
  npdata <- np$array(mm, dtype=dt)
  dset[t] <- npdata    

}

# Private utility function for converting index string of form '1:10:2' 
# into a python slice object
ind2slc <- function(indstr)  {
  builtins <- import_builtins()
  s <- as.integer(strsplit(indstr, ':')[[1]])
  if (length(s) != 3) {
    warn(paste0("slice syntax error"))
    return(NULL)
  } 

  # convert slice to Python conventions
  start <- s[1]-1     # Python: arrays start at 0
  stop <- s[2]-1      # Python: arrays start at 0
  step <- s[3] 

  # Python: if (stop - start) is a multiple of step, the 
  # last value is not included in the slice

  if ((stop-start)/step == round((stop-start)/step))  
    stop = stop+1
 
  sl <- builtins$slice(as.integer(start), as.integer(stop), as.integer(step))
}





