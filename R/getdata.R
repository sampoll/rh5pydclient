
`[.h5pyd._hl.files.File` <- function(obj, x)  {
     y <- obj$`__getitem__`(x)
}

`[.h5pyd._hl.group.Group` <- function(obj, x)  {
     y <- obj$`__getitem__`(x)
}

`[.h5pyd._hl.dataset.Dataset` <- function(obj, x)  {
     y <- obj$`__getitem__`(x)
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
#' @param endpoint endpoint
#' @return object of type \code{h5pyd_connection}
#' 
#' @examples
#' conn <- initConn(domain='/home/stvjc/tenx_full.h5', endpoint='http://52.4.181.237:5101') 
#' 
#' @importFrom reticulate import 
#' @importFrom reticulate import_builtins
#' @export
initConn <- function(domain, endpoint)  {
  h5py <- import("h5pyd", convert = FALSE)
  np <- import("numpy", convert = FALSE)
  builtins <- import_builtins()
  h5file <- h5py$File(domain, 'r', endpoint=endpoint)  # TODO: check for not-found
  return(list(h5py=h5py, np=np, builtins=builtins, h5file=h5file))
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

#' getSubmatrix
#'
#' Fetch data from \code{Dataset} object
#'
#' @param conn Object of type \code{h5pyd_connection} (from \code{initConn})
#' @param dset Object of type \code{Dataset}
#' @param indices Character vector of slices. Must be in format 'start:stop:step' 
#' each integer-valued.  (R-conventions)
#'
#' @examples
#' A <- getSubmatrix(conn, D, c('200:300:1', '400:500:1'))
#'
#' @importFrom reticulate py_to_r
#' @importFrom reticulate tuple
#' @export
getSubmatrix <- function(conn, dset, indices)  {
  sh <- py_to_r(dset$shape)
  if (length(sh) != length(indices))  {
    warn(paste0("invalid number of indices ",len(sh), " != ", len(indices)))
    return(NULL)
  }
    
  builtins <- conn$builtins
  isok <- TRUE
  slices <- c()

  for (i in 1:length(indices))  {
    s <- as.integer(strsplit(indices[i], ':')[[1]])
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
    slices <- c(slices, sl)
  }

  t <- tuple(slices)    # note: reticulate::tuple not builtins$tuple
  npdata <- dset[t]     
  data <- py_to_r(npdata)
}
