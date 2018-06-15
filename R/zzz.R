.onLoad <- function(libname, pkgname)  {

  # Make subsetting operator available for hd5pyd objects
  `[.h5pyd._hl.file.File` <- function(obj, x)  {
       y <- obj$`__getitem__`(x)
  }
  `[.h5pyd._hl.group.Group` <- function(obj, x)  {
       y <- obj$`__getitem__`(x)
  }

  `[.h5pyd._hl.dataset.Dataset` <- function(obj, x)  {
       y <- obj$`__getitem__`(x)
  }

}

