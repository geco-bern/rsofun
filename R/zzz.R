# Adopted from http://r-pkgs.had.co.nz/r.html
.onLoad <- function( libname, pkgname ){
  op <- options()
  op.rsofun <- list(
    rsofun.name = "Benjamin Stocker",
    rsofun.desc.author = "Benjamin Stocker <benjamin.stocker@gmail.com> [aut, cre]",
    rsofun.desc.license = "GNU General Public License",
    rsofun.desc = list()
  )
  toset <- !(names(op.rsofun) %in% names(op))
  if(any(toset)) options(op.rsofun[toset])

  invisible()
}