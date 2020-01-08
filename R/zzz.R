# Adopted from http://r-pkgs.had.co.nz/r.html
.onLoad <- function( libname, pkgname ){
  op <- options()
  op.rsofun <- list(
    rsofun.name = "Benjamin Stocker",
    rsofun.desc.author = "Benjamin Stocker <benjamin.stocker@gmail.com> [aut, cre]",
    rsofun.desc.license = "GNU General Public License",
    rsofun.desc = list(),
    rsofun.dir.sofun = "~/sofun/"
  )
  toset <- !(names(op.rsofun) %in% names(op))
  if(any(toset)) options(op.rsofun[toset])
  # rlang::warn("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  # rlang::warn("WARNING:")
  # rlang::warn("Setting option to specify location of SOFUN (Fortran):")
  # rlang::warn("rsofun.dir.sofun = '~/sofun/'")
  # rlang::warn("Change this option by:")
  # rlang::warn("options( list( rsofun.dir.sofun='string_path_where_sofun_is' ) )")
  # rlang::warn("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  invisible()
}
