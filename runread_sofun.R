##-----------------------------------------------------------
## Runs the model for one site.
##-----------------------------------------------------------
run_sofun_bysite <- function( sitename, setup ){

  cmd <- paste0( "echo ", sitename, " | ./run", setup$model )
  system( cmd )

}

##-----------------------------------------------------------
## Runs the model.
##-----------------------------------------------------------
run_sofun <- function( settings, setup, par ){

  ## change to directory from where model is executed
  here <- getwd() # save current working directory
  setwd( settings$dir_sofun )

  ## How to run the model from the command line (shell) is different for each implementation (Python and Fortran)
  if (settings$implementation=="fortran"){

    ## Compile source code
    if (setup$do_compile){
      cmd <- paste0("make ", setup$model)
      system( cmd )
    }

    ## create command as a string to execute the model from the shell
    ## The executable, specified by <setup$model>, runs all simulations belonging to an ensemble
    ## or single simulations in the case of global or <settings$ensemble> == FALSE.
    ## Note that for ensemble simulations, a text file containing all runnames belonging
    ## to this ensemble is <settings$dir_sofun>/run/runnames_<settings$name>.txt. 
    ## This is written by prepare_setup_sofun().
    cmd <- paste0("echo ", settings$name, " | ./run", setup$model )

    system( cmd )

  }

  setwd( here )
  return(error)
}

##-----------------------------------------------------------
## Reads output.
##-----------------------------------------------------------
read_sofun <- function( settings, setup, par ){


  return(out)
}

##-----------------------------------------------------------
## Runs the model and reads output in once.
##-----------------------------------------------------------
runread_sofun <- function( settings, setup, par ){

  error <- run_sofun( settings, setup, par )

  out <- read_sofun( settings, setup, par )

  return(out)
}