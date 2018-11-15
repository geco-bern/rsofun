download_file_cx1 <- function( path_remote, path_local ){

	if (!exists("uname")) uname <<- readline( prompt = "Enter your user name for logging onto CX1: " )

  ans <- readline( prompt = "Do you have access to Imperial's CX1 and set up for SSH connection without password? (y/n) " )
  if (ans=="y"){

    ans <- readline( prompt = "Have you connected to Imperial's VPN? (y/n) " )
    if (ans=="y"){

			## No files found at specified location
			rlang::warn( paste0("Downloading from ", path_remote, " to ", path_local ) )
			system( paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", path_remote, " ", path_local ) )

		} else {

			abort( "Aborted because you said VPN wasn't connected. Try again." )

		}

	} else {

		print("You have two options:")
		print("1. Set up SSH login without having to enter a password. Follow instructions here: https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys--2]")
		print(paste0("2. Download them manually. First create the following directory: ", dirname(path_local), " . Then, cd into that and download entering the following command in the terminal:"))
		print(paste0( "rsync -avz ", uname, "@login.cx1.hpc.ic.ac.uk:", path_remote, " ", path_local ))
		abort( "See you later.")

	}

}
