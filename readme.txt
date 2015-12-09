#Java Syntax Parser in Prolog
#CSC 435 Prolog Final Project
#Programmed by: Thomas Borgia, Connor Davis, Angela Huang, Nate Milkosky 

Java Syntax Basics
https://docs.oracle.com/javase/specs/jls/se7/html/jls-18.html

Compilation and Execution
----------------------------------------------------------------------
While in the SWI-Prolog interpereter, type "[jParser]." to compile.

To execute, type "parse_file("filename.ext")." in the swipl interpreter.

Java Files
----------------------------------------------------------------------
For the purposes of this program, the java files must separate each token with a whitespace. This is because the parse function currently cannot separate tokens by anything other than whitespace. This is something that can be implemented in the future. For examples, look at the test java files stored in the GitHub repository.

Installation Instructions
----------------------------------------------------------------------
For the purposes of this class, we recommend using the Prolog environment SWI-Prolog.

	---------- OSX ----------

	If you have Homebrew installed, you can simply run the command:

	brew tap homebrew/x11
	brew install swi-prolog
	Otherwise, download and manually install from here:
	http://www.swi-prolog.org/download/stable (Links to an external site.)

	Start the interpreter using the command

	swipl

	--------- Linux ---------

	On Linux, it is recommended that you install SWI-Prolog using a package manager.

	For example, in Ubuntu, run the command: 

	sudo apt-get install swi-prolog
	If you do not have a package manager, you will have to compile it from the source code.

	Start the interpreter by using:

	swipl
	 
	---------- Windows ----------

	On Windows, you have to run the installer found here:
	http://www.swi-prolog.org/download/stable (Links to an external site.)

	Once it is installed, the environment can be started from the start menu or where it was installed into.

	Running a Prolog file is as easy as opening a .pl file.
