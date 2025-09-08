IDENTIFICATION DIVISION.
program-id. INCOLLEGE.


*>###################################################################
ENVIRONMENT DIVISION.

input-output section.
file-control.
       select input-file assign to 'input.txt'
           organization is line sequential
           file status is input-file-status.

       select output-file assign to 'output.txt'
           organization is line sequential
           file status is output-file-status.


*>###################################################################
DATA DIVISION.

file section.
*>-----readInputLine variables-----
fd input-file.
01 input-buffer pic x(100). *> Need to decide on reasonable size for lines

*>-----outputLine variables-----
fd output-file.
01 output-line pic x(150).

working-storage section.
*>-----readInputLine variables-----
01 input-prompt pic x(100).
01 input-file-status pic xx.
       88 valid-read value "00".

*>-----outputLine variables-----
01 output-buffer pic x(150).
01 output-file-status pic xx.

*>-----logIn variables-----

local-storage section.


*>###################################################################
PROCEDURE DIVISION.
main.
       open input input-file.

       perform displayLogo.
       perform logInScreen.

       close input-file.
       stop run.


*> Paragraph: logInScreen
*> Purpose:   First menu of the program. Allows the user to sign in or create an account
*> Input:     None
*> Output:    None
logInScreen.
       perform with test after until (input-buffer = 'q' or input-buffer = 'Q' or not valid-read)
           perform outputLine
           perform displayDashedLine
           move "Welcome to inCollege! Select an option" to output-buffer
           perform outputLine
           perform displayDashedLine

           move " [0] Sign in" to output-buffer
           perform outputLine
           move " [1] Create an account" to output-buffer
           perform outputLine
           move " [q] Quit" to output-buffer
           perform outputLine
           move "> " to input-prompt
           perform readInputLine

           *> All other, call menu specific function
           *> q - continue
           *>default: invalid input
           evaluate true
               when input-buffer = '0'
                   perform signIn
               when input-buffer = '1'
                   perform accountCreation
               when (input-buffer = 'q' or input-buffer = 'Q' or not valid-read)
                   continue
               when other
                   move "Invalid input" to output-buffer
                   perform outputLine
           end-evaluate
       end-perform.
       exit.


signIn.
       *>stub
       exit.


accountCreation.
       *>stub
       exit.


*> Paragraph: readInputLine
*> Purpose:   Reads in the next line of the input file
*> Input:     None
*> Output:    input-buffer - Line from the file
*> User is responsible for opening and closing the input file when using this
readInputLine.
       if input-file-status = "00"
           read input-file
               not at end *>NOTE: Do we want to keep reading until we get a non blank input??
                   *>Simulating the user entering the input
                   string
                       function trim(input-prompt, trailing) delimited by size
                       " " delimited by size                                   *> Will always have an extra space (even if input has no prompt)
                       function trim(input-buffer, trailing) delimited by size
                       into output-buffer
                   end-string
                   perform outputLine
               at end
                   move input-prompt to output-buffer
                   perform outputLine

                   *> Input-buffer is stale now, so set to spaces
                   move spaces to input-buffer
                   *> Notify user
                   move "Reached end of input file ( ੭ˊᵕˋ)੭" to output-buffer
                   *> We should eithter
                       *> Make this trigger a use when to end the program?
                       *> Have the user be responsible for gracefully closing
                       *> Switch to manual input from accept

                   perform outputLine
           end-read
       else
           move input-prompt to output-buffer
           perform outputLine

           string
               "Error reading input file: " delimited by size
               input-file-status            delimited by size
               " (｡•́︿•̀｡)"                   delimited by size
               into output-buffer
           end-string
           perform outputLine
       end-if
       exit.


*> Paragraph: outputLine
*> Purpose:   Prints string in buffer to console and saves to output log
*> Input:     output-buffer - string you want to be output. Will be cleared
*> Output:    None
outputLine.
       open extend output-file.

       if output-file-status = "35"
           *> Log file does not exist yet, so create it
           open output output-file
       end-if

       *> Ensure file opened properly
       if output-file-status = "00"
           *> If we want to ensure that the console and output file are the same,
           *> console output can only happen if output file opens
           move output-buffer to output-line
           display function trim(output-line, trailing) *> Will make logged output have extra spaces compared to command line. Do we care?
           write output-line

           close output-file
       else
           display "Error opening output file: " output-file-status " (っ- ‸ - ς)"
       end-if

       *> Some characters get 'stuck', manually clearing fixes it though
       move spaces to output-buffer.
       exit.


*> Paragraph: displayLogo
*> Purpose:   Prints the beautiful ascii logo for us
*> Input:     None
*> Output:    None
displayLogo.
       perform displayASCIILine.
       perform outputLine.
       move "           /##            /######            /## /##                                        " to output-buffer.
       perform outputLine.
       move "          |__/           /##__  ##          | ##| ##                                        " to output-buffer.
       perform outputLine.
       move "           /## /####### | ##  \__/  /###### | ##| ##  /######   /######   /######           " to output-buffer.
       perform outputLine.
       move "          | ##| ##__  ##| ##       /##__  ##| ##| ## /##__  ## /##__  ## /##__  ##          " to output-buffer.
       perform outputLine.
       move "          | ##| ##  \ ##| ##      | ##  \ ##| ##| ##| ########| ##  \ ##| ########          " to output-buffer.
       perform outputLine.
       move "          | ##| ##  | ##| ##    ##| ##  | ##| ##| ##| ##_____/| ##  | ##| ##_____/          " to output-buffer.
       perform outputLine.
       move "          | ##| ##  | ##|  ######/|  ######/| ##| ##|  #######|  #######|  #######          " to output-buffer.
       perform outputLine.
       move "          |__/|__/  |__/ \______/  \______/ |__/|__/ \_______/ \____  ## \_______/          " to output-buffer.
       perform outputLine.
       move "                                                               /##  \ ##                    " to output-buffer.
       perform outputLine.
       move "                                                              |  ######/                    " to output-buffer.
       perform outputLine.
       move "                                                               \______/            	     " to output-buffer.
       perform outputLine.
       move "                                                                             ฅ^•ﻌ•^ฅ        " to output-buffer.
       perform outputLine.
       perform displayASCIILine.
       move "                                                                    Created by Team Kentucky" to output-buffer.
       perform outputLine.
       move "                            The world's best job site for students                          " to output-buffer.
       perform outputLine.
       perform outputLine.
       perform outputLine.
       exit.


displayASCIILine.
       move "############################################################################################" to output-buffer.
       perform outputLine.
       exit.


displayDashedLine.
       move "————————————————————————————————————————————————————————————————————————————————————————————" to output-buffer.
       perform outputLine.
       exit.
