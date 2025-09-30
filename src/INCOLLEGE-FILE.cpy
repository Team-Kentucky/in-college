
file section.
*>-----readInputLine variables-----
*> Input file record
fd input-file.
01 input-buffer pic x(100).

*>-----outputLine variables-----
*> Output file record
fd output-file.
01 output-line pic x(150).

*> Accounts file record
fd acct-database.
copy "account.cpy".
