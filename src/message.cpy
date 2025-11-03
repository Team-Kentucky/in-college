       01 message-record.
           05 message-key            pic x(64).
           05 message-sender         pic x(100).
           05 message-recipient      pic x(100).
           05 message-content        pic x(500).
           05 message-timestamp      pic x(20).
           05 message-read-flag      pic x(1) value 'N'.
               88 message-read       value 'Y'.
               88 message-unread     value 'N'.
