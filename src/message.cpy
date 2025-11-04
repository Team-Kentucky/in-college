       01 message-record.
          05 message-key            PIC X(64).
          05 message-sender         PIC X(100).
          05 message-recipient      PIC X(100).
          05 message-content        PIC X(500).
          05 message-timestamp      PIC X(20).
          05 message-read-flag      PIC X(1) VALUE 'N'.
              88 message-read       VALUE 'Y'.
              88 message-unread     VALUE 'N'.
