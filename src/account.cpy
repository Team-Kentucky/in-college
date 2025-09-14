       01 acct-record.
           05 acct-username pic x(100).
           05 acct-password pic x(12).
           *> Profile information
           05 acct-profile.
               10 profile-first-name pic x(50).
               10 profile-last-name pic x(50).
               10 profile-university pic x(100).
               10 profile-major pic x(100).
               10 profile-graduation-year pic 9(4).
               10 profile-about-me pic x(200).
               10 profile-experience.
                   15 experience-entry occurs 3 times.
                       20 exp-title pic x(100).
                       20 exp-company pic x(100).
                       20 exp-dates pic x(50).
                       20 exp-description pic x(100).
               10 profile-education.
                   15 education-entry occurs 3 times.
                       20 edu-degree pic x(100).
                       20 edu-university pic x(100).
                       20 edu-years pic x(50).
               10 profile-initialized pic x(1).
                   88 profile-exists value 'Y'.
                   88 profile-empty value 'N'.
