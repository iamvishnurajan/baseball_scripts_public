These is an R script and associated support files to publish daily charts for a given MLB team to bluesky

Data is pulled via MLB API

Charts are time trend (game log based) for OPS, Batting Avg w/ RISP, WHIP, and Fielding Range Factor per 9 Innings. These can be fairly easily edited for other charts / statistics.

When deploying, you must edit the R code to set variables in the "USER CONFIGURABLE INPUT PARAMETERS" section

Please note:
- Batting Avg w/ RISP is manually calculated as this does not seem to be available via MLB API in a game log form
- As noted in the code, that section of code may be error prone and is only a a best guesstimate of how the situational statistics are calculated. The method itself is self-consistent but will likely have minor differences to other statistical sources.
- Improvements for that section are welcomed.

Questions? Bugs? Feel free to reach out.


![bafkreifbfi7mb43njjannymy4rydemzgfth7kptywrgh432h4bkdd4xx5e](https://github.com/user-attachments/assets/3c7626ff-5391-4a96-8b34-b651931577b2)

![bafkreihvw6fe2rlg7qjk4vz2icga7a4lc3tmb4jtq4bmuaswjk5ug4dwra](https://github.com/user-attachments/assets/fe6ee7d6-e481-4179-a647-5ff1e5f9101a)

![bafkreiftnv2ivz7yrwuwboov4u4u55jfft6g6ygzvdwgnnrqxqdl6mtyrm](https://github.com/user-attachments/assets/8d5de8db-ee81-4f5d-b19a-9fc47bdba4df)

![bafkreiboja3c5niqx6s5vpmiacgciitndcwdbb24c7qzmjv2jworqcuyke](https://github.com/user-attachments/assets/479c9db8-1eb9-4bd8-bdec-97acbc35b9fc)
