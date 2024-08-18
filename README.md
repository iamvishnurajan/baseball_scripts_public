These are scripts to publish daily charts for a given MLB team to bluesky

Data is pulled via MLB API

Charts are time trend (game log based) for OPS, Batting Avg w/ RISP, WHIP, and Fielding Range Factor per 9 Innings. These can be fairly easily edited for other charts / statistics.

When deploying, you must edit the R code to set variables in the "USER CONFIGURABLE INPUT PARAMETERS" section

Please note:
- Batting Avg w/ RISP is manually calculated as this does not seem to be available via MLB API in a game log form
- As noted in the code, that section of code may be error prone and is only a a best guesstimate of how the situational statistics are calculated. The method itself is self-consistent but will likely have minor differences to other statistical sources.
- Improvements for that section are welcomed.

Questions? Bugs? Feel free to reach out.

