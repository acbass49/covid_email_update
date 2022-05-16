# covid_email_update
sends a saturday covid email update to everyone on a google survey spreadsheet. Blog post written up about this [here](https://alexbass.me/projects/weekly-covid-update/). I originally wrote this for myself, but thought others may benefit if interested for the weekly email itself or the code to generate it. Click the link above to navigate to the page where you subcribe.

### Cool features...
1. Sends a weekly email to subscribers with 3 figures for user's state and 3 figures nationally (total 6): total cases, total deaths, vaccination rate.
2. User can select their for personalized data.
3. No ads.

### Under the hood...
Reads a google survey from [here](https://alexbass.me/projects/weekly-covid-update/), reads a google sheet from the survey (using an encrypted token from a  google service account), runs an analysis script in R that creates a few figures, and sends an emails read in previously.

Email looks like image below (but updated to the current time) 👇

<img src="https://github.com/acbass49/covid_email_update/blob/master/IMG_17E9A72EF318-1.jpeg" width="200" />

<img src="https://github.com/acbass49/covid_email_update/blob/master/national.png" width="500" />
