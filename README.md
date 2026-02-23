# Text Mining with R

### Copom, FOMC and Internacional Relations scripts

This repository contains a few text mining projects using tidytext in R. 

While the focus and original intent was to quantify Central Bank comunication sentiment, I found these newly discovered tools 
to be very useful in another application: Discourse Analysis in International Relations. It is not my field of work or research, 
but I turned myself to it to contribute with a Vida (2025) paper. 

All scripts were made with help from AI to initially structure the code, specifically Gemini 3 Pro and Claude Sonnet 4.5. 
All qualitative stuff (like personalized lexicon, categories and weights) were not. 

I hope these scripts can be useful as a starting point for other projects :) 




first, run the script to download the minutes. 
it will create an "atas" folder in your directory in which will be saved minutes ranging from a customizable period.
that script uses webscraping in order to retrieve the minutes.

then, the other script uses tidytext to read, tokenize and evaluate the minutes' stance using a Loughram lexicon mixed with a personalized one.
this approach is heavily based in the BIS paper by Chong and Ho (2022) and the 2017 book by Julie Silge
