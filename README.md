# cricket
As a first project to apply over learning of shiny,we try to build this interactive R shiny web app dedicated to Indian cricketers. We use web scrapping from a reliable website to fetch the name, detailed bio data,career statistics of list of Indian cricket players played so far. Google ApI and ggmap package is use to fetch the latitude longitude of the birth place of the cricketers which is stored in the data. Later on after necessary data cleansing and manipulating,app is built.

This app contains two main tab panels:

Maps: It points the pace of birth of the selected players using a marker with a popup of detailed bio data of the same.

Statistics: This tab shows career Statistics. It contains two table namely "Batting" and "Bowling" which will show batting and bowling record of the cricketers respectively.

One can select cricketers on basis three filters:
a. On which format cricketers played
b. State of birth
c. On which year he had debuted
