# Long-billed hermit foraging efficiency and fear

> Expanding foraging theory to include individual variation 

![Inquiry call automatic detection](./img/example_fig.png)

## Table of contents
* [General info](#general-info)
* [Analyses](#Analyses)
* [To do list](#to-do-list)
* [Contact](#contact)

## General info

Code used for detecting inquiry calls produced during flight tests and analyzing dynamics of calling activity in solo and group flights

## Analysis

* [Automatic inquiry call detection](https://rpubs.com/marcelo-araya-salas/671954)
* [Calling activity results](https://rpubs.com/marcelo-araya-salas/671951)
    - Detecting with 2 or 3 different templates 
    - Cure current detections 
    - Calling rate along recordings 
    - Plot call rate per reproductive stage / sex? (include group baseline)
* [ID individuals in group flights](hhttps://rpubs.com/marcelo-araya-salas/679920)
    - 50 random forest with data subsets of same size for all individuals within a group
    - Testing different acoustic parameters sets
    - Determine probability threshold that optimizes sensitivity and number of calls used (all in solo flights)
    - Apply random forests on group flights, average probabilities and remove probabilities lower that optimal threshold 


## In progress

* 

## To-do list

* Add R-squared (or analogous) to tell apart variation explain by different models and factors (MAS)
* Model diagnostics for MCMCglmm analyses (MAS)
* Add github repository (MAS)
* Add data to figshare repository  (MAS)
* Add comments to paper (MAS)

## Status
Project is: _in progress_

## Contact
Created by [Marcelo Araya-Salas](https://marceloarayasalas.weebly.com/)
