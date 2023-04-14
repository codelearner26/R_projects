## Questions that I prepared for Economic Students as a Student Asistant (Emre Öksüz)


## Question 1 | First Steps 

Suppose Metallica has a concert scheduled, and they want to make sure their setlist fits within the allotted time frame. Using the following information, calculate how long their setlist would be in hours?


Their setlist consists of 18 song (6 songs each) , with the following average lengths:
  
short songs: 3 minutes and 30 seconds

medium songs: 5 minutes

long songs: 8 minutes and 30 seconds

On average, they take a 30-second break between each song.

Assuming the concert has a 2-hour time limit, will Metallica's setlist fit within the allotted time frame?

- Upon calculating the length of the hour, rounding the result to two decimal places and    expressing it in hours, a comparison can be made.

- You should get a boolean value, rounded length in hours and length in seconds at the end.

Such as;

False

2.04

7344

**Solution:**
```r
Short_songs <- (3*60+30)*6
Medium_songs<-5*60*6
Long_songs <- (8*60+30)*6
Breaks<-17*30


Total_song_length_in_seconds<-Short_songs+Medium_songs+Long_songs+Breaks

Total_song_length_in_hours<-Total_song_length_in_seconds / 3600
rounded<-round(Total_song_length_in_hours,2)
compare<- 2 >rounded

compare
Total_song_length_in_seconds
rounded
```
## Question 2 | Vectors

In the first 18 seasons of "Family Guy," there are a total of 1008 episodes. The show features a large cast of characters, including the Griffin family (Peter, Lois, Meg, Chris, and Stewie), their neighbors (Quagmire, Cleveland, and Joe), and other recurring characters (such as Brian, the family dog).

Based on available data, we know that:

On average, each episode is approximately 30 minutes long.

The average speaking rate of a character on the show is approximately 120 words per minute.

Assuming that the main characters( Peter, Lois, Meg, Chris, Stewie and Brian) talks twice as much as the side characters(Quagmire, Cleveland, and Joe) in an episode. %20 Quagmire’s words get censored since they were offensive. Using the following information, estimate the total number of words spoken by Peter and Quagmire in the show.


**Solution:**

```r

Episode_Length<-30
Side_length<- Episode_Length/15
Main_length<- Side_length*2
total_words_Peter <- Main_length*120 
total_words_Quagmire<- Side_length*120*0.8
Total<-(total_words_Quagmire+total_words_Peter)*1080
Total
```
Your output should be:


```r
725760

```


## Question 3 | Basic Plotting

- Generate a vector of 12 random integers (between 1 and 30) that represent the number of baby rabbits born in each month over one year period

- Create a vector of month names

- Plot a bar chart of the rabbit births

- Set the x-axis limits to include all months

- Use name.args() function in order to show months

- Set las=3 to see all the month names that you have

- Set the seed to 1907

**Solution:**
```r
RNGversion("3.3.1")
set.seed(1907)
rabbit_births <- sample(1:30, 12, replace=TRUE)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
barplot(rabbit_births, names.arg = months, xlab = "Month", ylab = "Number of Rabbit Births", main = "Rabbit Births by Month", las = 3)
```
Your output should look like:

![image](https://user-images.githubusercontent.com/115809121/224556032-15e0710b-a15b-48a5-92f2-ea9b774ff05b.png)
## Question 4 | Functions

In the movie La La Land, there is a scene where Sebastian plays the piano in a jazz club and get fired afterwards . Suppose we want to analyze the notes that Sebastian plays during this scene, and identify the actual notes based on the piano keys. Every time Sebastion plays the piano there will be a new group of 6 notes.Write a function called saving_sebastian and try to make him play 2 different songs. Maybe this time he will not get fired.

-	 You should assign numbers from 1 to 7. These numbers represent the amount of each note played during the scene
-	You can use noquote function in order to get all the values as desired
-	You need to take the notes as follows: C, C#, D, D#, E, F




**Solution:**

```r

 saving_sebastian <- function() {
  notes_1 <- c("C", "C#", "D", "D#", "E", "F", "song_1")
  amount_1 <- sample(1:7, replace = TRUE)
  names(amount_1) <- notes_1
  
  notes_2 <- c("C", "C#", "D", "D", "E", "F", "song_2")
  amount_2 <- sample(1:7, replace = TRUE)
  names(amount_2) <- notes_2
  
  output <- c(amount_1, amount_2)
  output[names(output) == "song_1" | names(output) == "song_2"] <- " "  
  
  return(noquote(output))
}
saving_sebastian()


```

## Question 5 | Decision Structures 

A UFC fight promotion is considering offering Conor McGregor a multi-fight contract. The contract decision is based on three criteria: the number of previous UFC wins, the average length of each fight, and the number of times he has been injured in previous fights.

If McGregor has won 5 or more UFC fights and has an average fight length of less than 15 minutes, he is offered a multi-fight contract regardless of the number of previous injuries. If he has won less than 5 UFC fights, the promotion will look at both his average fight length and the number of previous injuries. If his average fight length is less than 20 minutes and he has had fewer than 3 injuries, he will be offered a contract. If he meets one of these criteria, he will be called in for further discussions. If he meets none, the offer will be declined.






**Solution:**

```r

conor_contract <- function(wins, avg_length, injuries) {
  
  if (wins >= 5 & avg_length < 15) {
    
    return("Offer multi-fight contract regardless of injuries")
  } else if (wins < 5 & avg_length < 20 & injuries < 3) {
    return("Call in for further discussions")
  } else {
    return("Decline offer")
  }
}






```


## Question 5 | Loops

Suppose Percy Jackson is accused of stealing Zeus's lightning bolt and he is trying to escape from a group of angry monsters. He has to cross a river using a small boat, but the river is full of sea monsters that might try to attack him.

Write a loop to simulate Percy's boat journey across the river. The loop should generate a random number of sea monsters that Percy may encounter on the river, where the number of monsters follows a binomial distribution with parameters n = 10 and p = 0.6. For each encounter with a sea monster, Percy has a 70% chance of successfully fighting it off and continuing his journey, and a 30% chance of losing the lightning bolt and being forced to turn back.

The loop should keep track of Percy's progress across the river, and print out a message indicating whether he was successful in reaching the other side of the river with the lightning bolt, or if he lost it and was forced to turn back. The loop should stop running once Percy has either reached the other side or turned back due to losing the lightning bolt.


- Hint: You can use message function in order to see all the proccess that Percy had




**Solution:**

```r

percy_journey <- function() {
  num_monsters <- rbinom(1, size = 3, prob = 0.6)
  count_monsters <- 0
  
  while (count_monsters < num_monsters) {
    if (runif(1) < 0.7) {
      message("Percy successfully fought off a sea monster!")
    } else {
      message("Percy lost the lightning bolt and was forced to turn back.")
      return("not successful journey")
    }
    count_monsters <- count_monsters + 1
  }
  
  message("Percy successfully crossed the river and returned the lightning bolt to Zeus!")
  return("successful journey")
}
percy_journey()
```


