

:- expects_dialect(lps).

% Rock, Paper, Scissors
% 
en("
the maximum time is 5.

the events are:
   a player inputs a choice and an amount.

the actions are:
   a player pays a prize.

the fluents are:
   the game is over, known as gameOver.
   the reward is an amount, known as reward,
   a player has played a choice, known as played.

initially: the reward is 0.

scissors beats paper.
paper beats rock.
rock beats scissors.

the observations are:
  miguel inputs rock and 1000 from 1 to 2,
  bob inputs paper and 1000 from 1 to 2,
  alex inputs paper and 1000 from 2 to 3.

When a player inputs a choice and a value
and the value is greater than 0
and it is not the case that the player has played a second choice
then the player has played the choice.


the players are a number, known as num_players, at a time if
  the number at the time is
     the sum of all
        a player has played a value at the time.

It must not be true that
    the players are a number, known as num_players, at a time
and the number is greater than 2.

When a player inputs a choice and a value
then the reward that is a number becomes the number plus the value.

When a player pays a prize
then the reward that is a number becomes the number minus the prize.

If a first player has played a first choice at a first time
and a second player has played a second choice at the first time
and the first player is different from the second player
and the first choice beats the second choice
and it is not the case that the game is over at the first time
then initiate the game is over from the first time to a fourth time
and the reward is a prize at the first time
and the first player pays the prize from the first time to a second time.

If a first player has played a first choice at a first time
and a second player has played a second choice at the first time
and the first player is different from the second player
and it is not the case that the first choice beats the second choice
and it is not the case that the second choice beats the first choice
and it is not the case that the game is over at the first time
then initiate the game is over from the first time to a fourth time
and the reward is a prize at the first time
and a number is half the prize
and the first player pays the number from the first time to a second time
and the second player pays the number from the first time to the second time.
").

/** <examples>
?- go(Timeline).
?- dumplps.
?- dump.
*/