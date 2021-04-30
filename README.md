# 4xclearer

A board conquering game with total transparency. For observation.

# What?

It is common in 4X games to feature-overload. This is an experiment to see whether a 4X game can still be interesting if it is extremely limited in the different units and actions one can take. Rules may change any minute, with the exception of these cardinal ones:

* No fog. All players can see the state of the board at all times, as well as all other relevant information.
* No randomness, except in board generation. The outcome of any action should be predictable.

Obviously, this precludes some tactics common in 4X - exploration, surprise attacks, bluffing. The primary hypothesis I test is that it will still be tough to create an artificial player good at this. 

# Changelog

## V1

### Rules

* The board is a square of square tiles. Leaving the map is impossible, cities next to the map borders are disadvantaged.
* Cities control tiles around them within some radius (starting at 1, increasing by 1/40 per turn).
* Within these tiles, the player can buy a unit for 24 money. Units are bought moveless. 
* Each city produces an income of 1, plus you get an income of 1 for every tile you control.
* A unit gets one move per turn, which it *may* use to do one of: move one tile up/down/left/right, fight an enemy unit, capture an enemy city, found a city.
* If moved, it will either capture a city, fight an enemy unit, or move to the tile, depending on what was there before the move.
* No tile may contain more than one thing of the same player; such moves are invalid.
* Fighting means that both units in combat die.
* Cities may only be founded at tiles not belonging to anybody.
* Capturing a city has the sole effect of changing that city's allegiance to that of the attacking unit. The attacking unit stays where it is (and is done for the turn).
* Income is added to the player's treasury at the end of their turn.

### Resulting gameplay

Major issue: Cities cannot disappear AND they cannot be passed through, so it is possible to trap/station units in city circles and/or create an impenetrable barrier for the enemy. This creates an unwinnable game for the other player, since they will never be able to get past the barrier. This could be resolved by providing some way of destroying your own cities for money (like a plunder & burn strategy). 

Minor issue: City radius doesn't do much. The map usually gets filled up anyway, so there is no point to the whole concept. Instead, to reflect city growth, city center revenue might grow over time.

Units should be able to move through friendly cities. Trouble is, this will mess up the balance of power, since defense will get much easier and some strategy elements of city control will get lost. Two hack-y options: either allow free travel through a city (emerging on the other side - two steps instead of one) (there are never two cities directly next to each other), or make units in cities defenseless. First option sounds like it has better strategy possibilities.

A torus map might be better from a gameplay perspective, but maybe the borders provide an advantage - at the very least they allow for greater separation given a map size.

### AI

The AI's biggest success is following the rules and using most available actions *somehow*. It mostly wanders around randomly. Two major obstacles have already presented themselves: difficult pathfinding and difficult city planning.

Pathfinding will get easier with solving city transport, but units still lack a sense of direction. They don't know whether they should capture, defend, exterminate, or what. Try an emergent solution with units deciding on targets (nearby opponent cities, free spots, or enemy units). Targets are created at unit creation and are considered when picking new targets.

### Code quality

Structures should be generalised to objects. Actions should be provided in a more logical interface, perhaps through packages, so that the AI would only send in move attempts (and thus couldn't break the rules).

## V2

### Rules

* Income is now only tied to tile count. 
* Tiles belong to the player with most cities nearby, or nobody if there is a tie.
* Cities do not grow (but the feature hasn't been cut from the code entirely yet).
* Moving to a friendly city is an attempt to move two steps in that direction (through the city). This can be used to attack.

### Resulting gameplay

City defense is much easier, as is city regaining. Poorly placed cities are less valuable. Torus idea still interesting.

Bug: Now that tiles can get tied, it is possible to get many cities next to each other, and the move system doesn't work with it properly (jumping 4 tiles with 2 cities).

Graphics: Movement would be better without a pop-up window. Same for buying units. Also need a win/lose screen.

### AI

I solved the problem of pathfinding: the AI picks targets and moves towards them. When reaching the target, the unit will found a city, unless there is an enemy unit nearby, in which case it will kill it. This has the advantage of using most units in a meaningful way. With the current system, the AI crushes me with a 2-to-1 city starting advantage, but I crush it on 1-to-1.

Troubles with targets:

* Targets are not reconsidered unless rendered invalid, so the unit can miss big opportunities on its way to a target that has since become almost worthless.
* We first find buy location and then the target, but the other way around makes more sense (and is closer to how I think when playing). Once you pick a target, we should only pick tiles by their relation to that tile, but we just pick tiles good overall, then try to find something.
* Pathfinding can get stuck if there are a lot of units on at the same time. Pathfinding can overshoot one direction through a city and thus keep going back and forth.

### Overall quality

Visuals are much more pleasant and useful. I took a non-textual approach on the board and like it so far, but need to integrate everything to it. Code quality isn't too good, most things are misplaced.
