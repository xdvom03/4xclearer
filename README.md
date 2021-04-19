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

* The board is a square of square tiles. Leaving the map is impossible, cities next to the map bordees are disadvantaged.
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
