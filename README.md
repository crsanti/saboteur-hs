# Saboteur engine

## Original board game steps

Number of players: 3 to 10

### Set up

- Separate cards into path, action, nugget, role cards
- Draw role cards based on number of players. Shuffle them and deal them to players. (Role assignation)
  - Given 3 players: 1 saboteur and 3 miners
  - Given 4 players: 1 saboteur and 4 miners
  - Given 5 players: 2 saboteurs and 4 miners
  - Given 6 players: 2 saboteurs and 5 miners
  - Given 7 players: 3 saboteurs and 5 miners
  - Given 8 players: 3 saboteurs and 6 miners
  - Given 9 players: 3 saboteurs and 7 miners
  - Given 10 players: all dwarf cards

- Extract treasure cards and start card, set up maze with start card and shuffled treasure cards
- Merge action and path cards and shuffle them
- Deal playing cards to players based on number of players. Remaining cards will be the playing deck.
  - From 3 to 5 players: Each player is dealt 6 cards.
  - From 6 to 7 players: Each player is dealt 5 cards.
  - From 8 to 10 players: Each player is dealt 4 cards.
- Assign first turn to player.

### Playing

1. Play a card from the hand if it has one, otherwise pass turn. Playing a card means to:
  - Add path card to maze
  - Play action card
    - against a player (can be itself)
    - against the maze (to break a path or reveal a treasure)
  - Add one card from the hand to the discard pile
2. Draw card from playing deck if not empty
3. End turn.

### Rules for playing cards

There are four types of action cards:

- Broken tool card
- Tool repair card (with one or two tools to repair)
- Treasure reveal card
- Rockfall card

So next rules should be followed:

- You can not play a path card if he has a broken tool. He's forced to play an action card or discard from his hand.
- You can only play a tool repair against another player (or yourself) both tools matches (in case the repair card has two tools at least one of it must match). In case a two-tools repair card is played against a player that has those two broken tools, both are repaired.
- You can not play a broken tool card against a player that has that tool broken.
- A rockfall card can only be played to remove a path card (not start nor treasure cards) so it can only be player if there's at least one path card to remove.
- You can play a path card on the maze if all sides of that card are allowed and does fit all the sides with the positions that its siblings.

### Ending the round

The game ends when three rounds are finished. A round finishes when:

- All players have no cards on their hands. In that case saboteurs win.
- The gold card is reached by a valid path from start card. In that case miners win.

#### If saboteurs win

All saboteurs draws gold nugget cards from the deck up to a certain value (number of nuggets) based on number of saboteurs in game:

- If there were one saboteur: each gets 4 nuggets
- If there were two or three saboteurs: each gets 3 nuggets
- If there were four saboteurs: each gets 2 nuggets

#### If miners win

Shuffle and draw as many cards from the gold nugget deck as players in game and give them to the player that reached the treasure card. He selects draws one card and passes the remaining nugget cards to the next player in turn.

After all cards are dealed the round is finished.

### Set up next round

- All players keeps their assigned roles and gained nuggets.
- All path and action cards are piled and shuffled
- Treasure cards are shuffled and placed in the maze
- Players are dealt the same number of playing cards. Remaining cards are used as playing deck.
