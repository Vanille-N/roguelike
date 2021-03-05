# Projet Programmation 2 - Roguelike

Arnaud Daby-Seesaram
Neven Villani

------

## Execution

Tested with sbt 1.4.6 running Scala 2.13.4 on Java 15.0.2

```sh
$ git clone https://Vanille-N/roguelike
$ git checkout v1.0
$ cd roguelike
$ sbt
sbt:Roguelike> run
```

------

## Project architecture

### Folders

`assets/` contains level initialisation files in the `.room` format: a description of the size followed by a map of characters to describe the content of each position.

`help/` contains help files, all of which can be accessed through the `help ...` command in the graphical interface

`src/` contains project sources

### Files

Each source file has a header comment to summarize its contents. This section focuses more on the interaction between those files

`rogue`: project root, depends on `commands`, `environment`, `movement` and coordinates the interactions between these components and with the graphical interface

`commands`: dependencies needed to interact with `environment` and `entities`. Receives orders from `rogue` and applies them.

`environment`: uses the API from `entities` when it needs to apply an action to all organisms on a specific position. Also coordinates battles between organisms. Also depends on `spawn` for the same reason.

`movement`: player movement (no dependencies) and pathfinding (needs types from `environment`).

`entities`: depends on `stats`

`room`: depends on `spawn` to create walls and place spawners on map

`items`, `colorscheme`, `stats`, `random`: no notable dependencies

------

## Technical choices

### Actions

The player controls an immaterial entity (white) that can move anywhere on the grid (including through walls). Through its actions it can give orders to viruses (green) that appear from spawners (`"+"`) to move and interact with cells (red) and items (`"i"`).
In particular the player can
- move around with arrow keys (or `hjkl`)
- advance the progress of the game by pressing `n` (or typing `"step"` in the integrated command line)
- guide the viruses towards its current position
- select a group of viruses to give them specific orders that do not apply to the rest of the swarm
- take items from all selected viruses to put them in a special inventory
- give an item to a specific organism
- destroy an item
- change the behavior of viruses to either follow the cursor or go to a specific position
- ask for information on all organisms on a specific tile

Not all of these can be counted as truly separate actions, but we have deemed them enough to satisfy the "two actions in addition to movement and picking up items" constraint.

Type `"help"` in the command line to get more information on how to actually perform these actions.

### Items and non-playable entities

There are essentially two kinds of items: those that target all organisms in a certain area, and those that target a specific organism.
Their behaviors are different enough to count them as distinct kinds of entities : items of the first kind cannot be picked up, move around randomly and disappear after a certain time; items of the second kind are immobile and can be picked up, used and dropped by organisms.
Items of the second kind have an activation cost that is paid by the organism by consuming part of its stats

Other entities are all cells, but only some can move while the rest are immovable and constitute walls.
All cells have access to a pathfinding algorithm that makes them go towards (aggressive cells) or away from (passive cells) the bulk of the virus swarm.
Viruses have access to the same pathfinder but their behavior can be more finely tuned.

Cells and viruses appear from spawners, whose location, frequency and quantity of cells spawned are predetermined.
Items can interact with spawners, though in a more limited scope.

### Grid

The playing grid is a 2-dimentional array of buttons, which gives possibility of clicking on any of them to display its contents.

Its layout is as follows:

```
┌────────────────┐
│ VVVVVV  ++  ii │
│ VVVVVV  ++  ii │
│                │
│ CCCCCC  ++     │
│ CCCCCC  ++     │
└────────────────┘
```
`V`: arbitrary measure of the total strength of viruses located on this tile
`C`: same thing for cells
`+`: indicates the presence of a spawner if a `'+'` sign is displayed
`i`: indicates the presence of an item if a `'i'` character is displayed

There is no limit to the number of organisms or items that can be on a specific tile at the same time.

### Ergonomics

We were reluctant to create an interface that would require much clicking to interact with, but text-based interactions are also heavy when overused.
We thus decided on the following compromise.
- some amount of clicking:
    tiles are clickable to allow for printing information on their contents and more intuitive interaction with commands (clicking rather than specifying numeric coordinates)
- a lot of visual feedback:
    - viruses are green, cells are red, and each tile displays a level of green and red color components that reflect the number and strength of organisms on the tile
    - when a special event (item activation) occurs on a tile, a notification is sent through the use of the blue channel
    - a progress bar is shown to indicate the proportions of friendly vs hostile organisms alive
- key- and command-based interaction
    - all frequent commands have two versions, one with a keypress and one with a command
    It is easy to switch between the two modes by typing the key `:` to enter commands mode and entering the command `"q"` to exit command mode.
    - commands can have their parameters entered interactively in several steps with instructions at each step
    - there is a comprehensive and modular help menu (enter command `"help"` or `"help <foo>"`)

### Commands

When a command is executed, it is seen by the command line as a string (which only grows in the case of an interactive execution),
and as an array (composed of the spaced-separated words) for the `Execution*` commands.

Thus, matching the length of this array gives every information an `Execution*` command needs to know where the interactive commands stopped.

------

## Future improvements

We have plans to add
- more room layouts, levels composed of possibly several rooms
- a mechanism (possibly an item to pick up) that gives access to the next level
- a more diverse population of cells (both friendly and hostile)
- consumable items that give temporary or permanent stat boosts
- consumable items that give immunity to other items or organisms
