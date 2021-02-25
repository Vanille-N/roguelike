# Projet Programmation 2 - Roguelike

Arnaud Daby-Seesaram
Neven Villani

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

## Technical choices

### Actions

The player controls an immaterial entity (white) that can move anywhere on the grid (including through walls). Though its actions it can give orders to viruses (green) that appear from spawners (`"+"`) to move and interact with cells (red) and items (`"i"`).
In particular the player can
- move around with arrow keys (or `hjkl`)
- advance the progress of the game by pressing `n` or typing `"step"` in the integrated command line
- guide the viruses towards its current position
- select a group of viruses to give them specific orders that do not apply to the rest of the swarm (see `help select`)
- take items from all selected viruses to put them in a special inventory
- give an item to a specific organism
- destroy an item
- ask for information on all organisms on a specific tile

Not all of these can be counted as truly separate actions, but we have deemed them enough to satisfy the "two actions in addition to movement and picking up items" constraint.

### Items and non-playable entities

There are essentially two kinds of items: those that target all organisms in a certain area, and those that target a specific organism.
All items have an activation cost that is paid by the organism by consuming part of its stats

Other entities are all cells, but only some can move while the rest are immovable and constitute walls.
All moving organisms (controlled by the player or not) have access to a pathfinding algorithm that makes them go towards (viruses and aggressive cells) or away from (passive cells) the cursor.
They appear from a spawner, whose location, frequency and quantity of cells spawned are predetermined.

### Grid

The playing grid is a 2-dimentional array of buttons, which gives possibility of clicking on any of them to display its contents.

Its layout is as follows:

```
┌────────────────┐
│ VVVVVV  ++     │
│ VVVVVV  ++     │
│                │
│ CCCCCC  ++  ii │
│ CCCCCC  ++  ii │
└────────────────┘
```
`V`: arbitrary measure of the total strength of viruses located on this tile
`C`: same thing for cells
`+`: indicates the presence of a spawner if a `'+'` sign is displayed
`i`: indicates the presence of an item if a `'i'` character is displayed

There is no limit to the number of organisms or items that can be on a specific tile at the same time.
