# R_Conway_Life
Another R implementation of John Conway's "Game of Life"

### Basic conventions:
- Each board is represented as a matrix of zeroes (dead cells) and ones (live cells).
- Boards can also be imported and saved as .png images with one black pixel per live cell and one white pixel per dead cell.
- Various data about life session outcomes can be saved in .csv format.

### Key functions
- life() : returns the next frame following an input frame.  By default, the "birth" and "sustain" rules are consistent with the conventional ones.  Other rules can also be specified.
- run_life() : runs the Game of Life for a specified number of generations and returns a list containing information about the board size and population over time and a record of all frames which can be parsed using the get_board() function.
- pop_curve() : draws a line plot of board population by generation
- init_life_history : create/load dataframe to store past life runs

### TODO:
- figure out how to parse .rle files or other formats used in the community
- add time stamps to "life_runs" dataframe so I can tell precisely how inefficient this code is :/
- add beepr functionality
- add functionality for batch-generating starting boards and running sets of many boards
- add functionality for randomly sprinkling in live or dead cells in boards
