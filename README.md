 # 3 x 3 Rubiks Cube Simulator and Solver

The project contains the following files:

- Face.hs :- Defines the 6 faces of the cube
- Cube.hs :- Defines the cube
- KociembaSolver.hs :- Defines the solver
- FileIO.hs :- Defines the file IO functions
- Main.hs :- Defines the parts that deal with the user interface
- A hidden ".ghci" file :- Defines the ghci commands for importing certain modules
- cleanup.hs :- Stores all the code refactorings

## Running the project

To run the project:
1. Open the terminal in the directory where the project is stored. Then simply type `ghci` in the terminal.
2. Load Main.hs in the terminal by typing `:l Main.hs` in the terminal.
3. Type `main` in the terminal to run the program

Some caveats:

- The project calls a python script so make sure you have python3 installed
- To install the script call `pip install kociemba`

## Documentation

Here, I will provide the general idea of the project and how it works in more detail. However, descriptions of the individual functions will be provided in the respective files they are defined in.

### Some info:

- A cube has 6 faces, each with different colours.
- A face has 9 squares or facelets.
- A cube is solved when all the facelets of every face are the same colour.
- A cube is unsolved otherwise.

### Part 1: Face Representation

As mentioned above, a cube has 6 faces, and there are various ways to represent it. One could use arrays/matrices or skip the faces altogether and utilize an array of 54 characters to represent the cube. The number 54 corresponds to 9 * 6 = 54 facelets.

In this project, I have chosen to use 8-bit integers to represent each facelet color. Each facelet color is mapped to a value ranging from 0 to 5. A face is represented by a center facelet and the set of 8 facelets around it. The 8 facelets can be stored as an unsigned 64-bit integer, and the center facelet can be stored separately. The center facelet is stored separately as it is the only facelet that doent change when operations are performed on the cube

So
```
00000000 00000001 00000010 00000011 00000100 00000101 00000000 00000001
```

without spaces can be decoded as
```
WGR
G B
WYO
```

if center facelet is white, then we have the following face:
```
WGR
GwB
WYO
```

Now to rotate a face or to get/set the colour of a particular facelet we can just perform bits operations on the unsigned 64-bit integer. This is why this approach is faster and space efficient than using arrays/matrices. Once again, the concrete descriptions of the functions are provided in Face.hs. I have also included some code with ANSI colour codes to make the command line outputs more readable.

### Part 2: Cube Representation

After implementing the representation of a face, the next step is to define what a cube is. This is accomplished in the Cube.hs file. A cube is essentially a collection of 6 different faces. Rather than creating an array of 6 faces, I have assigned standard conventional names to each face.

The six faces with their names are as follows:
```
Up (uFace)
Down (dFace)
Right (rFace)
Left (lFace)
Front (fFace)
Back (bFace)
```
![Cube](https://www.wikihow.com/images/4/4e/Solve-a-Rubik's-Cube-in-20-Moves-Step-25.jpg)

Please note that unlike the image, in the project, I have used white as the front face and red as the up face.

Cube.hs also defines moves that can be performed on the cube. The names of the moves follow the Singmaster notation.

For information on the moves, please refer to 
[this](https://en.wikipedia.org/wiki/Rubik%27s_Cube_group#Cube_moves) link.

The moves themselves are implemented by internally setting the facelets of different faces according to the move. The code uses bit operation functions defined in Face.hs to perform the moves. These functions provide a nice abstraction that makes the code easier to read and understand. Additionally, info about the moves is provided in Cube.hs

### Part 3: Kociemba Solver

The Kociemba's algorithm for solving the cube is used in KociembaSolver.hs. The algorithm is explained in the [wikipedia](https://en.wikipedia.org/wiki/Optimal_solutions_for_the_Rubik%27s_Cube#Kociemba's_algorithm) article. I've used this algorithm instead of using conventional algorithms like the Beginner's Method and CFOP take to many moves to solve the cube. The Kociemba algorithm however only takes an average of 25-30 moves. A python script is used to run the algorithm.

### Part 4: File IO and Main

FileIO.hs contains the functions for loading and saving the cube to/from the file `cube.txt`. I've used strict I/O compared to lazy I/O in FileIO.hs for reflecting the intended side effects. The main function in Main.hs is used to run the program and contains functions for doing various operations and other actions on the cube.

### Part 5: Miscellaneous

For some reason, I couldn't load the modules System.Process and System.Random. I had to manually load them each time I started the compiler. To automate this, I've used included both the load statements in the file `.ghci` . The file is hidden and can't be accessed by default through the GUI file managers. I've also included old code that has been refactored out in the file `cleanup.hs`.

## References
[Medium Blog by Benjamin Botto](https://medium.com/@benjamin.botto/implementing-an-optimal-rubiks-cube-solver-using-korf-s-algorithm-bf750b332cf9)

[Wiki](https://wiki.ubc.ca/Course:CPSC312-2024W2/RubiksCubeSimulator)
