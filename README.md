# Overview


To ilistarate the Erlang language, I wrote a simple program that demonstrates the syntax of the language.  I wanted to learn the basics of the language and i did so by developing a few simple functions.

[Software Demo Video](https://youtu.be/dBaHmmvRkpQ)

# Development Environment

vscode was used to write the code.  The code was written in Erlang.
I used the erlang VSCode extension to provide code highlighting and intellisense.

# Program Description

The program contains the following functions:

1. print_hello() - This function prints "Hello World" to the console.
2. factorial(N) - This function calculates the factorial of a number N.
3. fib_example() - This function demonstrates the fibonacci sequence and output the first 10 numbers in the sequence.
4. hangman() - This function plays a simple game of hangman.

# Execution 

To run the program, you need to have Erlang installed on your machine.  You can download it [here](https://www.erlang.org/downloads).

To run the program, open a terminal and navigate to the folder that contains the code.  Then type the following command.

```bash
erl
```

This will open the Erlang shell.  Then type the following command.

```bash

c(demo).
```

This will compile the code.  

To display Hello World, type the following command.

```bash
demo:print_hello().
```

To display the factorial of a number, type the following command.

```bash
demo:factorial(5).
```

To demo the fibonacci sequence, type the following command.

```bash
demo:fib_example().
```

To play hangman, type the following command.

```bash
demo:hangman(). 
```
To exit the Erlang shell, type the following command.

```bash
q().
```

# Useful Websites

* [Erlang](https://www.erlang.org/doc/apps/stdlib/api-reference.html#modules)
* [Tutorials Point](https://www.tutorialspoint.com/erlang/index.htm)
* [Erlang Tutorial Youtube Playlist](https://www.youtube.com/watch?v=tS98LZzFz50&list=PL7QmO7FaKMe9clz0qsHqa4tYSUTWc2NYml)

# Future Work

* Make the hangman game more interactive. 
* Add a scoring system and a way to set the difficulty level.
* Add the ability to provide your own list of words for the hangman game.
* Expose the fibonacci sequence function to the user so they can input the number of terms they want to see.
* Add more functions to the program.