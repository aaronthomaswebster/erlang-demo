-module(demo).
-export([print_hello/0, factorial/1, fib_example/0, hangman/0]).

% Function to print "Hello, world!"
print_hello() ->
    io:format("Hello, world!~n").


% Function to compute factorial of a number
% Factorial of 0 is 1
factorial(0) ->
    1;
% Factorial of N is N * factorial(N - 1)
factorial(N) when N > 0 ->
    N * factorial(N - 1).

% Function to compute Fibonacci number
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N - 1) + fib(N - 2).

% Example function to test Fibonacci computation
fib_example() ->
    io:format("Fibonacci sequence for first 10 numbers:~n"),
    lists:foreach(fun(N) -> io:format("fib(~p) = ~p~n", [N, fib(N)]) end, lists:seq(0, 9)).


% Function returns a list of programming languages
programming_languages() ->
    [
        "erlang",
        "python",
        "jvascript",
        "haskell",
        "ruby",
        "java",
        "go",
        "rust",
        "swift",
        "kotlin",
        "typescript",
        "scala",
        "elixir",
        "perl"
    ].


% Entrance function for the Hangman game
% Selects a random programming language and starts the game
hangman() ->
    io:format("Welcome to Hangman!~n"),
    Language = get_random_item(programming_languages()),
    io:format("Guess the programming language: "),
    lists:foreach(fun(_) -> io:format("_ ") end, Language),
    io:format("\n"),
    io:format("Guess a letter: "),
    Input = io:get_line(""),
    [Char | _] = string:trim(Input),
    hangman(Language, [Char]).


% Recursive function to play the Hangman game
% Language: The programming language to guess
% Guesses: List of guessed letters
hangman(Language, Guesses) ->
    clear_screen(),
    % Print the word with underscores for unguessed letters
    print_underline(Language, Guesses),

    % Check if the word is complete
    case word_complete(Language, Guesses) of
        true ->
            % If the word is complete, print congratulations message
            Length = length(Guesses),
            io:format("Congratulations! You found the word \"~s\" in ~p guesses.~n", [Language, Length]);
        false ->
            % If the word is not complete, ask for another letter
            io:format("You have guessed: ~s~n", [lists:usort(Guesses)]),
            io:format("Guess a letter: "),
            Input = io:get_line(""),
            [Char | _] = string:trim(Input),
            hangman(Language, [Char | Guesses])
    end.

% Function to check if the word is complete
word_complete(Word, GuessedLetters) ->
    lists:all(fun(L) -> lists:member(L, GuessedLetters) end, Word).


% Function to print the word with underscores for unguessed letters
print_underline(Word, GuessedLetters) ->
    lists:foreach(
        fun(L) ->
            case lists:member(L, GuessedLetters) of
                true -> io:format("~c ", [L]);
                false -> io:format("_ ")
            end
        end,
        Word
    ),
    io:format("~n").


% Function to get a random item from a list
get_random_item(List) ->
    {_, _} = rand:seed(exsplus, os:timestamp()),
    Index = rand:uniform(length(List)) - 1,
    lists:nth(Index + 1, List).


% Function to clear the screen
clear_screen() ->
    % ANSI escape code to clear the screen
    io:format("~c[2J~c[H", [27, 27]).
