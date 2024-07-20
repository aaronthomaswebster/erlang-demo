-module(test).
-export([main/0, factorial/1, hangman/0, print_underline/2, get_random_item/1]).

print_hello() ->
    io:format("Hello, world!~n").

factorial(0) ->
    1;
factorial(N) when N > 0 ->
    N * factorial(N - 1).

% Function to compute Fibonacci number
fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N - 1) + fib(N - 2).

% Main function to test Fibonacci computation
main() ->
    io:format("Fibonacci sequence for first 10 numbers:~n"),
    lists:foreach(fun(N) -> io:format("fib(~p) = ~p~n", [N, fib(N)]) end, lists:seq(0, 9)).

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

hangman(Language, Guesses) ->
    clear_screen(),
    % Print the word with underscores for unguessed letters
    print_underline(Language, Guesses),

    % Check if the word is complete
    case word_complete(Language, Guesses) of
        true ->
            io:format("Congratulations! You guessed the word: ~s~n", [Language]);
        false ->
            io:format("You have guessed: ~s~n", [lists:usort(Guesses)]),
            io:format("Guess a letter: "),
            Input = io:get_line(""),
            [Char | _] = string:trim(Input),
            hangman(Language, [Char | Guesses])
    end.

word_complete(Word, GuessedLetters) ->
    lists:all(fun(L) -> lists:member(L, GuessedLetters) end, Word).

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

get_random_item(List) ->
    {_, _} = rand:seed(exsplus, os:timestamp()),
    Index = rand:uniform(length(List)) - 1,
    lists:nth(Index + 1, List).

clear_screen() ->
    % ANSI escape code to clear the screen
    io:format("~c[2J~c[H", [27, 27]).
