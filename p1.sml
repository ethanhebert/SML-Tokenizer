(* 
Ethan Hebert and Karina Chang
1-27-23
CSC-330-002
Winter 2023
Project #1
*)

(* Work Divison:
We did most of this assignment together in person.
The only things separate were Karina added the ability
to check if the last ID is a word (TextIO.endOfStream) and 
Ethan added the ability to see if char was a letter (Char.isAlpha).
*)

datatype token = ID of string | EQ | PL | MI | TI | DI;

(* Takes in a string of the filename and calls parseRecur with
its datastream to be held in the val tokenList. If tokenList
contains the error token, an empty list is returned. *)
fun parse(filename) = 
    let
        val infile = TextIO.openIn(filename)
        val tokenList = parseRecur(TextIO.input1(infile),infile)
    in
        if tokenSearch(tokenList,ID("1")) then [] else tokenList
    end


(* Idea from dec14 lecture notes linSearch *)
(* Takes in a token array and a given token and returns if the
token is in the array or not. We use this to test if our error
token ID("1") is in the array to then print an empty list. *)
and tokenSearch([],_) = false
  | tokenSearch(x::xs,token) = if x = token then true else tokenSearch(xs,token)


(* Reads datastream repetitively one char at a time and calls charCheck
on the char until instream is empty. Concatenates the returned token array
to the total token array which is returned at the end. *)
and parseRecur(NONE,infile) = (TextIO.closeIn(infile); [])
  | parseRecur(SOME char,infile) = 
      charCheck(char,[],infile,Char.isAlpha(char),TextIO.endOfStream(infile))
      @parseRecur(TextIO.input1(infile),infile)


(* Takes in a char and returns an array of what token(s) it is.
An operator returns a token of that operator.
If there is characters in word and the next character is
an operator, it tokenizes what is in word and returns an array 
with that and the operator.
If char is whitespace, it returns tokenized word.
If char is a letter, appends to word which is a char list and then
recursively calls charCheck for the next possible letter in the word.
If char is not an a-z or A-Z letter, it prints an error statement
and returns an ID("1") error token to be checked later.
Final 2 arguments are bools that say if the current char
is 1) a letter or not and 2) the last character in the instream. *)
and charCheck(#"=", word, infile, _, _) = 
        if null(word) then [EQ] else [ID(implode(rev(word))), EQ]
  | charCheck(#"+", word, infile, _, _) =
        if null(word) then [PL] else [ID(implode(rev(word))), PL]
  | charCheck(#"-", word, infile, _, _) =
        if null(word) then [MI] else [ID(implode(rev(word))), MI]
  | charCheck(#"*", word, infile, _, _) =
        if null(word) then [TI] else [ID(implode(rev(word))), TI]
  | charCheck(#"/", word, infile, _, _) =
        if null(word) then [DI] else [ID(implode(rev(word))), DI]
  | charCheck(#" ", word, infile, _, _) = 
        if null(word) then [] else [ID(implode(rev(word)))]
  | charCheck(char, word, infile, true, true) = 
        [ID(implode(rev(char::word)))] 
  | charCheck(char, word, infile, true, false) = 
        charCheck(Option.valOf(TextIO.input1(infile)),char::word,infile,Char.isAlpha(char),TextIO.endOfStream(infile))
  | charCheck(char, word, infile, false, _) =
        (print("Compilation error\n"); [ID("1")]);