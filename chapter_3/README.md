# Context-free grammar

* Token
** a word (collection of alphanumeric) is a token, an indivisible lexical unit of the language


* A formal grammar
** Is a system of rules called productions taht control the order in which words occur in a sentence


* The syntax of a sentence
Determines the relansthips between the words and pharese in a setence. That is the sintax of a langauge controls the strctuure of a sentence and notinh more

## Context-free grammar is system of definitions that can be used to break up a sentence into phrases solely on the basis of the sequence of string in the input sentence. A cibtext0free grannar sis issually represented in Backus-Naur form (BNF) in which porductions are represented as follows

part_of_speech -> definition

The -> operator (often represented as ::=) is the **is defined as** or **goes to** operator.

So a context free grammar is one in which all input sentencfes can e parsed strictly
on the bass of systax. Formally a context-free gramamr is composed of the following

1. A finite set of terminal symbols or tokens
2. A finiste set of non terminal symbols
3. A finite set of productions of the form S -> alpha where s is a non terminal symbol and alpha is a list of zero or mote terminal and non termimanl symbols, s is called the LHS of production and alpha is caleed the RHS. Every non terminal symbol that appears in the RHS in some production must algo appear on LHS. No terminal symbols may appear on LHHS
4. A single start or gola smybol from whch all the productions advice

Syntac rules are used to break a sentence into its components parts of speech and analyze the rlationship of one part to another (this is process is called **parsing** the sentence)
The termm is used in both linguititsic and compiler theory. A parser is a computer program that uses a context-free grammar to parse an input sentence --to isolate the components parts of speech for subsequent processing. Most parser programs are also recognizer programs; Theey accept (return yes) only if the input forms a syntaxcally correct senetence. The fact that a parser can alog generate code is issually inmaterial from a theoritical point of view.

**The parser can analyze only the strcuture of the sentence.**

_Semantics: meanings of the words_

Leftmost derivation
====================
Replace the leftmost non terminal in the partially parsed sentence with the equivalent
production's right hand side. Start with the goal symbol (the topmost symbol in the grammar) and the replace the leftmost non terminal at eac step

The foregoi nderivation wwas created in a top-down fashion, starting with the top most symbol in the grammar and working down the tokens. **This process is called a top down parse**

Rightmost derivation
=====================

You can also have a rght most derivation which starts with the goal symbol and replaces the rightmost non terminal with each steap. They sart with the nput symbols that comprise the right hand side with the equivalent left hand side. You still go thoruh the input from left tto right but you alwasys replace the right most handle in the viable freix
This is a bottom up parser, wihcih uses a right most dervation


LL and LR grammars
==================

An LL grammar is a grammar that ca be parsed by an LL parser. Recursive descent parsers are LL parsers. It goes through the input from left to right, doing a top-bottowm left most derivacion

An LR is goes thourgh the input from left to right, doing a bottom up right most dereivation


Grammar to DFA
==============
Grammers that can be transalto DFAs are called right linear gramaar. A grammar is right linear if the right hand side of each non erminal has a at most one non termnal in it, and that non terminal is at the far right of the right hand side

```
  a -> e
  a -> X b
```

Left recursive production
=========================
A left recusrive production is one wheere the left hand side appears as the 
leftmost symbol on the right hand side. Right recursive production is same but reversed

```
stmt_list -> stmt_list smmt | stmt
stmt -> A | B | C
```




