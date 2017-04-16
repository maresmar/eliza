% Eliza - main file
% Martin Mares <mmrmartin@gmail.com>

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%   Test methods   %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
test:-
	out("[toLowerCase] "),
	toLowerCase("A","a"),toLowerCase("5","5"),
	toLowerCase("b","b"),toLowerCase(" "," "),
	toLowerCase(",",",").
test:-
	out("[toUpperCase] "),
	toUpperCase("A","A"),toUpperCase("5","5"),
	toUpperCase("b","B"),toUpperCase(" "," "),
	toUpperCase(",",",").
test:-
	out("[delteChars] "),
	string_chars("Ahoj, jak se mas.?",In),
	string_chars("Ahoj jak se mas?",Out),
	deleteChars(In, punctuation, Out).
test:-
	out("[readWord] "),
	string_chars("Ahoj jak se mas	baf",In),
	Out=['Ahoj','jak','se','mas','baf'],
	toWords(In,Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Out/in interface %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% out(+Text):- prints output to some output stream
out(Text):-write(Text).

% in(+Text):- reads question from input stream
in(Text):-
	nl,
	write("> "),
	readLine(Text).

readLine(Text):-
	get_char(Char),
	toLowerCase(Char,LChar),
	readLine2(LChar,Text).
readLine2('\n',[]):-!.
readLine2(LChar,[LChar|T]):-readLine(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Basic utilities  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% constant predicates
charType('!', punctuation).
charType('?', punctuation).
charType('.', punctuation).
charType(',', punctuation).
charType('\'', punctuation).
charType(' ', whitespace).
charType('\t', whitespace).

% toLowerCase(+Char, -LChar):- lower case char (using ASCI codes)
toLowerCase(Char, LChar):-
	char_code(Char, Code),
	Code >= "A",
	Code =< "Z",
	NewCode is Code + 32,
	char_code(LChar, NewCode), !.
toLowerCase(Char, Char).

% toUpperCase(+Char, -UChar):- upper case char (using ASCI codes)
toUpperCase(Char, UChar):-
	char_code(Char, Code),
	Code >= "a",
	Code =< "z",
	NewCode is Code - 32,
	char_code(UChar, NewCode), !.
toUpperCase(Char, Char).

% deleteChars(+Line, -Type, -Res):- delete specific charType from line
deleteChars([Char|Rest],Type,Out):-
	charType(Char, Type),
	deleteChars(Rest,Type,Out),!.
deleteChars([Char|Rest],Type,[Char|Out]):-
	deleteChars(Rest,Type,Out),!.
deleteChars([],_,[]).

% toWords(+Line, -Words):- transfer output of readLine to list of words
toWords([],[]):-!.
toWords(Line, [Word|ResWords]):-
	readWord(Line, Word, ResLine),
	toWords(ResLine, ResWords).

% readWord(+Line, -Word, -ResLine) :- reads one word from line
% 	(the rest of line is returned in ResLine
readWord([], '', []).
readWord([Char|Res], '', Res) :- charType(Char, whitespace),!.
readWord([Char|ResLine], Word, Res) :- 
	readWord(ResLine, ResWord, Res),
	atom_concat(Char, ResWord, Word).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  Elisa function  %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic resID/2.
resID(_,0).

% init:- inits the environment for simplification rules
init:-
	consult("simplification.rules"),
	consult("reply.rules").	

% simplify(+In,-Out):- removes unnecessary characters eg. "," and "." 
% 	and simplify words
simplify(In, Out):-
	deleteChars(In, punctuation, Out1),
	toWords(Out1,Out2),
	findSynonyms(Out2,Out3),
	Out = Out3.

% findSynonyms(+Words, -Synonyms) :- finds synonyms using
% 	simplification rules (loaded by init function)
findSynonyms(Words, Syn) :-
	sr(Words, Syn, RestWords, ResOutput),!,
	findSynonyms(RestWords, ResOutput).
findSynonyms([Word| ResWords], [Word| ResSyn]):-
	findSynonyms(ResWords, ResSyn),!.
findSynonyms([], []).

% findReply(+Words, -Reply) :- finds reply with highest rank
% 	(loaded by init function)
findReply(Words, Reply) :-
	findReply2(Words, -2, 0, [], ID, Reply),
	ID \= 0,
	updateResID(ID).

% findReply2(+Words, +ActScore, +ActRuleID, +ActRes, -RuleID, -Res):- finds reply using two
%	accumulators
findReply2([H|T], ActScore, _, _, ID, Res):-
	findall(Score,rules(_, Score,[H|T],_),Rules),
	Rules \= [], % bagof doesn't work as I except
	max_list(Rules,NewScore),
	ActScore < NewScore,
	rules(NewID, NewScore,[H|T],Replyes),
	resID(NewID,ResID),
	nth0(ResID,Replyes,NewReply),
	findReply2(T, NewScore, NewID, NewReply, ID, Res),!.
findReply2([_|T], ActScore, ActID, ActRes, ID, Res):-
	findReply2(T, ActScore, ActID, ActRes, ID, Res).
findReply2([], _, ID, Res, ID, Res).

% updateResID(+ID):- moves to next reply for rule
updateResID(ID):-
	resID(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	retract((resID(ID,RID):-!)),
	asserta(resID(ID,NRID):-!),!.
updateResID(ID):-
	resID(ID,RID),
	once(rules(ID,_,_,Replyes)),
	length(Replyes, Len),
	NRID is (RID + 1) mod Len,
	asserta(resID(ID,NRID):-!).

% writeWords(+Words) - uppers first letter and writes words to output
writeWords([Word|Res]):-
	string_chars(Word,[Char|RChar]),
	toUpperCase(Char,UChar),
	readWord([UChar|RChar],Out,_),
	out(Out),
	writeWords2(Res).
% writes inner list
writeWords2([Word|Res]):-
	is_list(Word),
	writeWords2(Word),
	writeWords2(Res),!.
% writes punctuation
writeWords2([Word|Res]):-
	charType(Word,punctuation),
	out(Word),
	writeWords2(Res),!.
% writes standard char
writeWords2([Word|Res]):-
	out(" "),
	out(Word),
	writeWords2(Res),!.
writeWords2([]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%  Main function  %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elisa:-
	out("Looking for Elisa....\n"),
	init,
	out("Here she is...\n\n"),
	out("Hello, I\'m Elisa, how can I help you?"),
	elisa([hi]).

elisa([quit|_]):-!.
elisa(_):-
	in(Line),
	simplify(Line, Words),
	findReply(Words,Reply),
	writeWords(Reply),nl,
	elisa(Words).

	

