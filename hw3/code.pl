% CPSC 312 2021W1, Assignment #3: Animal Crossing Out
%
% There is a lot of provided code at the bottom, including most things that:
%
% + do input/output to the user (which uses non-logical predicates)
% + insert new clauses into the knowledge base (which uses non-logical predicates)
% + uses the special non-logical "cut" operator to directly interact with Prolog's
%   search strategy
%
% All that given code is worth reading but not required.
%
% However, your actual TODO items that are needed to make the game run are:
%
% + Parsing and manipulating yes-or-no responses: response/2 and negate_response/2
% + Setting up text for the program: make_win_text/2, loss_text/1, and
%   make_prop_text/3 (which is the only one of these that will take multiple clauses). 
% + asking the user for a yes-or-no answer: ask/2, and
% + guess/2: which requires understanding a bit about lists and Prolog "functions"
%   (which are not functions in the normal sense at all but compound terms).
%
% Your TODO items that are required for the assignment but not for the game are:
%
% + both_animal/1,
% + filled_script/1,
% + ancestor/2, and
% + hole/2 and hole/3.



:- use_module(library(readutil)).
:- use_module(library(gensym)).

% Making everything that our users may define dynamic.
% Also make our todo marker dynamic so it's easy to find what you need to do
% but the file will still load.
:- dynamic script/4, animal/3, todo/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The initial animal/script database.
%% Feel free to improve. Playing the game
%% is a good way to add new clauses to the KB! :)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% animal(Animal, PropName, YesOrNo) indicates that the property
% PropName holds (if YesOrNo = yes) or does not (if YesOrNo = no)
% for Animal.
animal("an elephant","larger than a breadbox",yes).
animal("an elephant","a mammal",yes).
animal("a sengi","larger than a breadbox",no).
animal("a komodo dragon","larger than a breadbox",yes).
animal("a komodo dragon","a mammal",no).

% script(Node, PropName, YesNode, NoNode) represents a step in our 20
% questions script. Its ID is Node, this step of the script describes
% asking if PropName holds and proceeding to node YesNode if so and to
% node NoNode if not.
script(start,"larger than a breadbox",bbyes,bbno).
script(bbyes,"a mammal",mmyes,mmno).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Your TODO items required to make the game work.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Response Mapping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% response(String, YesOrNo) is true
% if String corresponds to YesOrNo, where
% YesOrNo will be the constant yes or the
% constant no.
%
% Your solution must handle "yes" and "no"
% and your solution must not translate
% anything that is clearly not positive or negative as
% far as your code is concerned (like "maybe" or
% "Fruit flies like a banana."). You're free to have
% LOTS more positive or negative cases besides "yes"
% and "no"... but leave those to the bonus!
response("yes", yes). % Try just a couple of facts!
response("no", no).

% negate_response(YesOrNo, Negated) is true
% if YesOrNo is yes or no and Negated is the
% opposite.
negate_response(yes, no). % Again, try just a couple of facts!
negate_response(no, yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Text (some of which is given)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% make_win_text takes an animal name (Answer) and makes as
% Text a statement celebrating winning with Answer as the guess.
%
% for example, make_win_text("a sengi", "Yay! It is a sengi!")
make_win_text(Answer, Text) :- format(string(Text), "Yay! It is ~w!", [Answer]).

% loss_text(S) is true if S is our statement indicating we give up.
% Our loss text is: "Oh no. I give up!"
loss_text("Oh no. I give up!").  % Just a single fact should do nicely.




% make_prop_text(YesOrNo, Stem, Text) is true if Text is suitable
% text to indicate whether or not the property Stem holds, which
% it does if YesOrNo is yes and does not if YesOrNo is no.
%
% For example, make_prop_text(yes, "bigger than a breadbox", "bigger than a breadbox"),
% but make_prop_text(no, "a mammal", "not a mammal").
%
% Do NOT use format/3 for this. Use string_concat/3 instead.
% string_concat(S1, S2, S3) is true if S3 is equal to S1 + S2
% (and all three are strings).
make_prop_text(yes, S, S). % You will probably want two clauses!
make_prop_text(no, S, T) :- string_concat("not ", S, T).



%%%%%%%%%%%%%% The rest of the text utilities are given %%%%%%%%%%%%%%%%%
% (You can find the next todo in the section on "Get a yes-or-no answer".)

% anon_pronoun(S) is true if S is the anonymous animal pronoun (it).
anon_pronoun("it").

% make_question(Animal, QStem, Question) takes an Animal name
% and a question stem (QStem, a property name) and produces the
% as Question the text of the question to ask about that animal.
make_question(Animal, QStem, Question) :-
    format(string(Question), "Is ~w ~w?", [Animal, QStem]).

% make_question/2 works just like make_question/3 except it
% uses the anonymous animal pronoun (from anon_pronoun/1) as the
% animal name.
make_question(QStem, Question) :- 
    anon_pronoun(It), make_question(It, QStem, Question).



% Capitalize(Lower, Upper) takes any string as Lower
% and produces as Upper that string but with its first
% letter uppercase (regardless of whether it was lower-
% or upper-case before).
capitalize("", "").
capitalize(Lower, Upper) :-
    string_codes(Lower, [FLower|Rest]),
    to_upper(FLower, FUpper),
    string_codes(Upper, [FUpper|Rest]).

% intro_prompt(S) is true if S is the intro prompt for our game.
intro_prompt("Think of an animal you know a lot about.\nWhen you have it in mind, hit enter.").

% ok_reply(S) is true if S is our reply indicating "OK".
ok_reply("OK.").

% loss_answer_prompt(S) is true if S is our prompt for what the
% user's animal was when we give up.
loss_answer_prompt("What is your animal?").

% make_your_animal_text(Animal, Text) takes an animal name (Animal)
% and produces as Text a statement that Animal was the user's animal.
make_your_animal_text(Animal, Text) :- 
    format(string(Text), "Your animal is ~w.", [Animal]).

% new_yes_or_no_prompt(Animal1, Animal2, Text) takes two animal names
% and produces as Text a prompt asking the user for a yes-or-no question
% to distinguish them.
new_yes_or_no_prompt(Animal1, Animal2, Text) :-
    format(string(Text), "Finish this prompt with a yes-or-no question that would let me tell the difference between ~w and ~w.\n(Don't add a question mark at the end.)", [Animal1, Animal2]).

% new_yes_or_no_start(S) is true if S is the start to our
% standard form of yes-or-no question.
new_yes_or_no_start("Is it... ").

% i_can_ask_start(S) is true if S indicates the start of saying we can
% ask a sample question.
i_can_ask_start("I can ask: ").

% info_prop(Animal, YesOrNo, Stem, Text) takes an animal name,
% a property (whether it holds as YesOrNo and its name as Stem)
% and produces as Text a statement that the animal has the property
% (adjusted for whether it has the property holding or not holding).
% E.g., info_prop("a sengi", no, "larger than a breadbox", "a sengi is not larger than a breadbox").
info_prop(Animal, YesOrNo, Stem, Text) :-
    make_prop_text(YesOrNo, Stem, AText),
    capitalize(Animal, UAnimal),
    format(string(Text), "~w is ~w.", [UAnimal, AText]).


% info_props(Animal, Props, Text) constructs in Text
% a description of how Animal has each property in the
% list Props. (This isn't tail recursive but really
% should be!)
info_props(_, [], "") :- !.
info_props(Animal, [prop(YesOrNo, Prop)|Props], Text) :-
    info_prop(Animal, YesOrNo, Prop, OneText),
    info_props(Animal, Props, RestText),
    format(string(Text), "~w\n~w", [OneText, RestText]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Get a yes-or-no answer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ask(Question, Response): given a string Question,
% uses writeln/1 to write the question out to the user
% and read_line_to_string(current_input, Line) to read
% a reply from the user and process it.
% 
% Response should be yes if the user answered positively
% and no if they answered negatively. ask/2 just uses
% response/2 to determine if an answer is a yes or a no
% answer.
ask(Question, Response) :-
    writeln(Question), read_line_to_string(current_input, Input), response(Input, Response).  % A first clause for if the user answers yes or no.
ask(Question, Response) :-
    writeln("Please answer yes/no."), ask(Question, Response).  % A second (recursive) clause for if they answer something else.
           % In that case, we'll let them know we need a yes/no answer
           % and then start over again.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Find the Animal to Guess
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% guess(Animal, Props): given a list of Properties, produces an
% Animal that has those properties.
%
% The list of properties is of the form:
% [prop(yes, "any property"), prop(no, "any other property"), ...]
% i.e., a list of any length where each entry has a compound term
% named prop with either yes or no as its first argument and any string
% as its second. 
% 
% We've provided heads for the two rules you need already.
% The first rule should match any animal. In the second rule,
% notice how pattern-matching helps us break down the list and compound term
% and pull out the pieces we want. YesOrNo is the first property's
% "Holds?" value. Prop is the first property's name. Properties
% is the list of remaining properties, which may be empty.
%
% In the second rule, you need to find an Animal that both has
% the given property (with its "Holds?" value equal to YesOrNo)
% AND that matches all the remaining properties. If only you had
% some predicate that could already insist an animal matches a list
% of properties! (Recursion?)
guess(Animal, []) :- animal(Animal, _, _).
guess(Animal, [prop(YesOrNo, Prop)|Properties]) :- animal(Animal, Prop, YesOrNo), guess(Animal, Properties).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Your TODO items that are NOT required to make the game work.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% + filled_script/1, and
% + hole/2 and hole/3, 
%   for which we have given some meta-logical helpers.


% Find an animal with both "yes" and "no" properties.
both_animal(Animal) :- animal(Animal, _, yes), animal(Animal, _, no).

% Find all the script nodes for which there is an animal that has its stem as a yes
% property and there is an animal that has its stem as a no property.
filled_script(Node) :- script(Node, Stem, _, _), animal(_, Stem, yes), animal(_, Stem, no).

% Given any script node, find each of its "ancestors" in the script in turn, 
% starting with the first (the node with the ID start in our usual DB).
% That is: each node that comes before it in the script plus the node itself.
% (Yes, we are saying a node is its own ancestor. Oddly, a node is its own
% ancestor EVEN IF IT DOES NOT APPEAR IN script/4.)
%
% What is the base case here?
% Then, how many recursive cases do you have? (Look carefully at script/4!)
ancestor(Node, Node).
ancestor(Node, AncestorNode) :- script(AncestorNode, _, SomeNode, _), ancestor(Node, SomeNode).
ancestor(Node, AncestorNode) :- script(AncestorNode, _, _, SomeNode), ancestor(Node, SomeNode).


% hole(Node, HoleNode) is true if HoleNode is reachable from Node (via
% the script) and is a "hole". A "hole" is a node where there are NO animals
% with all the properties needed to get to that point.
%
% (hole(PossibleAnimals, Node, HoleNode) is just like hole(Node, HoleNode)
% except that it only considers the list of animals in PossibleAnimals, not
% all the animals in the DB.)
%
% Here are some examples of how hole/2 works. First, imagine we left the
% komodo dragon out of our initial KB. So, we'd have only this in our DB:
%
%  animal("an elephant","larger than a breadbox",yes).
%  animal("an elephant","a mammal",yes).
%  animal("a sengi","larger than a breadbox",no).
%  script(start,"larger than a breadbox",bbyes,bbno).
%  script(bbyes,"a mammal",mmyes,mmno).
%  
% Then, hole(start, mmno) would be true but no other hole(start, X) would be.
% That's because we can start at start with elephant and sengi as
% possible animals (so start is not a hole).  According to our script,
% we ask about "larger than a breadbox" at start.  Along the no path is
% the script node bbno. We still have sengi as a possible animal; so,
% bbno is not a hole. There is no script entry for bbno; so, we have
% nowhere else to search. Along the yes path is the script node
% bbyes. We still have the elephant as a possible animal along that
% path. So, that's not a hole. However, there's another script line for
% bbyes asking about whether it is a mammal. Along *that* yes path is
% mmyes, for which we still have elephant as a possible animal. So, it's
% not a hole either (and it has no script line so there's nowhere else
% to search on that path).
% 
% However, along the no path is mmno, and since we deleted komodo
% dragon, there are no possible animals left, and mmno is a hole!
% 
% (In our original DB, we'd still have komodo dragon; so, nothing would
% be a hole. Holes are bad news for our game; so, it's good that our
% original DB doesn't have any!)
% 
% If we went back to our original DB but deleted all the lines about
% komodo dragons and elephants being larger than a breadbox, we'd
% get:
% 
%  animal("an elephant","a mammal",yes).
%  animal("a komodo dragon","a mammal",no).
%  animal("a sengi","larger than a breadbox",no).
%  script(start,"larger than a breadbox",bbyes,bbno).
%  script(bbyes,"a mammal",mmyes,mmno).
%
% In this case, hole(start, bbyes) is true because we don't know that
% any animal is larger than a breadbox. We can also follow the script
% farther to find that hole(start, mmyes) and hole(start, mmno) are true
% (which they are because we already ran out of possible animals at bbyes).
%
% There's an odd special case here. If there are no animals at all in the DB,
% (or below for hole/3, if we simply provide no animals in the list of
% PossibleAnimals) then anything (whether or not it exists in the script)
% is a HoleNode. This should strongly suggest the base case for hole/3!
% Be careful, however, that is NOT the same as how you want to call hole/3
% from hole/2. (You don't want to always hit the base case right away!)
%
% You will need to know that an empty list looks like [].
%
% There are four related helpers below provided to you: animal/1,
% all_animals/1, candidate_animal/4, and filter_animals/4. You will
% only need to use all_animals/1 and filter_animals/4 in your two
% versions of hole (but you should find animal/1 and 
% candidate_animal/4 are predicates you could have written).

% Hint: Just define hole/2 in terms of hole/3. The all_animals/1 helper
% below is likely to be quite helpful here!
hole(Node, HoleNode) :- hole(Animals, Node, HoleNode), all_animals(Animals).

% Hints:
% + You'll want a base case and two recursive cases.
% + For the base case, what list of PossibleAnimals might indicate
%   you're done looking? Your base case should be VERY simple.
% + For your recursive cases, there are two ways to "go" from a script
%   node that has an entry in the script table. That entry also tells
%   you how to "cut down" the list of PossibleAnimals. The filter_animals/4
%   helper will help here! The two recursive cases will look very similar,
%   but write them as separate rules.
%
% Notes:
% + You should assume that PossibleAnimals will always be "ground".
%   That means it will not be a variable or have a variable anywhere
%   inside of it. (It will be a list of animal names, although those
%   names may or may not appear in the animal table.)
% + Except in the base case, you may also assume Node will be ground.
%   That shouldn't matter to you in the base case.
hole([], Node, HoleNode).
hole(PossibleAnimals, Node, HoleNode) :- ancestor(HoleNode, Node).


% This is a bit tricky; so, we're giving you two "meta-predicates" (and two regular
% predicate, one of which uses the built-in member/2 predicate) to help.
%
% A meta-predicate in Prolog is a bit like a non-pure function in Haskell:
% something that doesn't really behave according to the logical semantics
% we've been learning at all. (The only such thing we ran across briefly in
% Haskell was the terrifying unsafePerformIO, which we happily never used.)

% animal/1 is just a "type" predicate to determine if something is an animal.
% It's a normal predicate.
animal(A) :- animal(A, _, _).

% Get a list of all the animals. findall/3 is the meta-predicate we rely on.
% Read findall(Variable, Goal, Result) as "find all values of Variable such
% that Goal succeeds and collect them in Result". sort/2 is built-in and
% both sorts and removes duplicates from a list.
all_animals(Animals) :- findall(A, animal(A), DupAnimals), sort(DupAnimals, Animals).

% This is a normal predicate. candidate_animal(Animals, YesOrNo, Property, Animal) is
% true if Animal is one of Animals, and it has Property with value YesOrNo.
% It uses the (normal) built-in predicate member/2.
candidate_animal(Animals, YesOrNo, Property, Animal) :- 
    member(Animal, Animals), animal(Animal, Property, YesOrNo).

% filter_animals(InitAnimals, YesOrNo, Property, ResultAnimals) is true if ResultAnimals
% are those animals that have Property with the value YesOrNo (which must be the atom
% yes or the atom no) from among those in InitAnimals. It also uses findall/3.
filter_animals(InitAnimals, YesOrNo, Property, ResultAnimals) :-
    findall(R, candidate_animal(InitAnimals, YesOrNo, Property, R), ResultAnimals).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PROVIDED CODE. No need to change any of this.
%% The two atoms you may want to use are play/0 
%% (to play the game!) and dump_all/0 (to output
%% all the learned knowledge suitable for pasting
%% back into the KB.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Animal/Script Table Output.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% dump_animals/0 writes out everything in animal/3 to the user.
% It uses the special predicate fail/0 that automatically fails
% in a perhaps-creative way.
dump_animals :-
    animal(Animal, Stem, YesOrNo),
    write_term(animal(Animal, Stem, YesOrNo), [quoted, fullstop, nl]), 
    fail.
dump_animals.

% dump_script/0 similarly writes out everything in script/4.
dump_script :-
    script(Node, Stem, YNode, NNode),
    write_term(script(Node, Stem, YNode, NNode), [quoted, fullstop, nl]), 
    fail.
dump_script.

% dump_all/0 dumps the animals and the script.
dump_all :- dump_animals, fail.
dump_all :- dump_script, fail.
dump_all.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The core game.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% play/0 plays a full round of the game.
play :- 
    % Write out the game intro and wait for the user to hit enter.
    intro_prompt(Prompt), writeln(Prompt), read_line_to_string(current_input, _),

    % Play the game, starting with no properties of the animal known.
    play_game_from(start, []),

    % Let the user know how to get the new KB.
    nl, writeln("Use 'dump_all.' to get the script and animals if you want!").


% Continue the game given the answer, a yes node, and a no node to work with.
continue_game(yes, YNode, _, Props) :- play_game_from(YNode, Props).
continue_game(no, _, NNode, Props) :- play_game_from(NNode, Props).

% Play the game, starting from the given node and with the given
% list of properties so far known about the animal.
% 
% The "!" or "cut" has an interesting effect.
% It itself automatically succeeds (call goes straight to exit),
% but any attempt to retry a cut immediately causes the entire atom
% being defined to fail (here the atom is play_game_from). (Not just 
% THIS rule defining the atom fails, but all rules/facts. The cut's 
% fail port is tied directly to the atom's fail port, skipping
% everything else in the atom's box.
play_game_from(Node, Props) :-
    % Find our node in the script.
    % If there is one, commit to it with the "cut".
    script(Node, QStem, YNode, NNode), !,

    % Ask the node's question. Once we have an answer, commit to it.
    make_question(QStem, Question), ask(Question, Response), !,

    % Add the response about the animal to the list.
    NewProps = [prop(Response, QStem)|Props],

    % Remind the user what we know so far.
    ok_reply(OK), writeln(OK),
    anon_pronoun(It), info_props(It, NewProps, Text), writeln(Text),

    % Continue the game from the appropriate point.
    continue_game(Response, YNode, NNode, NewProps).

% Because of the strange "cut" thing, if we get to this rule,
% it's because there was no script node matching Node.
% So, end the game by:
% + making a guess, and
% + if it fails: give up and  giving up and building a new script node.
play_game_from(Node, Props) :-
    % Figure out what animal to guess and ask if it is correct.
    % We use cut to commit to the user's answer.
    guess(Animal, Props), make_question(Animal, Question), ask(Question, Response), !,

    % End the game based on the user's response.
    end_game(Node, Animal, Props, Response).

% If we get to here, then guess(Animal, Props) failed. We have no animal to guess!
play_game_from(_, _) :-
    writeln("Uh-oh. This isn't supposed to happen. I don't have a guess!"),

    % Special atom that automatically fails.
    fail.

% end_game(Node, Animal, Props, Result): ends the game. If Result is yes, we won!
% Tell the world and stop. If Result is no, we lost. Tell the user and then learn
% from the result.
end_game(_, Animal, _, yes) :-
    % We won!
    make_win_text(Animal, WinText), writeln(WinText).
end_game(Node, Animal, Props, no) :-
    % We lost.
    loss_text(LossText), writeln(LossText), learn(Node, Animal, Props).

% insert_all(Animal, Props): inserts into the animal/3 table all the properties
% given about Animal. Uses the special meta-predicate assert/1 to add facts to
% the KB dynamically.
insert_all(_, []).
insert_all(Animal, [prop(YesOrNo, Stem)|Props]) :-
    assert(animal(Animal, Stem, YesOrNo)),
    insert_all(Animal, Props).

% learn(Node, Animal, Props): get the user to give us a question that distinguishes
% their animal and ours and then add all the information we know about their animal
% plus the one new fact about ours, and a new script node to ask the new question.
learn(Node, Animal, Props) :-
    % Explain the loss and ask for the user's animal.
    loss_answer_prompt(APrompt), writeln(APrompt),
    read_line_to_string(current_input, YourAnimal), nl, 
    info_props(YourAnimal, Props, YourText), writeln(YourText), nl,
    info_props(Animal, Props, OurText), writeln(OurText), nl,

    % Get a new question to ask to distinguish the animals.
    new_yes_or_no_prompt(YourAnimal, Animal, YNPrompt), writeln(YNPrompt), nl,
    new_yes_or_no_start(YNStart), write(YNStart), read_line_to_string(current_input, NewStem),
    ok_reply(OK), writeln(OK),
    i_can_ask_start(ICanAsk), write(ICanAsk), make_question(NewStem, NewQ), writeln(NewQ), nl,

    % Determine whether their animal has the given property.
    make_question(YourAnimal, NewStem, YourNewPropQ), ask(YourNewPropQ, Response), !,

    % Update the knowledge base with the animal properties and new script node.
    insert_all(YourAnimal, [prop(Response, NewStem)|Props]),
    negate_response(Response, NegResponse), assert(animal(Animal, NewStem, NegResponse)),
    gensym(script_node, YesNode), gensym(script_node, NoNode),
    assert(script(Node, NewStem, YesNode, NoNode)),

    % Write out the key new knowledge in the knowledge base.
    info_prop(YourAnimal, Response, NewStem, YAStmt), writeln(YAStmt), 
    info_prop(Animal, NegResponse, NewStem, OAStmt), writeln(OAStmt), nl.
    


