:- ensure_loaded('chat.pl').

% Returneaza true dacă regula dată ca argument se potriveste cu
% replica data de utilizator. Replica utilizatorului este
% reprezentata ca o lista de tokens. Are nevoie de
% memoria replicilor utilizatorului pentru a deduce emoția/tag-ul
% conversației.
% match_rule/3
% match_rule(_Tokens, _UserMemory, rule(_, _, _, _, _)) :-fail.

match_rule(Tokens, UserMemory, rule(Expr, _, _, Emotion, Tag)) :- Expr = Tokens,
                get_tag(UserMemory, Tag1), (Tag1 \= sport -> member(Tag1, [Tag]);
				(get_emotion(UserMemory, Emotion1),  (Emotion1 \= neutru -> member(Emotion1, Emotion); Expr = Tokens))).
												

% Primeste replica utilizatorului (ca lista de tokens) si o lista de
% reguli, iar folosind match_rule le filtrează doar pe cele care se
% potrivesc cu replica dată de utilizator.
% find_matching_rules/4
% find_matching_rules(+Tokens, +Rules, +UserMemory, -MatchingRules)
find_matching_rules(Tokens, Rules, UserMemory, MatchingRules) :- findall(rule(A,B,C,D,E), 
	(member(rule(A,B,C,D,E), Rules), match_rule(Tokens, UserMemory, rule(A, B, C, D, E))), MatchingRules).

% Intoarce in Answer replica lui Gigel. Selecteaza un set de reguli
% (folosind predicatul rules) pentru care cuvintele cheie se afla in
% replica utilizatorului, in ordine; pe setul de reguli foloseste
% find_matching_rules pentru a obtine un set de raspunsuri posibile.
% Dintre acestea selecteaza pe cea mai putin folosita in conversatie.
%
% Replica utilizatorului este primita in Tokens ca lista de tokens.
% Replica lui Gigel va fi intoarsa tot ca lista de tokens.
%
% UserMemory este memoria cu replicile utilizatorului, folosita pentru
% detectarea emotiei / tag-ului.
% BotMemory este memoria cu replicile lui Gigel și va si folosită pentru
% numararea numarului de utilizari ale unei replici.
%
% In Actions se vor intoarce actiunile de realizat de catre Gigel in
% urma replicii (e.g. exit).
%
% Hint: min_score, ord_subset, find_matching_rules

flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).

% select_answer/5
% select_answer(+Tokens, +UserMemory, +BotMemory, -Answer, -Actions)
select_answer(Tokens, UserMemory, BotMemory, Answer, Actions) :- 
	rules(Cuv_cheie, Set_Reg), ord_subset(Cuv_cheie, Tokens),
	find_matching_rules(Tokens, Set_Reg, UserMemory, MatchingRules),

	findall(Reply, (member(rule(A, Replies, _,_,_), MatchingRules), member(Reply, Replies)), List),
	findall((Reply, Use), (member(Reply, List), get_answer(Reply, BotMemory, Use)), List_Fin),
	min_element(List_Fin, Answer), add_answer(Answer, BotMemory, _),
	findall(Action, (member(rule(A, Replies, Action,_,_), MatchingRules), member(Answer, Replies)), Actions_Lst), flatten2(Actions_Lst, Actions) . 

% Esuează doar daca valoarea exit se afla in lista Actions.
% Altfel, returnează true.
% handle_actions/1
% handle_actions(+Actions)
handle_actions(Actions) :- \+ member(exit, Actions).


% Caută frecvența (numărul de apariți) al fiecarui cuvânt din fiecare
% cheie a memoriei.
% e.g
% ?- find_occurrences(memory{'joc tenis': 3, 'ma uit la box': 2, 'ma uit la un film': 4}, Result).
% Result = count{box:2, film:4, joc:3, la:6, ma:6, tenis:3, uit:6, un:4}.
% Observați ca de exemplu cuvântul tenis are 3 apariți deoarce replica
% din care face parte a fost spusă de 3 ori (are valoarea 3 în memorie).
% Recomandăm pentru usurința să folosiți înca un dicționar în care să tineți
% frecvențele cuvintelor, dar puteți modifica oricum structura, această funcție
% nu este testată direct.


% find_occurrences/2
% find_occurrences(+UserMemory, -Result)

% Atribuie un scor pentru fericire (de cate ori au fost folosit cuvinte din predicatul happy(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie fericit.
% get_happy_score/2
% get_happy_score(+UserMemory, -Score)
get_happy_score(UserMemory, Score) :- findall(Words, (dict_keys(UserMemory, Keys), member(Sentence, Keys), words(Sentence, Words)), List),
 									  findall(Val, (member(Reply, List), unwords(Reply, Key), get_value(UserMemory, Key, Val), 
 									  member(Word, Reply), happy(Word)), List_Fin), sumlist(List_Fin, Score).

% Atribuie un scor pentru tristețe (de cate ori au fost folosit cuvinte din predicatul sad(X))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să fie trist.
% get_sad_score/2
% get_sad_score(+UserMemory, -Score)
get_sad_score(UserMemory, Score) :-findall(Words, (dict_keys(UserMemory, Keys), member(Sentence, Keys), words(Sentence, Words)), List),
  								   findall(Val, (member(Reply, List), unwords(Reply, Key), get_value(UserMemory, Key, Val), 
 									member(Word, Reply), sad(Word)), List_Fin), sumlist(List_Fin, Score).

% Pe baza celor doua scoruri alege emoția utilizatorul: `fericit`/`trist`,
% sau `neutru` daca scorurile sunt egale.
% e.g:
% ?- get_emotion(memory{'sunt trist': 1}, Emotion).
% Emotion = trist.
% get_emotion/2
% get_emotion(+UserMemory, -Emotion)
get_emotion(UserMemory, Emotion) :-  get_sad_score(UserMemory, Sad_score), get_happy_score(UserMemory, Happy_score),
	 							   ((Sad_score == Happy_score, Emotion = 'neutru'); (Sad_score > Happy_score, Emotion = 'trist');
	 							    (Sad_score < Happy_score, Emotion = 'fericit')).

% Atribuie un scor pentru un Tag (de cate ori au fost folosit cuvinte din lista tag(Tag, Lista))
% cu cât scorul e mai mare cu atât e mai probabil ca utilizatorul să vorbească despre acel subiect.
% get_tag_score/3
% get_tag_score(+Tag, +UserMemory, -Score)
get_tag_score(Tag, UserMemory, Score) :-  tag(Tag, Lista_Tag), findall(Words, (dict_keys(UserMemory, Keys), member(Sentence, Keys), words(Sentence, Words)), List),
  								   findall(Val, (member(Reply, List), unwords(Reply, Key), get_value(UserMemory, Key, Val), 
 									member(Word, Reply), member(Word, Lista_Tag)), List_Fin), sumlist(List_Fin, Score).

% Pentru fiecare tag calculeaza scorul și îl alege pe cel cu scorul maxim.
% Dacă toate scorurile sunt 0 tag-ul va fi none.
% e.g:
% ?- get_tag(memory{'joc fotbal': 2, 'joc box': 3}, Tag).
% Tag = sport.
% get_tag/2
% get_tag(+UserMemory, -Tag)
get_tag(memory{}, none). 
get_tag(UserMemory, Tag) :- findall((Tag_1, Use), (member(Tag_1, [sport, film]), get_tag_score(Tag_1, UserMemory, Use)), List_Fin),
	                        max_element(List_Fin, Tag).
