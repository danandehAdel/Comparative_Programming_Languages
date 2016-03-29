father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

%Question 9: 
grandma(X,Y) :- mother(X, P),
                mother(P, Y).

%Question 10
%Base case
descendants(X,Y) :- mother(X,Y);
                    father(X,Y).

descendants(X,Y) :- father(X,Z),
                    descendants(Z,Y).

descendants(X,Y) :- mother(X,Z),
                    descendants(Z,Y).

%Question 11: 
%siblings: either share father or mother or both (includes half-brothers/sister)
%siblings2: has to share the same mother/father (no half-siblings)
siblings(X,Y) :- X\=Y,
                 (father(D, X),
                 father(D, Y);
                 mother(M, X),
                 mother(M, Y)).

siblings2(X,Y) :- X\=Y,
                 father(D, X),
                 father(D, Y),
                 mother(M, X),
                 mother(M, Y).
%Question 12: 
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).
accepting(q2).
accepting(q3).

accepts(State, []) :- accepting(State).
accepts(State, [X|Y]) :- transition(State, Q, X),
                         accepts(Q, Y).
                         
						 
