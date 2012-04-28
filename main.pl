:- dynamic isFree/1.

:- dynamic harePosition/1.

:- dynamic dogsPosition/3.


%dlzka cakania na odpoved v sekundach
timeout(1000).

  %INICIALIZACE A MAZANI
%----------------------------------------------
initGround :- assert(isFree(2)), assert(isFree(3)),
  assert(isFree(5)), assert(isFree(6)), assert(isFree(7)), 
  assert(isFree(10)), assert(isFree(11)).

initHare :- assert(harePosition(8)).

initDogs :- assert(dogsPosition(1,4,9)).

initAll :- initGround, initHare, initDogs.
%-----------------------------------------------
clearGround :- isFree(X), 
              retract(isFree(X)),
              fail.
clearHare :- harePosition(X), 
              retract(harePosition(X)),
              fail.
clearDogs :- dogsPosition(X,Y,Z), 
              retract(dogsPosition(X,Y,Z)),
              fail.
              
clearAll :- clearGround ; clearHare ; clearDogs ; true.
%-----------------------------------------------

% PROPOJENI MEZI POLICKY PO KTERYCH CHODI KRALIK
connect(4,1). connect(4,5). connect(4,9).              
connect(1,2). connect(1,5). connect(1,4). connect(1,6).
connect(5,1). connect(5,4). connect(5,6). connect(5,9).
connect(9,4). connect(9,5). connect(9,6). connect(9,10).            
connect(2,1). connect(2,6). connect(2,3). 
connect(6,1). connect(6,2). connect(6,3). connect(6,5).
connect(6,7). connect(6,9). connect(6,10). connect(6,11). 
connect(10,9). connect(10,6). connect(10,11).
connect(3,2). connect(3,6). connect(3,7). connect(3,8).
connect(7,3). connect(7,6). connect(7,8). connect(7,11).
connect(11,10). connect(11,6). connect(11,7). connect(11,8).
connect(8,3). connect(8,7). connect(8,11).

%PROPOJENI MEZI POLICKY PO KTERYCH CHODI PSY
connectD(4,1). connectD(4,5). connectD(4,9).              
connectD(1,2). connectD(1,5). connectD(1,6).
connectD(5,1). connectD(5,6). connectD(5,9).
connectD(9,5). connectD(9,6). connectD(9,10).            
connectD(2,6). connectD(2,3). 
connectD(6,2). connectD(6,3). connectD(6,11).
connectD(6,7). connectD(6,10).  
connectD(10,6). connectD(10,11).
connectD(3,7). connectD(3,8).
connectD(7,3). connectD(7,8). connectD(7,11).
connectD(11,7). connectD(11,8).

% zisti ci je niektora pozicia pred zvolenou poziciou, nic pekne ale funguje
isBefore(4,_).% je prva pred vsetkyma
isBefore(8,_) :- !,fail. % je posledna
isBefore(1,4) :- !,fail.
isBefore(1,5) :- !,fail.
isBefore(1,9) :- !,fail.
isBefore(1,_) :- !.
isBefore(5,4) :- !,fail.
isBefore(5,1) :- !,fail.
isBefore(5,9) :- !,fail.
isBefore(5,_).
isBefore(9,4) :- !,fail.
isBefore(9,1) :- !,fail.
isBefore(9,5) :- !,fail.
isBefore(9,_) :- !.
isBefore(2,3) :- !.
isBefore(2,7) :- !.
isBefore(2,11) :- !.
isBefore(2,8) :- !.
isBefore(2,_) :- !,fail.
isBefore(6,3) :- !.
isBefore(6,7) :- !.
isBefore(6,11) :- !.
isBefore(6,8) :- !.
isBefore(6,_) :- !,fail.
isBefore(10,3) :- !.
isBefore(10,7) :- !.
isBefore(10,11) :- !.
isBefore(10,8) :- !.
isBefore(10,_) :- !,fail.
isBefore(3,8) :- !.
isBefore(3,_) :- !,fail.
isBefore(7,8) :- !.
isBefore(7,_) :- !,fail.
isBefore(11,8) :- !.
isBefore(11,_) :- !,fail.


% zisti ci zajac nevyhral
isHareWin :-
	dogsPosition(Dog1,Dog2,Dog3),
	harePosition(Hare),
	isBefore(Hare,Dog1),
	isBefore(Hare,Dog2),
	isBefore(Hare,Dog3).

% treba dorobit podminku ked vyhraju psy
isDogsWin :- fail.

	
	
%POHYB KRALIKA A PSU
%------------------------------------------
moveHare(V) :-
  harePosition(U),    %ziskani pozice kralika
  connect(U,V),       %kontrola zda existuje cesta mezi uzly
  isFree(V),          %kontrola jesli je uzel volny
  !,
  assert(isFree(U)),
  retract(isFree(V)),      %upraveni zaznamu v databazi
  assert(harePosition(V)),
  retract(harePosition(U)).
  
  
  
moveDogs(I,V) :-
                  %ziskani aktualni pozice psu
  (I = 1 ->  dogsPosition(U,X,Y) 
      ;       ( I = 2 -> dogsPosition(X,U,Y)
              ;              (I = 3 -> dogsPosition(X,Y,U)                               
                              )
              )
  ),
  connectD(U,V),    %kontrola zda je cesta mezi dvema uzly
  isFree(V),        %kontorola jestli uzel neni obsazenej
  !,
  assert(isFree(U)),      %pridani noveho a odebrani stareho uzlu z databaze
  retract(isFree(V)),
                    %uprava aktualnich pozic psu
  (I = 1 ->   assert(dogsPosition(V,X,Y)), retract(dogsPosition(U,X,Y)) 
      ;       ( I = 2 ->  assert(dogsPosition(X,V,Y)), retract(dogsPosition(X,U,Y))
              ;              (I = 3 ->  assert(dogsPosition(X,Y,V)), retract(dogsPosition(X,Y,U))                               
                              )
              )
  ). 
%-------------------------------------------
 
convertInput(InputChar,ConvertedChar) :-
	atom_codes(InputChar,[Code|_]),
	(          
          Code == 65 -> ConvertedChar = '10' %je to A
          ;
          Code == 66 -> ConvertedChar = '11' %je to B
          ;
          ConvertedChar = InputChar
        ).
 
%kontrola o jakou zpravu se jedna                 
isHare(Name,Body,Field) :- Name = 'HARE', 
          sub_string(Body,0,_,0,Nmbr),
          convertInput(Nmbr,CNmbr),
          atom_number(CNmbr,Field).
                  
          
isDog(Name,Body,ID,Field) :- Name = 'DOG', 
          sub_string(Body,0,1,_,Idx), atom_number(Idx,ID),
          sub_string(Body,2,_,0,Nmbr), 
          convertInput(Nmbr,CNmbr),
          atom_number(CNmbr,Field).
isPositions(Name) :- Name = 'POS'.
isQuit(Name) :- Name = 'QUIT'.


%vraci prvni prvek ze seznamu
getFirst(X,[X]) :- !.
getFirst(X,[X|_]).

%vraci posledni prvek ze seznamu
getLast(X,[X]) :- !.
getLast(X,[_|L]) :- getLast(X,L).  

%test na EOF alebo LF.
isEOFEOL(Char) :-
	Char == end_of_file, halt; 
	(char_code(Char,Code), Code==10).

%cita riadok zo vstupu ukonceny LF alebo EOF.
read_line(Line) :-
	get_char(Char),
	(isEOFEOL(Char), Line = [], !;
		read_line(LL), atom_codes(Char,[Cd]),
		[Cd|LL] = Line).

%HLAVNI FUNKCE

% sluzi len na testovanie prikazov
test :-	
	clearAll,
	initAll,
	read_line(Line),%precita riadok a v vrati text ako kody znakov
	skipLastItem(Line,Edited),%odstrani ; zo vstupu
	atom_codes(StringMessage,Edited),%prevedie spat na text, aby fungovala make_term
	make_term(StringMessage,List),
	getFirst(Name,List),              %do Name se ulozi prvni prvek pole	
    	getLast(Body,List),               %do Body se ulozi druhy a zaroven posledni prvek
	(
		isDog(Name,Body,ID,Field) -> write(Name),nl,write(ID),nl,write(Field),moveDogs(ID,Field)
			;
			(
			isHare(Name,Body,Field) -> write(Name),nl,write(Field),moveHare(Field)
			)
				;
				(
				isQuit(Name) -> write(Name)
				)
					;
					(
					isPositions(Name) -> writePositions						
					)
		
	).
	
% vypise sucasne pozicie
writePositions :- 
	harePosition(Hare),
	dogsPosition(X,Y,Z),
	write('Hare:'),
	write(Hare),
	nl,
	write('Dog 1:'),
	write(X),
	nl,
	write('Dog 2:'),
	write(Y),
	nl,
	write('Dog 3:'),
	write(Z),
	nl.
 

isWinner :- 
	isHareWin -> write('Hare WIN'),nl,write('Starting new game'),nl,start;
    	isDogsWin -> write('Dogs WIN'),nl,write('Starting new game'),nl,start.


% inicializuje hracie pole a spusti hru 
start :- 
    clearAll,
    initAll,
    %zistenie kdo za koho hra
    read_line(Line),		         
    skipLastItem(Line,Edited),	      
    atom_codes(StringMessage,Edited), 	
    make_term(StringMessage,List),    
    getFirst(Name,List), 
    getLast(Body,List),
    (
    Name = 'START' -> write('HARE:8;'),nl,write('Zadajte pozici psa'),nl,play(1)% 1 znamena ze AI hraje za zajaca
    ;
	    (
	    isHare(Name,Body,_) -> update_dog_position,write('Zadajte pozici zajice'),nl,play(-1) % -1 AI hraje za psov
	    ;
	    fail
	    )
    ).    
 
% hra
play(HareOrDogs) :-    
    get_time(TimeStart),
    read_line(Line),		      %precita riadok a v vrati text ako kody znakov       
    skipLastItem(Line,Edited),	      %odstrani ; zo vstupu    
    atom_codes(StringMessage,Edited), %prevedie spat na text, aby fungovala make_term    	
    make_term(StringMessage,List),    %rozdelime zpravu do pole (napr zpravu 'HARE:3', rozdelime do pole ['HARE', '3']    
    getFirst(Name,List),              %do Name se ulozi prvni prvek pole    
    getLast(Body,List),               %do Body se ulozi druhy a zaroven posledni prvek
    (isQuit(Name) -> !,fail	      %pokud je zprava 'QUIT' dochazi k ukonceni aplikace
	    ;
	    (                		      %jinak se kontroluje zda jde o zpravu pro psy nebo kralika
		    %je to pes a je to validny tah
		    isDog(Name,Body,ID,Field),HareOrDogs = 1 ->  moveDogs(ID,Field),update_hare_position
		    ;
		    (
			    %je to zajac a je to validny tah              
			    isHare(Name,Body,Field),HareOrDogs = -1 ->  moveHare(Field),update_dog_position
			    ;
			    (
				    isPositions(Name) -> writePositions
				    ;
				    (
				    write('invalid move'),nl,(HareOrDogs = 1,write('dogs win'),nl;write('hare win'),nl)
				    )
			    )
		    )
	    )	   
    ),    
    get_time(TimeEnd),
    (
	    ResultTime is TimeEnd - TimeStart,
	    timeout(Timeout),
	    ResultTime > Timeout -> write('waiting time exceeded'),nl,start% TODO: treba vypisat vitaza
	    ;	      
	    play(HareOrDogs)	    
	    
    ).


  
  %ROZDELENI RETEZCE
%to sem nasel na netu a jen to pouzil 
make_term(StringArgs, Term) :-    
    split_atom(StringArgs, ':', Args),
    Term =.. [Args].        

split_atom(A, E, L) :-
    atom_chars(A, C),
    split_atom2(C, E, L).

split_atom2([], _, []).
split_atom2(C, E, [A|L]) :-
    append(C0, [E|C1], C), !,
    atom_chars(A, C0),
    split_atom2(C1, E, L).
split_atom2(C, _, [A]) :-
    atom_chars(A, C).
  
% vynecha posledny znak, vyuzije sa na odstranenie ;
skipLastItem([_],[ ]) :- !.
skipLastItem([H|T],[H|S]) :-
    skipLastItem(T,S).  
   
   
%ZAJIMAVY FCE....
%between(1, 2, Count). 


%vstup aplikacie
prolog :-
	prompt(_, ''),
	start.
  
%///////////////////////////
:- dynamic bestField/2.
bestField(0,0).

findBestField(ID) :- connect(8,Y),
                    isFree(Y),
                    bestField(ID,FVal),
                    value(Y,Val),
                    Val > FVal, 
                    retract(bestField(ID,FVal)),
                    assert(bestField(Y,Val)),                              
                    fail.
findBestField(ID) :- bestField(ID,_).   

% ohodnocovacia funkcia
evalPos(1,40).%prva pozicia je pre zajaca vitaztvo a pre psov je nepodstatna
evalPos(2,40).
evalPos(3,40).
evalPos(4,99).
evalPos(5,90).
evalPos(6,99).%najviac moznosti pohybu
evalPos(7,80).
evalPos(8,50).
evalPos(9,60).
evalPos(10,50).
evalPos(11,40).


% zoznam platnych pohybov pre zajaca a psov
% pohyby pre zajaca
move(Pos,NextPos,1) :-
	connect(Pos,NextPos),
	isFree(NextPos).
	
% pohyby pre psa
move(Pos,NextPos,-1) :-
	connectD(Pos,NextPos),
	isFree(NextPos).
move(Move, Pos, Pos1,1) :-
	connect(Pos,Move),
	connect(Move,Pos1),
	isFree(Pos1).
	
move(Move, Pos, Pos1,-1) :-
	connectD(Pos,Move),
	connectD(Move,Pos1),
	isFree(Pos1).

		

% v zozname pozici najde s navyssim ohodnotenim, ak nenajde 
% ziadnu volnu poziciu, tak vrati Val = -1
findBest([Pos],Pos,Val) :- 	
	(
	isFree(Pos) -> evalPos(Pos,Val) 
	;
	Val = -1
	),!.
findBest([Pos|Positions],BestPos,Val) :-
	findBest(Positions,EvBestPos,ValO),	
	(
	isFree(Pos) -> evalPos(Pos,ValI)
	;
	ValI = -1
	),
	(
	ValO < ValI -> BestPos = Pos,Val = ValI
	;
	ValO >= ValI -> BestPos = EvBestPos,Val = ValO
	).

%todo: treba poriesit sposob pohybu psov
update_dog_position :-	
	dogsPosition(X,Y,Z),
	minimax(2,X,1,-1,Dog1Value,Dog1Move),
	minimax(2,Y,1,-1,Dog2Value,Dog2Move),
	minimax(2,Z,1,-1,Dog3Value,Dog3Move),
	moveDogs(1,Dog1Move).
	

update_hare_position :-	
	harePosition(Pos),	
	minimax(4,Pos,1,1,_,Move),	
	moveHare(Move).


/*
vypocita pomocou minimax najvyhodnejsiu poziciu
minimax(+Depth, +Position, +Player, +HareOrDogs, -BestValue, -BestMove)

	Depth - hlbky vytvaraneho stromu
	Position - aktualna pozicia
	Player - striedanie max/min (hrac/oponent) 1 ak je hrac -1 oponent
	HareOrDogs - urcuje ci ide o psa(-1) alebo zajaca(1)
	Value - hodnota pozicie
	Move - najvyhodnejsia pozicia
*/    
minimax(0, Position, Player, _ , Value, _) :- % ohodnoti koncovy list stromu 
      evalPos(Position, Evaluated),
      Value is Evaluated * Player.
minimax(Depth, Position, Player, HareOrDogs , Value, Move) :-
      Depth > 0, 
      D1 is Depth - 1,
      bagof(NextPos, move(Position, NextPos, HareOrDogs), Moves), % zoznam platnych pohybov, zohladnuje aj obsadene pozicie
      minimax(Moves, Position, D1, Player, HareOrDogs, -100, nil, Value, Move),!.
      
% vypocet by mal zohladnovat aj ci ide o zajaca alebo psa
minimax([], _, _,_,_, Value,Best,Value,Best).
minimax([Move|Moves],Position,Depth,Player, HareOrDogs, Value0,Move0,BestValue,BestMove):-
      move(Move, Position, Position1, HareOrDogs),       
      Opponent is -Player,
      Animal is HareOrDogs,
      minimax(Depth, Position1, Opponent, HareOrDogs, OpponentValue, _), 
      Value is -OpponentValue,
      ( 
      Value > Value0 -> minimax(Moves,Position,Depth,Player, HareOrDogs, Value ,Move ,BestValue,BestMove)
      ;
      minimax(Moves,Position,Depth,Player,HareOrDogs,Value0,Move0,BestValue,BestMove)
      ). 