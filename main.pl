:- dynamic isFree/1.

:- dynamic harePosition/1.

:- dynamic dogsPosition/3.

:- dynamic turnCounter/1.	

staticHareMove(0) :- moveHare(7),	writeln('HARE:7;').
staticHareMove(1) :- isFree(6), moveHare(6),	writeln('HARE:6;').
staticHareMove(1) :- moveHare(3),	writeln('HARE:3;').
staticDogsMove(0) :- moveDogs(1,6),	writeln('DOG:1,6;').
staticDogsMove(1) :- moveDogs(2,1),	writeln('DOG:2,1;').

%dlzka cakania na odpoved v sekundach
timeout(1).

  %INICIALIZACE A MAZANI
%----------------------------------------------
initGround :- assert(isFree(2)), assert(isFree(3)),
  assert(isFree(5)), assert(isFree(6)), assert(isFree(7)), 
  assert(isFree(10)), assert(isFree(11)).

initHare :- assert(harePosition(8)).

initDogs :- assert(dogsPosition(1,4,9)).

initTurnCounter :- assert(turnCounter(0)).

initAll :- initGround, initHare, initDogs, initTurnCounter.
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
clearTurnCounter :- turnCounter(X), 
              retract(turnCounter(X)),
              fail.
              
clearAll :- clearGround ; clearHare ; clearDogs ; clearTurnCounter ; true.
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

isHareFree :- harePosition(Hare),
                connect(Hare,Pos),
                isFree(Pos),!.
                
isDogsWin :- \+ isHareFree.

	
	
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
	isHareWin -> write('Hare WIN'),nl,halt;
    	isDogsWin -> write('Dogs WIN'),nl,halt.

isFast(TimeStart) :-
	get_time(TimeEnd),
    	ResultTime is TimeEnd - TimeStart,
	timeout(Timeout),
	ResultTime < Timeout.	
%	;
%	write('AI waiting time exceeded'),nl,write('You win'),nl,fail.

		

% inicializuje hracie pole a spusti hru 
start :- 
    clearAll,
    initAll,
    %zistenie kdo za koho hra    
    read_line(Line),
    get_time(TimeStart),		         
    skipLastItem(Line,Edited),	      
    atom_codes(StringMessage,Edited), 	
    make_term(StringMessage,List),    
    getFirst(Name,List), 
    getLast(Body,List),
    (
    Name = 'START' -> write('HARE:8;'),nl,play(1)% 1 znamena ze AI hraje za zajaca
    ;
	    (
	    isHare(Name,Body,_) -> 
	    	turnCounter(X), 
	    	staticDogsMove(X),
	    	turnCounter(X),
		NewCount is X + 1,
		retract(turnCounter(X)),
		assert(turnCounter(NewCount)),
		isFast(TimeStart),
	    	play(-1) % -1 AI hraje za psov
	    ;
	    fail
	    )
    ).    
 
% hra
play(HareOrDogs) :-
    isWinner
    ;
    read_line(Line),		      %precita riadok a v vrati text ako kody znakov
    get_time(TimeStart),       
    skipLastItem(Line,Edited),	      %odstrani ; zo vstupu    
    atom_codes(StringMessage,Edited), %prevedie spat na text, aby fungovala make_term    	
    make_term(StringMessage,List),    %rozdelime zpravu do pole (napr zpravu 'HARE:3', rozdelime do pole ['HARE', '3']    
    getFirst(Name,List),              %do Name se ulozi prvni prvek pole    
    getLast(Body,List),               %do Body se ulozi druhy a zaroven posledni prvek
    (isQuit(Name) -> !,fail	      %pokud je zprava 'QUIT' dochazi k ukonceni aplikace
	    ;
	    (                		      %jinak se kontroluje zda jde o zpravu pro psy nebo kralika
		    %je to pes a je to validny tah
		    isDog(Name,Body,ID,Field),HareOrDogs = 1 ->
		    	(
			    	moveDogs(ID,Field),		    	 
	            		(
	            		(turnCounter(X), staticHareMove(X))
	            		;
	           		update_hare_position
	              		),
				isFast(TimeStart)
				;
				write('You win1'),nl,halt
			)
		    ;
		    (
			    %je to zajac a je to validny tah              
			    isHare(Name,Body,Field),HareOrDogs = -1 ->  
			    	(
				    	moveHare(Field),
			 		(
	                		(turnCounter(X),staticDogsMove(X)) 
	                		;
	                		update_dog_position
	                		),	            
				    	isFast(TimeStart)
				    	;
				    	write('You win2'),nl,halt
			    	)
			    ;
			    (
				    isPositions(Name) -> writePositions
				    ;
				    (
				    write('invalid move'),nl,(HareOrDogs = 1,write('dogs win'),nl;write('hare win'),nl),halt
				    )
			    )
		    )
	    )	   
    ),
    turnCounter(X),
    NewCount is X + 1,
    retract(turnCounter(X)),
    assert(turnCounter(NewCount)),
    play(HareOrDogs).    
%    get_time(TimeEnd),
%    (
%	    ResultTime is TimeEnd - TimeStart,
%	    timeout(Timeout),
%	    ResultTime > Timeout -> write('waiting time exceeded'),nl,start% TODO: treba vypisat vitaza
%	    ;	      
%	    play(HareOrDogs)	    
%	    
%   ).


  
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
evalPos(3,42).
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
		

% v zozname pozici najde s navyssim ohodnotenim
findHight([Pos],Pos,Val) :-
	evalPos(Pos,Val),!.
	
findHight([Pos|Positions],BestPos,Val) :-
	findHight(Positions,EvBestPos,ValO),	
	evalPos(Pos,ValI),
	(
	ValO < ValI -> BestPos = Pos,Val = ValI
	;
	ValO >= ValI -> BestPos = EvBestPos,Val = ValO
	).

findLow([Pos],Pos,Val) :-
	evalPos(Pos,Val),
	!.
	
findLow([Pos|Positions],LowPos,Val) :-
	findLow(Positions,EvLowPos,ValO),	
	evalPos(Pos,ValI),	
	(
		ValO > ValI -> LowPos = Pos,Val = ValI
		;
		ValO =< ValI -> LowPos = EvLowPos,Val = ValO
	).
% z vlozenych hodnot vyberie s najvacsou hodnotou
chooseDog([Dog],[Value],[Move],Value,Move,Dog).	
chooseDog([Dog|Dogs],[Value|Values],[Move|Moves],Value,Move,Dog) :-	
	chooseDog(Dogs,Values,Moves,RetValue,_,_),
	Value >= RetValue,
	!.
chooseDog([_|Dogs],[Value|Values],[_|Moves],RetValue,RetMove,RetDog) :-	
	chooseDog(Dogs,Values,Moves,RetValue,RetMove,RetDog),
	Value < RetValue,
	!.
	

%todo: treba poriesit sposob pohybu psov
update_dog_position :-		
	dogsPosition(X,Y,Z),	
%	write('tu je'),
	minimax(2,X,1,-1,Dog1Value,Dog1Move),
	minimax(2,Y,1,-1,Dog2Value,Dog2Move),
	minimax(2,Z,1,-1,Dog3Value,Dog3Move),
%	write(Dog1Value),nl,
%	write(Dog1Move),nl,
%	write(Dog2Value),nl,
%	write(Dog2Move),nl,
%	write(Dog3Value),nl,
%	write(Dog3Move),nl,
	chooseDog([1,2,3],[Dog1Value,Dog2Value,Dog3Value],[Dog1Move,Dog2Move,Dog3Move],_,Move,Dog),
%	write(Dog),nl,
%	write(Move),nl,
	moveDogs(Dog,Move),
	write('DOG:'),
	write(Dog),
	write(','),	
	writeMove(Move),
	write(';'),
	nl.
%	Dog1Value =:= Dog2Value, Dog2Value =:= Dog3Value ->
%		moveDogs(1,Dog1Move),
%		write('DOG:1'),
%		write(','),	
%		writeMove(Dog1Move),
%		write(';'),
%		nl
%	;
%	(
%		write(Dog1Value),nl,Dog1Value >= Dog2Value ->
%			Dog1Value >= Dog3Value, moveDogs(1,Dog1Move),
%			write('DOG:1'),
%			write(','),	
%			writeMove(Dog1Move),
%			write(';'),
%			nl
%		;
%		(
%			Dog2Value >= Dog1Value -> 
%				Dog2Value >= Dog3Value, moveDogs(2,Dog2Move),
%				write('DOG:2'),
%				write(','),	
%				writeMove(Dog2Move),
%				write(';'),
%				nl
%			;
%			moveDogs(3,Dog3Move),
%			write('DOG:3'),
%			write(','),	
%			writeMove(Dog3Move),
%			write(';'),
%			nl
%		)
%	).
	  

update_hare_position :-			
	harePosition(Pos),	
	minimax(4,Pos,1,1,_,Move),	
	moveHare(Move),
	write('HARE:'),
  	writeMove(Move),
	write(';'),
	nl.
	
writeMove(Move) :-  (Move < 10, write(Move))
	             ; (Move = 10, write('A'))
	             ; (Move = 11, write('B')).


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
minimax(0, Position, Player, _ , Value, Position) :- % ohodnoti koncovy list stromu 
      evalPos(Position, Evaluated),
      Value is Evaluated * Player.
minimax(Depth, Position, Player, HareOrDogs , Value, Move) :-
      Depth > 0,
      D1 is Depth - 1,
      bagof(NextPos, move(Position, NextPos, HareOrDogs), Moves), % zoznam platnych pohybov, zohladnuje aj obsadene pozicie  
      length(Moves,Size),
      Size > 1 -> minimax(Moves, Position, D1, Player, HareOrDogs, -100, nil, Value, Move),!%vypocet minimax pre zvoleneho hraca
      ;
      (	      %	ak nenajde ziadnu tak sa skus presunut na minimax poziciu, ktora je volna, inak false	
	      Size = 0 -> evalPos(Position,Value),isFree(Position),Move=Position
	      ;
	      (
		      % ak je len jeden platny krok, tak nema zmysel riesit strom
		      move(Position, Move, HareOrDogs),
		      evalPos(NextPos,Value)
	      )
      ),!.
      
      
% vypocet by mal zohladnovat aj ci ide o zajaca alebo psa


minimax([], _, _,_,_, Value,Best,Value,Best).
minimax([Move|Moves],Position,Depth,Player, HareOrDogs, OpValue, OpMove, BestValue,BestMove):-  
      Opponent is -Player,% zmena hraca
      Animal is -HareOrDogs,% zmena zvierata      
      minimax(Depth, Move, Opponent, Animal ,OponentValue, _),%vnorenie do dalsej urovne stromu
      Value is -OponentValue,
      ( 
      Value > OpValue -> minimax(Moves,Position,Depth,Player, HareOrDogs, Value ,Move ,BestValue,BestMove)
      ;
      minimax(Moves,Position,Depth,Player,HareOrDogs,OpValue,OpMove,BestValue,BestMove)
      ).        