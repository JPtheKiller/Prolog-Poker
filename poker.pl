%auxiliary predicates:

%checks if there are no repeat members in a list, useful to check for repeat cards.
not_repeat([]).
not_repeat([X|L]):- not(member(X,L)),not_repeat(L).

%basic definitions

valid_cards([2,3,4,5,6,7,8,9,10,jack,queen,king,ace]).
valid_suits([diamonds,clubs,spades,hearts]).

card_value(C,C):- valid_cards(LC), member(C,LC), number(C).
card_value(jack,11).
card_value(queen,12).
card_value(king,13).
card_value(ace,14).

card(C,S):- valid_cards(LC), member(C,LC), valid_suits(LS), member(S,LS).

%definition of hand.
hand([card(C,S)]):- card(C,S),!.
hand([card(C,S)|H]):- card(C,S), hand(H),!.

%definition of a full hand.
valid_hand(H):- hand(H),not_repeat(H).
full_hand(H):- length(H,5),valid_hand(H).

%counts ocurrences of cards with number N in hand H.
handnumcounter(_,[],0).
handnumcounter(N,[card(N,_)|H],C):-  handnumcounter(N,H,C1), C is C1+1.
handnumcounter(N,[card(M,_)|H],C):-  N\==M, handnumcounter(N,H,C).

%counts ocurrences of cards with suit S in hand H.
numberofS([],_,0).
numberofS([card(_,S)|H],S,N):- numberofS(H,S,N1), N is N1+1.
numberofS([card(_,M)|H],S,N):- M\==S, numberofS(H,S,N).

%get the max card value of a full hand.
max_hand_value([card(X1,_),card(X2,_),card(X3,_),card(X4,_),card(X5,_)],V):- card_value(X1,V1),card_value(X2,V2),card_value(X3,V3),card_value(X4,V4),card_value(X5,V5), max_list([V1,V2,V3,V4,V5],V). 
	     
%poker rules

royal_flush(H):- member(card(10,S),H),member(card(jack,S),H),member(card(queen,S),H),
				 member(card(king,S),H),member(card(ace,S),H).

straight_flush(H):-   card_value(C1,CV1),card_value(C2,CV2),card_value(C3,CV3),card_value(C4,CV4),card_value(C5,CV5),
					  CV2 is CV1+1, CV3 is CV2+1, CV4 is CV3+1,CV5 is CV4+1,
					  member(card(C1,S),H),member(card(C2,S),H),member(card(C3,S),H),
					  member(card(C4,S),H),member(card(C5,S),H),!.


fourofakind(H):- member(card(X,_),H), handnumcounter(X,H,4),!.


full_house(H):- valid_cards(LC),member(X,LC), member(Y,LC), X\==Y,
				member(card(X,_),H), handnumcounter(X,H,3),
				member(card(Y,_),H), handnumcounter(Y,H,2),!.


flush(H):- numberofS(H,_,5),!.

straight(H):- card_value(C1,CV1),card_value(C2,CV2),card_value(C3,CV3),card_value(C4,CV4),card_value(C5,CV5),
			  CV2 is CV1+1, CV3 is CV2+1, CV4 is CV3+1,CV5 is CV4+1,
			  member(card(C1,_),H),member(card(C2,_),H),member(card(C3,_),H),member(card(C4,_),H),member(card(C5,_),H),!.

threeofakind(H):- member(card(X,_),H), handnumcounter(X,H,3),!.

two_pair(H):- valid_cards(LC), member(X,LC), member(Y,LC), X\==Y,
			  member(card(X,_),H), handnumcounter(X,H,2),
    		  member(card(Y,_),H), handnumcounter(Y,H,2),!.

one_pair(H):- member(card(X,_),H), handnumcounter(X,H,2),!.

%hand value goes from 2 to 23, 0 if it isnt a full hand, 2-14 is a basic hand.

hand_value(H,0):- not(full_hand(H)),!.
hand_value(H,23):- royal_flush(H),!.
hand_value(H,22):- straight_flush(H),!.
hand_value(H,21):- fourofakind(H),!.
hand_value(H,20):- full_house(H),!.
hand_value(H,19):- flush(H),!.
hand_value(H,18):- straight(H),!.
hand_value(H,17):- threeofakind(H),!.
hand_value(H,16):- two_pair(H),!.
hand_value(H,15):- one_pair(H),!.
hand_value(H,V):- max_hand_value(H,V),!.

%deck shuffling

sortSuit([],_,[]):-!.
sortSuit([X|A],B,[card(X,B)|C]):-sortSuit(A,B,C),!.

unDeck([X],F):-valid_cards(B),sortSuit(B,X,F),!.
unDeck([X|Suits],F):- valid_cards(B),sortSuit(B,X,C),unDeck(Suits,A),append(A,C,F).

deck(X):- valid_suits(Suits),unDeck(Suits,D),random_permutation(D,X).

initdeck():- deck(X),nb_setval(deck,X).
getdeck(VALUE):- nb_getval(deck,VALUE).


%hand initalization

addcard(P):- getdeck([X|D]), nb_getval(P,H), append(H,[X],NH) , nb_setval(P,NH), nb_setval(deck,D).
	     
inithand():- nb_setval(player1,[]), nb_setval(player2,[]), addcard(player1),addcard(player2),nb_setval(playing1,1),nb_setval(playing2,1).

gethand(P,H):- nb_getval(P,H).	    

%pot initalization

initmoney(X):- nb_setval(pot,0),nb_setval(bet,0),nb_setval(money1,X),nb_setval(money2,X),nb_setval(hasbet1,0),nb_setval(hasbet2,0).

getpot(P):- nb_getval(pot,P).

getbet(B):- nb_getval(bet,B).

%writing a hand
writehand([]).
writehand([card(V,S)|H]):- write(V),write(" of "),write(S),write("        "), writehand(H).

%game

%game starts here
play():- init(),playing(1),nl,askforcontinue.

askforcontinue():- writeln("Do you want to continue? (y/n)"),readln([C]),(
						C==y,continueplay(),!;
						C==n,!;
						writeln("Please write y or n."),askforcontinue,!).

%initialiazing game
init():- initdeck(),inithand(),askforchips.
askforchips():- writeln("How many chips do the players have?"),readln([M]),(
					number(M),initmoney(M),!;
					writeln("Please write a number."),askforchips).
					

onlyplayer(1):- nb_getval(playing2,0).	 
onlyplayer(0):- nb_getval(playing2,1).

%playing the game
playing(X):- X \== 4,nl,write("----- "), Y is X+1,write(Y),write(" CARDS ------"),nl,write("Press Enter to draw your card."),readln(_),
	     addcard(player1),addcard(player2),nl,write("Your cards:"), nl, gethand(player1,H), writehand(H), nl,nl, 
	     writeln("Current Pot:") ,getpot(P) ,writeln(P),nl,nb_setval(bet,0),nb_setval(hasbet1,0),nb_setval(hasbet2,0),nb_setval(called1,0),nb_setval(called2,0),
		 onlyplayer(ONLYP),(
			ONLYP==0,betting(),!;
			ONLYP==1,writeln("Only Player 1 hasn't folded, there's no need to bet."),!),
		 playing(Y).



%finishing the game
playing(4):-    nl,addcard(player1),addcard(player2),writeln("Both players have 5 cards. Press Enter to see the results."),readln(_),
		writeln("Player 1 hand:"), nl, gethand(player1,H1), writehand(H1),nl,nl,
		writeln("Player 2 hand:"), nl, gethand(player2,H2), writehand(H2),nl,
		nb_getval(playing1,F1),nb_getval(playing2,F2),(
			F1==0,F2==1, writeln("Player 1 has folded."),nl,win(2),!;
			F1==1,F2==0, writeln("Player 2 has folded."),nl,win(1),!;
			F1==0,F2==0, writeln("Both players have folded."),nl,win(0),!;
			F1==1,F2==1, hand_value(H1,V1),hand_value(H2,V2),nl,(
				V1>V2, win(1),!;
				V2>V1, win(2),!;
				V1==V2,win(0),!)).
%betting
betting():- nb_getval(called1,C1),nb_getval(called2,C2),player(C1),bot(C2),nb_getval(called1,A1),nb_getval(called2,A2),(
	    A1==1,A2==1,update_money,!;
	    writeln("Someone hasn't called yet."),betting()).

player(0):- nb_setval(called1,1),nb_getval(playing1,F),getbet(B),player_betting(B,F).
player(1).


bot(0):- nb_getval(playing2,F),(
			F==0,writeln("Player 2 has folded."),nb_setval(called2,1),!;
 			nb_setval(called2,1), gethand(player2,H),nb_getval(money2,C),getbet(B),botAction(H,C,B),nl,!).

bot(1).

update_money():- nb_getval(money2,CM),(										%only update bot's money here, except when they all in
				 CM\==0,nb_getval(hasbet2,HB),NM is CM-HB,nb_setval(money2,NM),!;
				 CM==0,!). 

/*
%Randombot decisions
retarbot():-nb_getval(money2,0),!.
retarbot():-nb_getval(playing2,0),!.
retarbot():-getbet(P),retarbet(P).

retarbet(P):- P==0,nb_getval(money2,M),random_permutation([bet,check],[X|_]),nomoney(P,X,M);
	   P>0,nb_getval(money2,M),random_permutation([raise,call,fold],[X|_]),nomoney(P,X,M).

nomoney(_,check,_):-writeln("Player 2 has checked."),!.
nomoney(_,fold,_):-nb_setval(playing2,0),writeln("Player 2 has folded."),!.
nomoney(P,X,M):-(
			(X==raise;X==bet),P<M,random(F),D is integer(float_integer_part((F*(M-P)+P+1))),NM is M-D,nb_setval(money2,NM),getpot(L), 
			NP is L+D,nb_setval(pot,NP),E is D-P, nb_setval(bet,E),write("Player 2 has "),
				((X==raise,write(X),write("d to "),write(D),writeln(" chips."));
				X==bet,write(X),write(" "),write(D),writeln(" chips.")),
			nb_setval(called1,0),!);
			
			(P>=M,nb_setval(money2,0),getpot(L), NP is L+M,nb_setval(pot,NP),writeln("Player 2 has gone All in."),!);

			(P<M,NM is M-P,nb_setval(money2,NM),getpot(L), NP is L+P,nb_setval(pot,NP),writeln("Player 2 has called."),!).
*/


%player actions 

player_betting(0,1):- 	nb_getval(money1,M),(
							M==0,writeln("You have no more chips, the game will continue."),!;
							M\==0,write("Will you bet or check? (bet/check) (You have "),write(M),writeln(" chips)"),readln([A]),(
								A == bet,p_bet(M),!;
								A == check,!;
								writeln("Please write bet or check."),player_betting(0,1))).

player_betting(B,1):-	B \== 0,nb_getval(money1,M),getpot(P),nb_getval(hasbet1,HB),TB is B-HB, /*write("You have bet "),writeln(HB), */
		(M>=TB,M\==0,nl,write("Will you raise, call or fold? (raise/call/fold) (You have "),write(M),
		write(" chips and you have to bet "),write(TB),write(" more to call)"),nl,readln([A]),(
			A == raise,p_raise(M,P,B,HB),nb_setval(called2,0),!;
			A == call, p_call(M,TB,P),!;
			A == fold,nb_setval(playing1,0),!);
		M<TB,(M>0,write("You don't have enough chips to call, will you bet all your chips or fold? (allIn/fold) (You have "),write(M),
		write(" chips and you need "),write(TB),write(" chips to call)"),nl,readln([A]),(
			A == allIn, allIn(M,P),!;
			A == fold,nb_setval(playing1,0),!));
		M==0,writeln("You have no more chips, the game will continue."),!).

player_betting(_,0):- nb_setval(called1,1),writeln("You have folded.").

p_bet(M):- writeln("How many chips?"), readln([N]),(
						number(N),N=<M, nb_getval(pot,D), NM is M-N,DM is D+N,nb_setval(money1,NM), nb_setval(pot,DM), nb_setval(bet,N),nb_setval(called2,0),nb_setval(hasbet1,N);
						write("Please write a number lower than your money. (You have "),write(M),writeln(" chips)"),p_bet(M)).

p_raise(M,P,B,HB):-  writeln("To how much do you want to raise?"), readln([N]),(
		number(N),B=<N,N=<M, TB is N-HB,NM is M-TB, nb_setval(money1,NM), 
			  NP is P+TB,nb_setval(pot,NP), 
			  getbet(B),nb_setval(bet,N),
			  nb_setval(hasbet1,N),!;
		write("Please write a number higher than current bet and lower than your money. (Current bet is "),write(B),write(" and you have "),write(M),writeln(" chips)"),p_raise(M,P,B,HB)).

p_call(M,TB,P):-   NM is M-TB, nb_setval(money1,NM), 
				   NP is P+TB, nb_setval(pot,NP),
				   getbet(B),nb_setval(hasbet1,B).

allIn(M,P):-  nb_setval(money1,0), 
			  NP is P+M, nb_setval(pot,NP),
			  nb_getval(hasbet1,HB),NHB is HB+M,nb_setval(hasbet1,NHB).


%PATCHON'S_BOT

%get the max card value of a hand and its suit.
max_hand_value([card(X,S)],X,S,1).
max_hand_value([card(X1,S1),card(X2,S2)],V,S,2):- card_value(X1,V1),card_value(X2,V2), max_list([V1,V2],V), card_value(N,V),member(card(N,S),[card(X1,S1),card(X2,S2)]),!.
max_hand_value([card(X1,S1),card(X2,S2),card(X3,S3)],V,S,3):- card_value(X1,V1),card_value(X2,V2),card_value(X3,V3), max_list([V1,V2,V3],V), card_value(N,V), member(card(N,S),[card(X1,S1),card(X2,S2),card(X3,S3)]),!.
max_hand_value([card(X1,S1),card(X2,S2),card(X3,S3),card(X4,S4)],V,S,4):- card_value(X1,V1),card_value(X2,V2),card_value(X3,V3),card_value(X4,V4), max_list([V1,V2,V3,V4],V), card_value(N,V), member(card(N,S),[card(X1,S1),card(X2,S2),card(X3,S3),card(X4,S4)]),!.
max_hand_value([card(X1,S1),card(X2,S2),card(X3,S3),card(X4,S4),card(X5,S5)],V,S,5):- card_value(X1,V1),card_value(X2,V2),card_value(X3,V3),card_value(X4,V4),card_value(X5,V5), max_list([V1,V2,V3,V4,V5],V), card_value(N,V), member(card(N,S),[card(X1,S1),card(X2,S2),card(X3,S3),card(X4,S4),card(X5,S5)]),!.

%checks if a hand has a repeated card value.
repeats([]):- false.
repeats([X|L]):- member(X,L),!; repeats(L).

%sorts the hand by card value, works for repeated values because prolog magic.
sortHand([card(X,S)],[card(X,S)]).
sortHand(H,F):- length(H,L), max_hand_value(H,V,S,L), card_value(X,V), delete(H,card(X,S),HN), sortHand(HN,S1), append(S1,[card(X,S)],F).

%checks if all cards in a hand have the same suit.
sameSuit([card(_,_)]).
sameSuit([card(_,S),card(Y,S)|H]):- sameSuit([card(Y,S)|H]).

%check if  a hand can straight/royal flush.
canStraight([card(X,_)|H]):- length(H,L), max_hand_value(H,V,_,L), card_value(X,X1),card_value(V,V1), C is V1-X1, C<5, not(repeats(H)).
canRoyal([card(X,S)|H]):- card_value(X,N), N>9, sameSuit([card(X,S)|H]), canStraight([card(X,S)|H]).

%checks how many pairs a hand has
hasPairs([],0).
hasPairs([card(X,S)|H],N):-  handnumcounter(X,[card(X,S)|H],P), P \==2, hasPairs(H,N).
hasPairs([card(X,S)|H],N):-  handnumcounter(X,[card(X,S)|H],2), hasPairs(H,N1), N is N1 + 1,!.

%checks if a hand has a three of a kind.
hasToaK([card(X,S)|H]):-  handnumcounter(X,[card(X,S)|H],3),!; hasToaK(H).

%checks if a hand has a four of a kind.
isFoaK([card(X,S)|H]):- length(H,L), L>2, (
                                          handnumcounter(X,[card(X,S)|H],4),!; 
                                          max_hand_value(H,V,_,L), card_value(N,V), handnumcounter(N,[card(X,S)|H],4),!).


%checks the potential of a hand relative to it's size.
%size 2
handPower(H,2,5):- canRoyal(H),!.
handPower(H,2,4):- canStraight(H), sameSuit(H),!.
handPower(H,2,3):- sameSuit(H),!.
handPower(H,2,2):- canStraight(H),!; hasPairs(H,1),!.
handPower(_,2,0):-!.
%size 3
handPower(H,3,8):- canRoyal(H),!.
handPower(H,3,6):- canStraight(H), sameSuit(H),!.
handPower(H,3,4):- sameSuit(H),!.
handPower(H,3,3):- canStraight(H),!; hasToaK(H),!.
handPower(H,3,2):- hasPairs(H,1),!.
handPower(_,3,0):-!.
%size 4
handPower(H,4,10):- canRoyal(H),!.
handPower(H,4,9):- isFoaK(H),!.
handPower(H,4,8):- canStraight(H), sameSuit(H),!.
handPower(H,4,5):- hasToaK(H),!; sameSuit(H),!.
handPower(H,4,3):- canStraight(H),!; hasPairs(H,2),!.
handPower(H,4,2):- hasPairs(H,1),!.
handPower(_,4,0):-!.

handPower(H,_,1):- (member(card(ace,_),H),!;member(card(king,_),H),!;member(card(queen,_),H),!).		

%bot checks the potential of a hand, determined by P.
botcallscheck(H,P):- length(H,L), sortHand(H,HF), handPower(HF,L,P).      

%bot calculates the maximum viable bet this turn, using the potential (P), the number of cards (4th factor in predicate) and the number of chips it has (C) in hand as limiters.
botbetlimit(P,C,BL,2):- F is P/5, FL is F*60, FLL is FL/100, B is FLL*C, BL is round(B). 
botbetlimit(P,C,BL,3):- F is P/8, FL is F*80, FLL is FL/100, B is FLL*C, BL is round(B).
botbetlimit(P,C,BL,4):- F is P/10, B is F*C, BL is round(B).

farFrom(B,L):- LT is L*0.5, B=<LT.

%Bot receives hand, number of chips he has and the current bet, calculates his bet limit into BL and checks if he should fold call or raise.
botAction(H,C,B):- botcallscheck(H,P), length(H,L), botbetlimit(P,C,BL,L),BR is BL*0.6, BF is round(BR), BP is BL*1.2,BM is round(BP),nb_getval(money2,M),
				   %writeln("Bot's BF,BL and BM:"),write("BF: "),writeln(BF),write("BL: "),writeln(BL), write("BM: "), writeln(BM), 
									  (
									  M==0, writeln("Player 2 has no chips."),!;
									  B==0, BF==0, writeln("Player 2 has checked"),!;
                                      B<BF, botraiseto(BF),!;
                                      BF=<B, farFrom(B,BL), botraiseto(BL),!;
                                      not(farFrom(B,BL)), B=<BL, botcall,!;
									  BL<B, BM>=B,(BM>=M,botallin(B),!;BM=<M, botcall,!),!;
									  BM<B, botfold,!;
									  writeln("Player 2 is confused"),botfold,!).
															 

botallin(B):-	nb_getval(hasbet2,HB), TB is B-HB,
				getpot(P), NP is P+TB,nb_setval(pot,NP),
				nb_setval(hasbet2,B),
				nb_setval(money2,0),writeln("Player 2 betted all their chips to continue in the game.").											 
															 
botfold():- nb_setval(playing2,0),writeln("Player 2 has folded.").

botcall():-  nb_getval(hasbet2,HB),getbet(B), TB is B-HB,
			 getpot(P), NP is P+TB,nb_setval(pot,NP),
			 nb_setval(hasbet2,B),
			 writeln("Player 2 has called."),write_bot_chips.

botraiseto(M):- nb_setval(called1,0), nb_setval(bet,M),
				nb_getval(hasbet2,HB), TB is M-HB,
				getpot(P), NP is P+TB,nb_setval(pot,NP),
				nb_setval(hasbet2,M),
				write("Player 2 has raised to "),write(M),writeln(" chips."),write_bot_chips.																		  

write_bot_chips():- nb_getval(money2,C2),nb_getval(hasbet2,HB),CW is C2-HB,write("Player 2 has "),write(CW),write(" chips.").				
				
%END_OF_CHON'S_BABY_BOT
																		  
%checking winning conditions, and distribuiting chips
win(0):- nl,writeln("It's a draw!"), getpot(P), nb_getval(money1,M1),nb_getval(money2,M2), P2 is div(P,2), N1 is M1+P2, N2 is M2+P2, nb_setval(money1,N1),nb_setval(money2,N2).
win(1):- nl,writeln("Player 1 wins!"),getpot(P), nb_getval(money1,M), N is M+P, nb_setval(money1,N).
win(2):- nl,writeln("Player 2 wins!"),getpot(P), nb_getval(money2,M), N is M+P, nb_setval(money2,N).

%if the game continues
continueplay():- nl,write("Player 1 Chips: "),nb_getval(money1,M1),writeln(M1),write("Player 2 Chips: "),nb_getval(money2,M2),writeln(M2),
				 writeln("Do you want to reset the chips? (y/n)"),readln([C]),(
					C==y,init(),!;
					C==n,initdeck(),inithand(),nb_setval(bet,0),nb_setval(pot,0),!;
					C\==y,C\==n,writeln("Please write y or n."),continueplay,!),
				 playing(1),askforcontinue.		

/*
	TODO:
		-Multiple Players
	
*/

							      
