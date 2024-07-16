% PRIMERA PARTE (FAMILIA SIMPSONS)

% 1) Diseñar la familia (padre es papa o mama)

padre(homero,bart).
padre(homero,lisa).
padre(homero,maggie).

padre(marge,bart).
padre(marge,lisa).
padre(marge,maggie).

padre(ned,rod).
padre(ned,tod).

padre(abe,homero).
padre(abe,herbert).

persona(Persona) :- padre(_,Persona). % el que es hijo, es persona
persona(Persona) :- padre(Persona,_). % el que es padre, es persona

% Si Homero es padre de Bart.
% padre(homero,bart).

% Si Homero es padre de Bort.
% padre(homero,bort).

% Si existe un padre para Maggie. (con y sin ejemplos de padre)
% padre(P, maggie).  --> con ejemplos
% padre(_, maggie).  --> sin ejemplos (solo pregunta si existe un padre)

% Si existe un padre para Ned.
% padre(_, ned).

% Si existe un hijo de Herbert.
% padre(herbert, _).

% Si Ned tiene hijos.  (con y sin ejemplos de hijos)
% padre(ned, Hijo).  --> con ejemplos 
% padre(ned, _).     --> sin ejemplos (solo pregunta si tiene hijos)  

% Si hay algún padre.
% padre(_,_).

% 2) Bart es hermano de Lisa?

sonHermanos(Persona1,Persona2) :-
    padre(Padre,Persona1),
    padre(Padre,Persona2),
    Persona1 \= Persona2.

% 3) Herbert es Tio de Bart y Ned es Tio de Maggie

esTio(Tio,Sobrino) :-
    padre(Padre,Sobrino),
    sonHermanos(Padre,Tio).

esTio(ned,maggie).

% 4) ¿quienes son criados por si mismos? --> personas que tienen un unico padre

criadoPorSiMismo(Persona) :-
    padre(_,Persona), % existe un padre para la Persona (la Persona tiene al menos un padre)
    not(tieneMasDeUnPadre(Persona)).

tieneMasDeUnPadre(Persona) :-
    padre(Padre1,Persona),
    padre(Padre2,Persona),
    Padre1 \= Padre2.

% 5) Modificar la base de conocimientos para determinar que Homero NO es padre de Rod y Tod
% NO necesitamos cambiar nada porque, por
% el principio de Universo Cerrado, al consultar ya nos daria false.

% 6) 
quilombero(bart).
quilombero(homero).

deportista(rod).
deportista(tod).
deportista(lisa).

esTranqui(Persona) :-
    persona(Persona),
    not(deportista(Persona)),
    not(quilombero(Persona)).

% 7) 

%    forall(Antecedente, Consecuente)

% a) fan del deporte --> todos sus hijos son deportistas
fanDelDeporteV2(Persona) :-
    padre(Persona,_),
    forall(padre(Persona, Hijo), deportista(Hijo)). % para todo hijo del padre, el hijo es deportista
    
fanDelDeporteV1(Persona) :-
    padre(Persona,_),
    not(noTieneHijosDeportistas(Persona)).

noTieneHijosDeportistas(Persona) :-
    padre(Persona,Hijo),
    not(deportista(Hijo)).

% b) NO se banca a sus hermanos --> todos ellos son tranqui, pero la persona no
noSeBancaASusHermanos(Persona) :-
    sonHermanos(Persona,_), % tiene hermano la persona (existe un hermano para la persona) 
    not(esTranqui(Persona)),
    forall(sonHermanos(Persona,Hermano), esTranqui(Hermano)). % para todo hermano de la persona, el hermano es tranqui

% c) persona con pareja soñada -> todos los hijos de la persona tambien son hijos de su pareja
parejaSoniada(Persona,Pareja) :-
    padre(Persona, _), % la persona es padre
    padre(Pareja, _),  % la pareja es padre 
    Persona \= Pareja,
    forall(padre(Persona, Hijo), padre(Pareja, Hijo)). % para todo hijo de una Persona, es hijo de la Pareja
