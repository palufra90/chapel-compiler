module EnvironmentFunctions where

import Abschapel
import Lexchapel
import ErrM



-----------------
--- Data Type ---
-----------------


-- Environment per le variabili e costanti, in cui il primo Bool serve per controllare la sovrascrittura delle variabili mentre il secondo indica se è una variabile o una costante
data Env = Var Id Type Bool Pass	
	   deriving(Eq,Ord,Show)


-- Environment per le funzioni
data EnvF = Fun Id Type [Type]
	    deriving(Eq,Ord,Show)


data EnvL = Label Id Bool
	    deriving(Eq,Ord,Show)

-----------------
--- Functions ---
-----------------


-- funzione per inizializzare l'environment delle variabili/costanti
initEnv = []


-- funzione per inizializzare l'environment delle funzioni
initEnvF = [(Fun (Id "writeInt") TypeVoid [TypeInt]),(Fun (Id "writeFloat") TypeVoid [TypeFloat]),
	    (Fun (Id "writeChar") TypeVoid [TypeChar]),(Fun (Id "writeString") TypeVoid [TypeString]),
	    (Fun (Id "readInt") TypeInt []),(Fun (Id "readFloat") TypeFloat []),
	    (Fun (Id "readChar") TypeChar []),(Fun (Id "readString") TypeString [])]




showTypeName (TypeInt) 		= "Int"
showTypeName (TypeFloat)	= "Float"
showTypeName (TypeChar) 	= "Char"
showTypeName (TypeString) 	= "String"
showTypeName (TypeBool) 	= "Boolean"
showTypeName (TypePointer t) 	= "Pointer [" ++ showTypeName t ++ "]"
showTypeName (TypeArray size t) = "Array [" ++ showTypeName t ++ "]"
showTypeName (TypeMix) 		= "Multiple Type"
showTypeName (TypeVoid) 	= "Void"
showTypeName (TypeError) 	= "Error"


------------------------
--- Controllo errori ---
------------------------


--aggiunge una nuova funzione all'ambiente
addFunDecl x (y@(Fun n _ _))  | (searchFun n x) =  y:(deleteFun n x)	
			      | otherwise = y: x 	

--aggiunge una nuova funzione all'ambiente
addLabelDecl x [] = [(Label x True)]
addLabelDecl x (y@(Label id b):ys) | (x==id ) = ((Label x True):ys)
				   | otherwise = addLabelDecl x ys

-- aggiunge una lista di variabili a un'ambiente
addVarDecl xs [] = xs
addVarDecl xs (y:ys) = (addVarDecl (insOneDecl xs y) ys)

--errore di tipo negli assegnamenti
assTypeError t1 isconst t2 e1 e2 op | (isconst) = "Assignment error at " ++ (tokenPos2 op) ++ ". A constant cannot be modified"
				    | (t1==TypeFloat && t2==TypeInt) = ""
				    | (t1==TypeString && t2==TypeChar) = ""
				    | (t1/=t2) = "Type error at " ++ (tokenPos2 op) ++ " Given: (" ++ (showTypeName t2) ++ "). Expected (" ++ (showTypeName t1) ++ ")"
				    | ((e1 /= "")||(e2 /="")) = e1 ++ e2
				    | otherwise = ""




--controlla che le espressioni dell'array siano del tipo indicato
--qui si potrebbe controllare che il range sia solo uno su quello dell'array unidimensionale con lenght=1
checkArrayExpr x y op 	| (x==TypeError) = ("Type error at " ++ (tokenPos2 op) ++ ". Expected: Array "  )
			| (y==TypeMix) = ("Type error at " ++ (tokenPos2 op) ++ ". Given: Multiple Type Array"  )
			| (x==TypeVoid) = ("Wrong array size at " ++ (tokenPos2 op) )
		   	| (x==y) = ""
		   	| otherwise = ("Type error at " ++ (tokenPos2 op) ++ ". Given: Array[" ++ (showTypeName y) ++ "]. Expected: Array[" ++ (showTypeName x) ++ "]")


--controlla se una label è già stata dichiarata
checkDeclLabel [] _ = Nothing
checkDeclLabel (x:xs) y = case (checkLabel x y) of {
				Just a -> Just a;
				Nothing -> checkDeclLabel xs y;
			}

-- data una lista composta da id e un ambiente, controlla se le varibili id sono già state dichirate
checkDeclVar [] _  = Nothing
checkDeclVar (x:xs) ys = case (checkVar x ys) of {
				Just a -> Just a;
				Nothing -> checkDeclVar xs ys;
			}


--controlla se la funzione richiamata è presente nell'ambiente
checkErrFun e id env tl p | (e /= "") = e
			  | not(searchFun id env) = "Scope Error at "++(tokenPos2 p)++": Function or procedure  "++(getIdName id)++" not in scope"
			  | otherwise = checkNParamFun id env tl p


--controlla se una label è presente e ridichiarabile
checkLabel lab [] = Nothing
checkLabel lab ((Label a True):xs)	| lab==a = Just a
					| otherwise = checkLabel lab xs
checkLabel lab ((Label a False):xs)	= checkLabel lab xs


--controlla se i parametri del range sono entrambi interi
checkIntParam x y op = if((x == y) && (x == TypeInt)) then ""
			else ("Type error in range costructor at " ++ (tokenPos2 op) ++ ". Given: (" ++ (showTypeName x) ++ ".." ++ (showTypeName y) ++ "), Expected: (Int..Int)")


--controlla che la segnatura della funzione sia corretta
checkNParamFun id env tl p  =  if ( (length (getTypeListFun (getFun id env))) /= (length tl)  ) 
				then "Sintax Error at "++(tokenPos2 p)++": Wrong n.of arguments in call of "++(getIdName id)++", expected: "++(show (length (getTypeListFun (getFun id env))))
				else ( case ( matchTypeList (getTypeListFun (getFun id env)) tl ) of {
						Just a  -> "Type Error at "++(tokenPos2 p)++": Wrong argument type, couldn't match "++(showTypeName (fst a))++" with "++(showTypeName (snd a))++" in function "++(getIdName id);
						Nothing -> "";
						} )

--dato un id e un ambiente, controlla se si può dichiarare una variabile
checkVar id [] = Nothing
checkVar id ((Var a _ True _ ):xs) 	| id==a = 	Just a
				   	| otherwise = checkVar id xs
checkVar id ((Var a _ False _ ):xs)     = checkVar id xs


-- controlla che non ci siano variabili doppie tra parametri differenti in una definizione di funzione(procedura)
checkVarParamList [] ys = Nothing
checkVarParamList (x@(Var a _ _ _):xs) ys | (searchVar a ys) =  Just a
					  | otherwise = (checkVarParamList xs ys)

-- cancella la dichiarazione di una funzione
deleteFun  id [] = []								 
deleteFun  id (x@(Fun a _ _ ):xs)  	|  id==a = xs
					|  otherwise = x:(deleteFun id xs)


-- elimina se presente una variabile dalla lista dato l'id
deleteVar   id [] = []								 
deleteVar   id (x@(Var a _ _ _ ):xs)  	|  id==a = xs
					|  otherwise = x:(deleteVar id xs)

-- data un id di funzione e il tipo di ritorno, genera un elemento per l'ambiente
genFunElem x y z = (Fun x y z)


--genera attributo hasReturn per la lista di when
genHasReturnWhen x y retX retY 	| (x==[]) = retY
			   	| otherwise = retX && retY

--genera il tipo della lista 
genTypeList x y = if (x == y ) then x
			else TypeMix

--genera il tipo delle expr di when
genTypeWhen x y | (x == TypeVoid) = y
		| (x == y) = x
		| otherwise = TypeMix


-- data una lista di id e un tipo, costruisce una lista di oggetti per l'ambiente
genVarList [] y pass = []
genVarList (x:xs) y  pass = (Var x y True pass):(genVarList xs y pass)


-- Imposta il tipo risultante dei due operandi per l'addizione/concatenazione
getAddOpType x y  | (x == TypeInt && y == TypeInt) = TypeInt
		  | (((x == TypeInt && y == TypeFloat) || 
		     (x == TypeFloat && y == TypeFloat) || 
		     (x == TypeFloat && y == TypeInt))) = TypeFloat
		  | ((x == TypeString || x== TypeChar) &&
		     (y == TypeString || y== TypeChar)) = TypeString
		  | otherwise = TypeError


--Imposta il tipo risultante degli operandi della addizione/concatenazione
getAddType x y 	| ((x == TypeInt && y == TypeFloat) || 
		  (x == TypeFloat && y == TypeFloat) || 
		  (x == TypeFloat && y == TypeInt)) = TypeFloat
	      	| (x == TypeInt && y == TypeInt) = TypeInt
		| ((x == TypeChar || x == TypeString) && 
		  (y == TypeChar || y == TypeString)) = TypeString
		| otherwise = TypeError


--cerca la funzione dati l'id e l'ambiente
getFun id (x@(Fun a _ _):xs) | id == a = x
			     | otherwise = getFun id xs


--ritorna il nome di un id
getIdName (Id str) = str

--ritorna true se è una costante
getIsConst id [] = False
getIsConst id (x@(Var a t _ pass):xs)	| (id==a && pass==Pass_const) = True
					| (id==a && pass/=Pass_const) = False
					| otherwise = getIsConst id xs

-- Imposta il tipo risultante dei due operandi
getOpType x y | ((x == TypeInt && y == TypeFloat) || 
		 (x == TypeFloat && y == TypeFloat) || 
		 (x == TypeFloat && y == TypeInt)) = TypeFloat
	      | (x == TypeInt && y == TypeInt) = TypeInt
	      | otherwise = TypeError


-- restituiscono il tipo
getTypeArray (TypeArray _ t) = t

getTypeListFun (Fun _ _ tl ) = tl

getTypeFun (Fun _ t _ ) = t

getTypePnt (TypePointer t) = t

getTypeVar (Var _ t _ _) = t


-- estrae una variabile dall'ambiente se è presente
getVarType id [] = TypeError
getVarType id (x@(Var a t _ _ ):xs)	| id==a =	t
					| otherwise = getVarType id xs

--inserisce dichiarazione nell'ambiente
insOneDecl xs (y@(Var n _ _ _ )) | (searchVar n xs) = y:(deleteVar n xs)
			         | otherwise = y:xs


matchTypeList [] [] = Nothing
matchTypeList (x:xs) (y:ys) 	| x/=y = Just (x,y)
				| otherwise = matchTypeList xs ys

-- resetta il valore booleano per permettere la sovrascrittura delle variabili
newEnv[] = []
newEnv((Var a t _ pass):xs) |(pass == Pass_const) = (Var a t True pass) : newEnv xs
			    |otherwise = (Var a t False pass) : newEnv xs

-- resetta il valore booleano delle variabili esistenti e aggiunge le nuove
newEnvPar [] y = y
newEnvPar x [] = newEnv(x)
newEnvPar x y = (unionEnv y $ newEnv(x))


-- determina se c'è una funzione in una lista di funzioni dato il suo id	
searchFun id [] = 	False								
searchFun id ((Fun z _ _ ):xs)	| id==z = True
				| otherwise = searchFun id xs

-- determina se c'è una label in una lista di label dato il suo id	
searchLabel id [] = 	Just id							
searchLabel id ((Label z _ ):xs)	| id==z = Nothing
					| otherwise = searchLabel id xs

-- determina se c'è una una variabile in una lista di variabili dato il suo id			
searchVar id [] = False
searchVar id ((Var a _ _ _):xs) | id==a = True
				| otherwise = searchVar id xs


-- unisce due ambienti
unionEnv [] y = y
unionEnv (x@(Var n _ _ _):xs) y | (searchVar n y) = unionEnv xs $ (x:(deleteVar n y))
			      	| otherwise = unionEnv xs (x:y)

-- Verifica se c'è un errore nel tipo degli operandi dell'addizione/concatenazione
verifyAddOp x y e1 e2 op | (e1 /= "" || e2 /= "") = e1 ++ e2
		         | (((x == TypeInt || x == TypeFloat ) && (y == TypeInt || y == TypeFloat)) ||
			    ((x == TypeString || x == TypeChar) && (y == TypeString || y== TypeChar))) = ""
			 | otherwise = "Type error at " ++ (tokenPos2 op) ++ " Impossible addition or concatenation between: ("++ (showTypeName x )++ ", " ++ (showTypeName y) ++ ")"    


--Verifica che la lista di range passati all'array siano ben definiti
verifyArray x y e1 e2 op = if ( e1 == "" && e2 == "" )  
			then (case x of {
				(TypeArray _ _ ) -> (if ( y == TypeInt)
							then ""
							else ("Type error at " ++ (tokenPos2 op) ++ " Given: (" ++ (showTypeName y) ++ "). Expected (ListInt)"));
				otherwise -> ("Type error at " ++ (tokenPos2 op) ++ " Expected (Type Array)")})
			else e1 ++ e2


--verifica che quando dichiaro lista di variabili le rexpr siano del tipo indicato
verifyArrayType a n = case a of {
				(TypeArray xs c ) -> (if (length xs == n) then c
							else TypeVoid);
				otherwise -> TypeError}


-- Verifica che x,y in x AND|OR y siano booleani
verifyBoolExp x y e1 e2 op | (e1 /= "" || e2 /= "") = e1 ++ e2
			   | (x == TypeBool && y == TypeBool) = ""
			   | otherwise =  "Type error at " ++ (tokenPos2 op) ++ " Given: (" ++(showTypeName x )++ ", " ++ (showTypeName y) ++ ")"

-- Verifica che x in not x sia booleano
verifyBoolUnExp x y op = if ((y == "") &&(x == TypeBool) ) then ""
			else ("Type error at " ++ (tokenPos2 op) ++ " Given: " ++
					 (showTypeName x ))


--Verifica che il cast avvenga tra tipi ammissibili
verifyCast x t err op	| (err /= "") = err
			| (((x == TypeInt || x == TypeFloat ) && (t == TypeInt || t == TypeFloat)) ||
			   ((x == TypeChar) && (t == TypeString || t == TypeChar)) ||
			   ((x == TypeInt || x == TypeFloat ) && (t == TypeString))) = ""
			| otherwise = "Type error at " ++ (tokenPos2 op) ++ " Impossible cast operation between: ("++ (showTypeName x )++ ", " ++ (showTypeName t) ++ ")"


-- Verifica che x e y abbiano lo stesso tipo
verifyEqOp x y e1 e2 op | (e1 /= "" || e2 /= "") = e1 ++ e2
			| (x == y) = ""
			| otherwise = "Type error at " ++ (tokenPos2 op) ++ " Given: (" ++(showTypeName x )++ ", " ++ (showTypeName y) ++ "). Expected: same type"


--verifica che tutti i parametri del ciclo for siano corretti
--id se non è una costante viene inserito nell'env locale, altrimenti dà errore
--gli operatori di range # e by devono avere argomenti interi
verifyFor id env tRange e1 t2 e2 op 	| (getIsConst id env) = "Index in For loop cannot be a constant at "++ (tokenPos2 op)
					| (tRange /= TypeRange) = "Wrong range expression in For loop at " ++ (tokenPos2 op)++". Expected: (Int..Int)"
					| (t2 /= TypeInt) = "Wrong range operators in For loop at " ++ (tokenPos2 op)++". Given: "++(showTypeName t2 )++" Expected: Int"
					| (e1 /= "" || e2 /= "") = e1 ++ e2
					| otherwise = ""


--verifica che quando dichiaro lista di variabili le rexpr siano del tipo indicato
verifyIdType typ typList list 	| (typ==typList) = "" 
				| (typ==TypeFloat && typList==TypeInt) = ""
				| (typ==TypeString && typList==TypeChar) = ""
			      	| otherwise = ("Type error at " ++ (tokenPos2 list) ++ ". Given: " ++ (showTypeName typList) ++ ". Expected: " ++ (showTypeName typ))


--verifica che l'expr di un if sia booleano
verifyIfCond typ e1 op	| (e1 /= "") = e1
			| (typ == TypeBool) = ""
			| otherwise = "Type error at " ++ (tokenPos2 op) ++ " Given: (" ++(showTypeName typ )++ "). Expected (Boolean)"
 

-- Verifica che gli operandi siano interi
verifyIntOp x y  e1 e2 op | (e1 /= "" || e2 /= "") = e1 ++ e2
			  | (x == TypeInt && y == TypeInt) = ""
			  | otherwise = "Type error at " ++ (tokenPos2 op) ++" Given: ("++ (showTypeName x )++ ", " ++ (showTypeName y) ++ ")"


-- Verifica che non ci siano errori
verifyNEqOp e1 e2 = if (e1 == "" && e2 == "") then ""
			else e1 ++ e2


-- Verifica che x sia int o float
verifyNum x e op | (e /= "") = e
		 | (x == TypeInt || x == TypeFloat) = ""
		 | otherwise = "Type error at " ++ (tokenPos2 op) ++ " Given: " ++ (showTypeName x )


-- Verifica se c'è un errore nel tipo degli operandi
verifyOp x y e1 e2 op | (e1 /= "" || e2 /= "") = e1 ++ e2
		      | ((x == TypeInt || x == TypeFloat ) && (y == TypeInt || y == TypeFloat)) = ""
		      | otherwise = "Type error at " ++ (tokenPos2 op) ++ " Given: ("++ (showTypeName x )++ ", " ++ (showTypeName y) ++ "). Expected: Int or Float"
