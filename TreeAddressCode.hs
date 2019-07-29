module TreeAddressCode where

import Abschapel
import Lexchapel
import ErrM
import EnvironmentFunctions





-----------------
--- Data Type ---
-----------------

data Tac = 
	  AssignOp T T
	| UnOp Op T T 
	| BinOp Op T T T	
	| UJump Label
	| Goto T
	| Jump T Label
	| JumpNot T Label
	| DeclProc String Id Int
	| ProcCall String T Id [T] 
	| Lab T 
	| OnExpJump Label
	| Return T
	  deriving (Eq,Ord,Show)


type T = String
type Op = String
type Label = Int



-----------------
--- Functions ---
-----------------

-- genera una lista di assegnamenti (usata nelle dichiarazioni multiple di variabili)
arrayAssign _ _ [] = []
arrayAssign _ [] _ = []
arrayAssign n (x:xs) (y:ys) = (AssignOp ((getIdName x) ++ "[" ++ (show n) ++ "]" ) y) : (arrayAssign (n+1) [x] ys) ++ (arrayAssign n xs (y:ys))

--arrayMulAssign [] _ = []
--arrayMulAssign _ [] = []
--arrayMulAssign (x:xs) (y:ys) = (arrayAssign 1 x y) ++ (arrayAssign ((length y)+1) x ys) ++(arrayMulAssign xs (y:ys))


--calcola la lista di temporanei per l'array
arrayTemp _ [] _ = []
arrayTemp t (x:xs) typ = [BinOp "*" ( "t" ++ (show t)) x (show(offset typ))] ++ (arrayTemp (t+1) xs typ)

--offset da usare nel richiamo degli array, per recuperare l'indirizzo di memoria
offset typ = case typ of {
		TypeInt -> 4;
		TypeFloat -> 8;
		TypeChar -> 2;
		TypeString -> 32;
		TypeBool -> 1;
		(TypePointer t) -> 4;
		(TypeArray r t) -> (length r) * (offset t)
	}

printParam [] = []
printParam [x] = x
printParam (x:xs) = x ++ "," ++  printParam xs


--stampa i temporanei della lista dei parametri dell'array
printParamArray t 0 = ""
printParamArray t 1 = ("t" ++ (show(t+1)))
printParamArray t n = ("t" ++ (show(t+1)) ++ ",") ++ (printParamArray (t+1) (n-1))

--mostra il valore della foglia
showBasic (Int i) = show i
showBasic (Float i) = show i
showBasic (Char i) = "'"++(i:"'")
showBasic (String i) = "\""++i++"\""
showBasic (Bool BoolTrue) = "true"
showBasic (Bool BoolFalse) = "false"

-- genera una lista di assegnamenti (usata nelle dichiarazioni multiple di variabili)
tacAssign [] y = []
tacAssign (x:xs) y = (AssignOp (getIdName x) y) : (tacAssign xs y)



----------------------
--- Pretty Printer ---
----------------------

-- esegue il pretty print del tac
printTac []   = ""
printTac (x:xs) = (case x of{
				AssignOp t1 t2 -> "\t" ++ t1 ++ " = " ++ t2;
				UnOp op t1 t2 ->"\t" ++ t1 ++ " = " ++ op ++ " " ++ t2;
				BinOp op t1 t2 t3 ->"\t" ++ t1 ++ " = " ++ t2 ++ " " ++ op ++ " " ++ t3;
				UJump lab -> "\t" ++ "goto label " ++ (show lab);
				Goto lab -> "\t" ++ "goto label " ++ lab;
				Jump t1 lab ->	"\t" ++"if " ++ t1  ++ " goto label " ++ (show lab);
				JumpNot t1 lab -> "\t" ++"if false " ++ t1  ++ " goto label " ++ (show lab);
				DeclProc str id int ->str ++ " " ++ (getIdName id) ++  "/" ++ (show int);
				ProcCall str t id lt -> "\t" ++ t ++ "call " ++ (getIdName id) ++  " (" ++ (printParam lt) ++ ")";
				Lab lab -> "label" ++ lab ++ " :";
				OnExpJump lab -> "\tonexceptiongoto "  ++  "label" ++  (show lab);
				Return t -> "\treturn " ++ t;
				}) ++ "\n" ++ (printTac xs) 


