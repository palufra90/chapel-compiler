{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Printchapel where

-- pretty-printer generated by the BNF converter

import Abschapel
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (4*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: [a] -> Doc
  prtList = concatD . map (prt 0)

instance Print a => Print [a] where
  prt _ = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)



instance Print Id where
  prt _ (Id i) = doc (showString ( i))
  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])



instance Print Prog where
  prt i e = case e of
   Program cmds -> prPrec i 0 (concatD [prt 0 cmds])


instance Print Decl where
  prt i e = case e of
   VarDecl ids type' -> prPrec i 0 (concatD [doc (showString "var") , prt 0 ids , doc (showString ":") , prt 0 type' , doc (showString ";")])
   VarDeclAss ids type' expr -> prPrec i 0 (concatD [doc (showString "var") , prt 0 ids , doc (showString ":") , prt 0 type' , doc (showString "=") , prt 0 expr , doc (showString ";")])
   ArrDeclSing ids type' exprs -> prPrec i 0 (concatD [doc (showString "var") , prt 0 ids , doc (showString ":") , prt 0 type' , doc (showString "=") , doc (showString "[") , prt 0 exprs , doc (showString "]") , doc (showString ";")])
   ArrDeclMult ids type' exparrayelems -> prPrec i 0 (concatD [doc (showString "var") , prt 0 ids , doc (showString ":") , prt 0 type' , doc (showString "=") , doc (showString "[") , prt 0 exparrayelems , doc (showString "]") , doc (showString ";")])
   ConstDecl id type' expr -> prPrec i 0 (concatD [doc (showString "const") , prt 0 id , doc (showString ":") , prt 0 type' , doc (showString "=") , prt 0 expr , doc (showString ";")])
   LabelDecl id -> prPrec i 0 (concatD [doc (showString "label") , prt 0 id , doc (showString ":") , doc (showString ";")])
   ProcDecl id params cmd -> prPrec i 0 (concatD [doc (showString "proc") , prt 0 id , doc (showString "(") , prt 0 params , doc (showString ")") , prt 0 cmd])
   FuncDecl id params type' cmd -> prPrec i 0 (concatD [doc (showString "proc") , prt 0 id , doc (showString "(") , prt 0 params , doc (showString ")") , doc (showString ":") , prt 0 type' , prt 0 cmd])


instance Print Param where
  prt i e = case e of
   Param id type' -> prPrec i 0 (concatD [prt 0 id , doc (showString ":") , prt 0 type'])
   PassParam pass id type' -> prPrec i 0 (concatD [prt 0 pass , prt 0 id , doc (showString ":") , prt 0 type'])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print Pass where
  prt i e = case e of
   Pass_val  -> prPrec i 0 (concatD [doc (showString "val")])
   Pass_ref  -> prPrec i 0 (concatD [doc (showString "ref")])
   Pass_const  -> prPrec i 0 (concatD [doc (showString "const")])


instance Print ExpR where
  prt i e = case e of
   Range exprange -> prPrec i 0 (concatD [prt 0 exprange])
   ExpAdd expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "+") , prt 0 expr])
   ExpSub expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "-") , prt 0 expr])
   ExpMul expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "*") , prt 0 expr])
   ExpDiv expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "/") , prt 0 expr])
   ExpPot expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "**") , prt 0 expr])
   ExpMod expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "%") , prt 0 expr])
   ExpCast expr type' -> prPrec i 0 (concatD [prt 0 expr , doc (showString ":") , prt 0 type'])
   ParExp expr -> prPrec i 0 (concatD [doc (showString "(") , prt 0 expr , doc (showString ")")])
   ExpNeg expr -> prPrec i 0 (concatD [doc (showString "-") , prt 0 expr])
   ExpLeft expl -> prPrec i 0 (concatD [prt 0 expl])
   ExBasic basic -> prPrec i 0 (concatD [prt 0 basic])
   ExpEqu expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "==") , prt 0 expr])
   ExpLes expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "<") , prt 0 expr])
   ExpLeq expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "<=") , prt 0 expr])
   ExpGre expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString ">") , prt 0 expr])
   ExpGrq expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString ">=") , prt 0 expr])
   ExpNEq expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "!=") , prt 0 expr])
   ExpAnd expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "&&") , prt 0 expr])
   ExpXor expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "^") , prt 0 expr])
   ExpOr expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "||") , prt 0 expr])
   ExpNot expr -> prPrec i 0 (concatD [doc (showString "!") , prt 0 expr])
   FunExp id exprs -> prPrec i 0 (concatD [prt 0 id , doc (showString "(") , prt 0 exprs , doc (showString ")")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print ExpRange where
  prt i e = case e of
   ExpRange expr0 expr -> prPrec i 0 (concatD [prt 0 expr0 , doc (showString "..") , prt 0 expr])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print ExpArrayElem where
  prt i e = case e of
   ExpArrayElem exprs -> prPrec i 0 (concatD [doc (showString "[") , prt 0 exprs , doc (showString "]")])

  prtList es = case es of
   [] -> (concatD [])
   [x] -> (concatD [prt 0 x])
   x:xs -> (concatD [prt 0 x , doc (showString ",") , prt 0 xs])

instance Print ExpL where
  prt i e = case e of
   NameExp id -> prPrec i 0 (concatD [prt 0 id])
   ArrExp expl exprs -> prPrec i 0 (concatD [prt 0 expl , doc (showString "[") , prt 0 exprs , doc (showString "]")])


instance Print Basic where
  prt i e = case e of
   Int n -> prPrec i 0 (concatD [prt 0 n])
   Float d -> prPrec i 0 (concatD [prt 0 d])
   Char c -> prPrec i 0 (concatD [prt 0 c])
   String str -> prPrec i 0 (concatD [prt 0 str])
   Bool boolean -> prPrec i 0 (concatD [prt 0 boolean])


instance Print Boolean where
  prt i e = case e of
   BoolTrue  -> prPrec i 0 (concatD [doc (showString "true")])
   BoolFalse  -> prPrec i 0 (concatD [doc (showString "false")])


instance Print Type where
  prt i e = case e of
   TypeInt  -> prPrec i 0 (concatD [doc (showString "int")])
   TypeFloat  -> prPrec i 0 (concatD [doc (showString "float")])
   TypeChar  -> prPrec i 0 (concatD [doc (showString "char")])
   TypeString  -> prPrec i 0 (concatD [doc (showString "string")])
   TypeBool  -> prPrec i 0 (concatD [doc (showString "bool")])
   TypePointer type' -> prPrec i 0 (concatD [doc (showString "pointer") , prt 0 type'])
   TypeArray expranges type' -> prPrec i 0 (concatD [doc (showString "[") , prt 0 expranges , doc (showString "] ") , prt 0 type'])


instance Print Cmd where
  prt i e = case e of
   CmdSing commsing -> prPrec i 0 (concatD [prt 0 commsing])
   CmdMult cmds -> prPrec i 0 (concatD [doc (showString "{") , prt 0 cmds , doc (showString "}")])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])

instance Print CommSing where
  prt i e = case e of
   CmdExpR expr -> prPrec i 0 (concatD [prt 0 expr , doc (showString ";")])
   CmdWLabel id -> prPrec i 0 (concatD [prt 0 id , doc (showString "::") , doc (showString "\n")] )
   CmdIf expr cmd -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ") ") , doc (showString "then") , prt 0 cmd])
   CmdIfElse expr cmd0 cmd -> prPrec i 0 (concatD [doc (showString "if") , doc (showString "(") , prt 0 expr , doc (showString ") ") , doc (showString "then") , prt 0 cmd0 , doc (showString "else") , prt 0 cmd])
   CmdWhile expr blockloop -> prPrec i 0 (concatD [doc (showString "while") , doc (showString "(") , prt 0 expr , doc (showString ") ") , prt 0 blockloop])
   CmdDoWhile cmd expr -> prPrec i 0 (concatD [doc (showString "repeat") , prt 0 cmd , doc (showString "until") , doc (showString "(") , prt 0 expr , doc (showString ")") , doc (showString ";")])
   CmdFor id exprange expfor blockloop -> prPrec i 0 (concatD [doc (showString "for") , prt 0 id , doc (showString "in") , prt 0 exprange , prt 0 expfor , prt 0 blockloop])
   CmdForSimple id exprange blockloop -> prPrec i 0 (concatD [doc (showString "for") , prt 0 id , doc (showString "in") , prt 0 exprange , prt 0 blockloop])
   CmdSelect expr whens -> prPrec i 0 (concatD [doc (showString "select") , prt 0 expr , doc (showString "{") , prt 0 whens , doc (showString "}")])
   CmdSelectOther expr whens cmd -> prPrec i 0 (concatD [doc (showString "select") , prt 0 expr , doc (showString "{") , prt 0 whens , doc (showString "otherwise") , prt 0 cmd , doc (showString "}")])
   CmdAsgn expl expr -> prPrec i 0 (concatD [prt 0 expl , doc (showString "=") , prt 0 expr , doc (showString ";")])
   CmdDecl decl -> prPrec i 0 (concatD [prt 0 decl])
   CmdGoTo id -> prPrec i 0 (concatD [doc (showString "goto") , prt 0 id , doc (showString ";")])
   CmdReturnFun expr -> prPrec i 0 (concatD [doc (showString "return") , prt 0 expr , doc (showString ";")])
   CmdReturnProc  -> prPrec i 0 (concatD [doc (showString "return") , doc (showString ";")])
   CmdBreak  -> prPrec i 0 (concatD [doc (showString "break") , doc (showString ";")])
   CmdContinue  -> prPrec i 0 (concatD [doc (showString "continue") , doc (showString ";")])
   CmdTryCatch cmd0 cmd -> prPrec i 0 (concatD [doc (showString "try") , prt 0 cmd0 , doc (showString "catch") , prt 0 cmd])


instance Print ExpFor where
  prt i e = case e of
   ExpFor_1 expr -> prPrec i 0 (concatD [doc (showString "by") , prt 0 expr])
   ExpFor_2 expr -> prPrec i 0 (concatD [doc (showString "#") , prt 0 expr])
   ExpFor_3 expr0 expr -> prPrec i 0 (concatD [doc (showString "#") , prt 0 expr0 , doc (showString "by") , prt 0 expr])


instance Print BlockLoop where
  prt i e = case e of
   BlockLoopCmd cmd -> prPrec i 0 (concatD [prt 0 cmd])
   BlockLoop_2 commsing -> prPrec i 0 (concatD [doc (showString "do") , prt 0 commsing])


instance Print When where
  prt i e = case e of
   CmdWhen expr blockloop -> prPrec i 0 (concatD [doc (showString "when") , prt 0 expr , prt 0 blockloop])

  prtList es = case es of
   [] -> (concatD [])
   x:xs -> (concatD [prt 0 x , prt 0 xs])


