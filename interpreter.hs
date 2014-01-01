
-- ==================================
-- Types

type Ident = String	    		-- a name or identifier in BIL
type State = [(Ident, Int)]   	-- a list of identifier, value bindings (dynamic)

-- ==================================
-- Data Types

data Program = Prog [Decl] [Stmt]
	deriving (Show)

data Decl = Variable String
	deriving (Show)

data Stmt = Assign String Expr
		| Loop BoolExpr [Stmt]
	deriving (Show)

data Expr = Plus Expr Expr			-- +
		| Times Expr Expr 			-- *
		| Inc Ident 				-- ++
		| Var Ident
		| Val Int
	deriving (Show)
	
data BoolExpr = Eq Expr Expr		-- ==
		| Neq Expr Expr				-- !=
		| Lt Expr Expr				-- <
		| Gt Expr Expr				-- >
		| Lte Expr Expr				-- <=
		| Gte Expr Expr				-- >=
	deriving (Show)
	
-- ===================================
-- Interpreter functions

exec :: Program -> State -- executing program returns a final state 
exec (Prog decls stmts)	= run (buildState decls) stmts
exec (Prog _ stmts)		= []
exec (Prog decls _)		= []

run :: State -> [Stmt] -> State -- runs the program through the provided statements with the given state
run state (s:stmts)	= run (evalStmt s state) stmts
run state []		= state

while :: BoolExpr -> [Stmt] -> State -> State -- runs the provided statements and updates the state while BoolExpr -> True
while expr stmts state = if evalBool expr state then while expr stmts (run state stmts) else state

buildState :: [Decl] -> State -- builds a state from the initial variable declarations
buildState []			= []
buildState (d:decls)	= elab d (buildState decls)

getVal :: Ident -> State -> Int -- return the integer binding of a variable in a given state
getVal var (s:st)	= if var == fst s then snd s else getVal var st
getVal var []		= -1

setVal :: Ident -> Int -> State -> State -- sets the integer binding of a variable and returns the updated state
setVal var val (s:st)	= (if var == fst s then (var, val) else s) : setVal var val st
setVal var val []		= []

eval :: Expr -> State -> Int -- evaluate an expression 
eval (Plus x y) st		= (eval x st) + (eval y st)			-- x + y
eval (Times x y) st		= (eval x st) * (eval y st)			-- x * y
eval (Inc x) st			= (getVal x st) + 1					-- x++
eval (Var x) st			= (getVal x st)
eval (Val x) st			= x

evalBool :: BoolExpr -> State -> Bool -- evaluate a boolean expression
evalBool (Eq x y) st	= (eval x st) == (eval y st)		-- x == y
evalBool (Neq x y) st	= (eval x st) /= (eval y st)		-- x != y
evalBool (Lt x y) st	= (eval x st) <  (eval y st)		-- x < y
evalBool (Gt x y) st	= (eval x st) >  (eval y st)		-- x > y
evalBool (Lte x y) st	= (eval x st) <= (eval y st)		-- x <= y
evalBool (Gte x y) st	= (eval x st) >= (eval y st)		-- x >= y

evalStmt :: Stmt -> State -> State -- evaluates a statement
evalStmt (Assign var expr) state	= setVal var (eval expr state) state
evalStmt (Loop expr stmts) state	= while expr stmts state

elab :: Decl -> State -> State -- add a declaration to a state / create a new state with a declaration
elab dec st = ((getDeclVar dec), 0) : st

getDeclVar :: Decl -> String -- get the string associated with a declared variable
getDeclVar (Variable x) = x

-- ===================================
-- Test programs

-- #1

decl1 = Variable "x"							-- int x;
decl2 = Variable "y"							-- int y;
decls = [decl1, decl2]

stmt1 = Assign "x" (Val 1)						-- x = 1;
stmt2 = Assign "x" (Inc ("x"))					-- x++;
stmt3 = Assign "y" (Plus (Var "x") (Val 4))		-- y = x + 4;
stmts = [stmt1, stmt2, stmt3]

smallBil = Prog decls stmts

-- #2

decla = Variable "a"							-- int a;
declb = Variable "b"							-- int b;
declc = Variable "c"							-- int c;
decls2 = [decla, declb, declc]

stmta = Assign "a" (Val 2)						-- a = 2;
stmtb = Assign "b" (Val 3)						-- b = 3;
stmtc = Assign "c" (Plus (Var "a") (Var "b"))	-- c = a + b;
stmtd = Assign "b" (Inc ("b"))					-- b++;
stmte = Assign "a" (Times (Var "b") (Var "c"))  -- a = b * c
stmts2 = [stmta, stmtb, stmtc, stmtd, stmte]

testProg = Prog decls2 stmts2

-- #4
decl4 = Variable "i"									-- int i
decl5 = Variable "j"									-- int j
decls1 = [decl4, decl5]

stmt4 = Loop (Lt (Var "i") (Val 5)) [stmt5, stmt6]		-- while(i < 5) {
stmt5 = Assign "i" (Inc ("i"))							-- i++
stmt6 = Assign "j" (Plus (Var "j") (Val 2))				-- j += 2 }
stmts1 = [stmt4]

whileTest = Prog decls1 stmts1