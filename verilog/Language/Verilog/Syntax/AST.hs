--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Verilog.Syntax.AST
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : ghc
--
-- An abstract syntax tree (AST) for Verilog.  We used the following to help
-- write the definition of the AST:
--
--  * <http://www.verilog.com/VerilogBNF.html>
--
--  * <http://www.hdlworks.com/hdl_corner/verilog_ref/index.html>
--
--  * <http://en.wikipedia.org/wiki/Verilog>
--
-- This AST is very close to the concrete syntax, and is only meant for
-- generating, pretty printing, and parsing.  To do anything more advanced, it
-- should be converted into another form.
--
-- The AST is incomplete in many places, and deviates from the spec in a few
-- places, too.  We've made an effort to document these places.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable
, TypeOperators
, GADTs
, GADTSyntax
, ExistentialQuantification
, FlexibleInstances
, StandaloneDeriving #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Verilog.Syntax.AST
  (
  -- * The top-level types
  Verilog(..), Module(..), Description(..),

  -- * User-defined primitives
  UDP(..), UDPDecl(..), UDPInitialStatement(..),
  TableDefinition(..), CombinationalEntry(..), SequentialEntry(..),
  LevelSymbol, levelSymbols, validLevelSymbol,
  OutputSymbol, outputSymbols, validOutputSymbol,
  NextState, nextStates, validNextState,
  Edge(..), EdgeSymbol, edgeSymbols, validEdgeSymbol,

  -- * Items and Declarations
  PortItem(..), NonPortItem(..), Item(..), FunctionType(..), LocalDecl(..),
  ParamDecl(..), InputDecl(..), OutputDecl(..), OutputRegDecl(..), InOutDecl(..),
  NetDecl(..), RegDecl(..), RegType(..), EventDecl(..),

  -- * Module, UDP and Primitive Instantiations
  PrimitiveInst(..), PrimInst(..), PrimInstName(..), PrimType(..),
  Instance(..), Parameter(..), Inst(..), Connections(..), NamedConnection(..),

  -- * Behavioral Statements
  Statement(..), Assignment(..), LValue,
  CaseWord(..), CaseItem(..), BlockDecl(..),

  -- * Expressions
  Expression, ConstExpr, Expression'(..), ConstExpr',
  Number(..), Base(..), Sign(..), intExpr,
  UnaryOp(..), BinaryOp(..),

  -- * Miscellaneous
  Ident(..), ParamAssign(..), ExpandRange(..), Range(..), RegVar(..),
  AssignmentControl(..), DelayControl, EventControl(..), Delay,
  EventExpr(..), ScalarEventExpr,
  ChargeStrength, DriveStrength(..),
  Strength0(..), Strength1(..), NetType(..)
  ) where


import Data.Generics    ( Data, Typeable )

import Language.Verilog.Syntax.Ident
import Language.Verilog.Syntax.Expression

-- -----------------------------------------------------------------------------
-- 1. Source Text

newtype Verilog = Verilog [Description]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | This should be module or user-defined primitive, but for now we are only
-- supporting module.
data Description
  = ModuleDescription Module
  | UDPDescription UDP
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A top-level module has the module name, the list of ports (both input and
-- output), and the body of the module (a list of declarations).  In the spec,
-- the ports have a more complicated type than simply @Ident@.
data Module
  = Module Ident     -- The name of module
           [Ident]   -- The list of ports, including both inputs and outputs
                     -- In the spec, this is a more complicated type.
           [Item]    -- The module's body, a list of declarations.
  | ModulePortDecl Ident
    [PortItem]
    [NonPortItem]

deriving instance Eq Module
deriving instance Ord Module
deriving instance Show Module
deriving instance Data Module
deriving instance Typeable Module

data Item where
  Port :: PortItem -> Item
  NonPort :: NonPortItem -> Item

deriving instance Eq Item
deriving instance Ord Item
deriving instance Show Item
deriving instance Data Item
deriving instance Typeable Item

data PortItem
  = InputDeclItem InputDecl
  | OutputDeclItem OutputDecl
  | OutputRegDeclItem OutputRegDecl
  | InOutDeclItem InOutDecl
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A declaration.
data NonPortItem
  = ParamDeclItem ParamDecl
  | NetDeclItem NetDecl
  | RegDeclItem RegDecl
  | EventDeclItem EventDecl
  | PrimitiveInstItem PrimitiveInst
  | InstanceItem Instance
  | ParamOverrideItem [ParamAssign]
  | AssignItem (Maybe DriveStrength) (Maybe Delay) [Assignment]
  -- TODO: SpecifyBlock
  | InitialItem Statement
  | AlwaysItem Statement
  | TaskItem Ident [LocalDecl] Statement
  | FunctionItem (Maybe FunctionType) Ident [LocalDecl] Statement
  | CommentItem String
  deriving (Eq, Ord, Show, Data, Typeable)

-- --------------------

-- | User-defined primitive (UDP)
data UDP
  = UDP Ident     -- Name of UDP
        Ident     -- Name of output variable
        [Ident]   -- Name of input variables
        [UDPDecl] -- input/output/reg declarations
        (Maybe UDPInitialStatement)
        TableDefinition
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A UDP can have output, input, and reg declarations.
data UDPDecl
  = UDPOutputDecl OutputDecl
  | UDPInputDecl InputDecl
  | UDPRegDecl Ident
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A UDP initial statement defines the initial value of a UDP.
data UDPInitialStatement
  = UDPInitialStatement Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- According to the spec, the initial value of a UDP can be any of the
-- following, but we don't make that restriction in the AST.
{-
<init_val>
::= 1'b0
||= 1'b1
||= 1'bx
||= 1'bX
||= 1'B0
||= 1'B1
||= 1'Bx
||= 1'BX
||= 1
||= 0
-}

-- | A UDP's definition is a truth table.
data TableDefinition
  = CombinationalTable [CombinationalEntry]
  | SequentialTable [SequentialEntry]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An entry in a combinational table.
data CombinationalEntry
  = CombinationalEntry [LevelSymbol] OutputSymbol
  deriving (Eq, Ord, Show, Data, Typeable)

type LevelSymbol = Char

-- | A level symbol is one of the following characters:
-- 0   1   x   X   ?   b   B
levelSymbols :: [LevelSymbol]
levelSymbols = "01xX?bB"

validLevelSymbol :: Char -> Bool
validLevelSymbol = flip elem levelSymbols

type OutputSymbol = Char

-- | An output symbol is one of the following characters:
-- 0   1   x   X   ?   b   B
outputSymbols :: [OutputSymbol]
outputSymbols = "01xX"

validOutputSymbol :: Char -> Bool
validOutputSymbol = flip elem outputSymbols

data SequentialEntry
  = SequentialEntry [Either LevelSymbol Edge] LevelSymbol NextState
  deriving (Eq, Ord, Show, Data, Typeable)

data Edge
  = EdgeLevels LevelSymbol LevelSymbol
  | EdgeSymbol EdgeSymbol
  deriving (Eq, Ord, Show, Data, Typeable)

type EdgeSymbol = Char

edgeSymbols :: [Char]
edgeSymbols = "rRfFpPnN*"

validEdgeSymbol :: Char -> Bool
validEdgeSymbol = flip elem edgeSymbols

type NextState = Char

nextStates :: [NextState]
nextStates = outputSymbols ++ "-"

validNextState :: NextState -> Bool
validNextState = flip elem nextStates

-- -----------------------------------------------------------------------------
-- 2. Declarations

data FunctionType
  = FunctionTypeRange Range
  | FunctionTypeInteger
  | FunctionTypeReal
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A local declaration in a task or function definition, including the ports
-- of the task or function. Functions are not allowed to have local 'output' and
-- 'inout' declarations, but we do not make that restriction here.
data LocalDecl
  = LocalParamDecl ParamDecl
  | LocalInputDecl InputDecl
  | LocalOutputDecl OutputDecl
  | LocalInOutDecl InOutDecl
  | LocalRegDecl RegDecl
  deriving (Eq, Ord, Show, Data, Typeable)

newtype ParamDecl
  = ParamDecl [ParamAssign]
  deriving (Eq, Ord, Show, Data, Typeable)

data InputDecl
  = InputDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data OutputDecl
  = OutputDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data OutputRegDecl
  = OutputRegDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data InOutDecl
  = InOutDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data NetDecl
  = NetDecl NetType (Maybe ExpandRange) (Maybe Delay) [Ident]
  | NetDeclAssign NetType (Maybe DriveStrength) (Maybe ExpandRange)
    (Maybe Delay) [(Ident, Expression)]
  -- TODO trireg <charge_strength>? <expandrange>? <delay>?
  -- TODO allow mixing assignment and non-assignments, such as:
  --      wire x = 1, y, z = 0;
  deriving (Eq, Ord, Show, Data, Typeable)

-- note that only the "reg" type allows for a vector range before the RegVar,
-- but we don't make that restriction in the AST.
data RegDecl
  = RegDecl RegType (Maybe Range) [RegVar]
  deriving (Eq, Ord, Show, Data, Typeable)

-- TODO support multi-dimensional array
data RegVar
  = RegVar Ident (Maybe Expression) -- ^ with optional initial value
  | MemVar Ident Range              -- ^ array
  deriving (Eq, Ord, Show, Data, Typeable)

newtype EventDecl
  = EventDecl [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- Module, UDP, and primitive instances

data PrimitiveInst
  = PrimitiveInst PrimType (Maybe DriveStrength) (Maybe Delay) [PrimInst]
  deriving (Eq, Ord, Show, Data, Typeable)

data PrimInst
  = PrimInst (Maybe PrimInstName) [Expression]
  deriving (Eq, Ord, Show, Data, Typeable)

data PrimInstName
  = PrimInstName Ident (Maybe Range)
  deriving (Eq, Ord, Show, Data, Typeable)

data PrimType
  = Gate_and | Gate_nand | Gate_or | Gate_nor | Gate_xor | Gate_xnor | Gate_not
  | Gate_buf | Gate_bufif0 | Gate_bufif1
  | Gate_notif0 | Gate_notify1 | Gate_pulldown | Gate_pullup
  | Gate_nmos | Gate_rnmos | Gate_pmos | Gate_rpmos | Gate_cmos | Gate_rcmos
  | Gate_tran | Gate_rtran
  | Gate_tranif0 | Gate_rtranif0 | Gate_tranif1 | Gate_rtranif1
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show PrimType where
  show Gate_and      = "and"
  show Gate_nand     = "nand"
  show Gate_or       = "or"
  show Gate_nor      = "nor"
  show Gate_xor      = "xor"
  show Gate_xnor     = "xnor"
  show Gate_not      = "not"
  show Gate_buf      = "buf"
  show Gate_bufif0   = "bufif0"
  show Gate_bufif1   = "bufif1"
  show Gate_notif0   = "notif0"
  show Gate_notify1  = "notify1"
  show Gate_pulldown = "pulldown"
  show Gate_pullup   = "pullup"
  show Gate_nmos     = "nmos"
  show Gate_rnmos    = "rnmos"
  show Gate_pmos     = "pmos"
  show Gate_rpmos    = "rpmos"
  show Gate_cmos     = "cmos"
  show Gate_rcmos    = "rcmos"
  show Gate_tran     = "tran"
  show Gate_rtran    = "rtran"
  show Gate_tranif0  = "tranif0"
  show Gate_rtranif0 = "rtranif0"
  show Gate_tranif1  = "tranif1"
  show Gate_rtranif1 = "rtranif1"

-- --------------------
-- module and udp instantiations

-- | A module or UDP instantiation.  The syntax for module and UDP instances is
-- ambiguous.  If there is a @#@ followed by an expression, there is no way to
-- know whether the expression is a delay for a UDP instance or a parameter
-- assignment for a module instance, without looking at the names of the UDP
-- declarations that are in scope (which is not a job for the parser).

data Instance
  = Instance
    Ident                             -- Name of the module (not the instance)
    (Either [Expression] [Parameter]) -- A list of delay expressions or
                                      -- unnamed parameter expressions, or a
                                      -- list of named parameter assignments
    [Inst]                            -- Module instances
  deriving (Eq, Ord, Show, Data, Typeable)

-- the spec says this is just one or more expressions, but that doesn't seem
-- right to me.  It should be a mapping of parameter names to expression values.
-- I suspect that the spec is outdated (applies to an older Verilog standard).
-- | A parameter value assignment is used in a module instance to associate a
-- parameter with a value.
data Parameter
  = Parameter Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A module instance.  The name of the module and the parameter assignments
-- are defined in @ModuleInst@, which has any number of @Instance@s.  The
-- instance itself has a name and a list of port connections.
data Inst
  = Inst Ident          -- Name of the instance
         (Maybe Range)  -- I'm actually not sure what this is for!
         Connections    -- The (input and output) port connections.
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Connections in a module instance can be all nnammed or all unnamed.  When
-- they are unnamed, this means they are positional, like in most programming
-- languages.  However, when the formal ports are named, then you can specify
-- the connections in any order.
data Connections
  = Connections [Expression]
  | NamedConnections [NamedConnection]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A named connection, like a parameter value assignment, associates a port
-- with a value in a module instance.
data NamedConnection
  = NamedConnection Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- ----------------------------------------------------------------------------
-- 5. Behavioral Statements

-- | Behavioral statements.
data Statement
  -- | blocking assignment, e.g. @x \= y@
  = BlockingAssignment LValue (Maybe AssignmentControl) Expression
  -- | non-blocking assignment, e.g. @x \<= y@
  | NonBlockingAssignment LValue (Maybe AssignmentControl) Expression
  -- | @if@ statement.  Both branches are optional.
  | IfStmt Expression (Maybe Statement) (Maybe Statement)
  -- | @case@, @casex@, or @casez@ statement.
  | CaseStmt CaseWord Expression [CaseItem]
  -- | @forever@ statement, e.g. @forever $display(\"Hello!\")@;
  | ForeverStmt Statement
  -- | @repeat@ statement, e.g. @repeat (10) \@(posedge clk);@
  | RepeatStmt Expression Statement
  -- | @while@ statement, e.g. @while (x < 10) x = x + 1;@
  | WhileStmt Expression Statement
  -- | @for@ statement, e.g. @for (i = 0; i < 10; i = i + 1) $display(\"%d\", i);@
  | ForStmt Assignment Expression Assignment Statement
  -- | @delay@ statement, e.g. @\#10 x = 1;@
  | DelayStmt Delay (Maybe Statement)
  -- | Control statement triggered by an event, e.g. @\@(posedge clk) x <= 10;@
  | EventControlStmt EventControl (Maybe Statement)
  -- | @wait@ statement, e.g. @wait (x) y <= 0;@
  | WaitStmt Expression (Maybe Statement)
  -- TODO: -> <name_of_event>
  -- | a sequence of statements between @begin@ and @end@ keywords.
  | SeqBlock (Maybe Ident) [BlockDecl] [Statement]
  -- | a set of parallel statements, enclosed between @fork@ and @join@ keywords.
  | ParBlock (Maybe Ident) [BlockDecl] [Statement]
  -- | call a task, with optional arguments like in a function call.
  | TaskStmt Ident (Maybe [Expression])
  | TaskEnableStmt Ident [Expression]
  -- TODO SystemTaskEnableStmt Ident [Expression]
  | DisableStmt Ident
  -- | @assign@ statement (like continuous assignment).
  | AssignStmt Assignment
  -- | @deassign@ statement.
  | DeAssignStmt LValue
  -- | @force@ statement.
  | ForceStmt Assignment
  -- | @release@ statement.
  | ReleaseStmt LValue
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An assignment.
data Assignment
  = Assignment LValue Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- A LValue is supposed to be limited to the following:
--    ident | ident[expr] | ident[const_expr : const_expr ] | concatentation
-- However, we don't make that restriction in our AST.
type LValue = Expression

data CaseWord
  = Case | Casex | Casez
  deriving (Eq, Ord, Data, Typeable)

instance Show CaseWord where
  show Case  = "case"
  show Casex = "casex"
  show Casez = "casez"

-- | One case item in a @case@ statement.
data CaseItem
  = CaseItem [Expression] (Maybe Statement)
  | CaseDefault (Maybe Statement)
  deriving (Eq, Ord, Show, Data, Typeable)

data BlockDecl
  = ParamDeclBlock ParamDecl
  | RegDeclBlock RegDecl
  | EventDeclBlock EventDecl
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- Miscellaneous

-- | Assign a parameter in its declaration.
data ParamAssign
  = ParamAssign Ident ConstExpr
  deriving (Eq, Ord, Show, Data, Typeable)

data ExpandRange
  = SimpleRange Range
  | ScalaredRange Range
  | VectoredRange Range
  deriving (Eq, Ord, Show, Data, Typeable)

data Range
  = Range ConstExpr ConstExpr
  deriving (Eq, Ord, Show, Data, Typeable)

-- | The optional timing control for a procedural assignment statement
-- (i.e. blocking and nonblocking assignments)
data AssignmentControl
  = DelayControl Delay
  | EventControl EventControl
  | RepeatControl Expression EventControl
  deriving (Eq, Ord, Show, Data, Typeable)

data EventControl
  = EventControlIdent Ident     -- ^ @\@identifier@
  | EventControlExpr EventExpr  -- ^ @\@(event_expression)@
  | EventControlWildCard        -- ^ @\@\*@
  deriving (Eq, Ord, Show, Data, Typeable)

type DelayControl = Delay

-- the actual syntax for <delay> is:
-- <number> | <identifier> | <mintypmax_expression>,
-- where mintypmax_expression can be expression.
-- so, we just use expression here.

type Delay = Expression

data EventExpr
  = EventExpr Expression
  | EventPosedge ScalarEventExpr
  | EventNegedge ScalarEventExpr
  | EventOr EventExpr EventExpr
  deriving (Eq, Ord, Show, Data, Typeable)

-- From the spec:
-- "Scalar event expression is an expression that resolves to a one bit value."
type ScalarEventExpr = Expression

data ChargeStrength
  = Charge_small | Charge_medium | Charge_large
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show ChargeStrength where
  show Charge_small  = "small"
  show Charge_medium = "medium"
  show Charge_large  = "large"

data DriveStrength
  = Strength01 Strength0 Strength1
  | Strength10 Strength1 Strength0
  deriving (Eq, Ord, Show, Data, Typeable)

data Strength0
  = Supply0 | Strong0 | Pull0 | Weak0 | Highz0
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show Strength0 where
  show Supply0 = "supply0"
  show Strong0 = "strong0"
  show Pull0   = "pull0"
  show Weak0   = "weak0"
  show Highz0  = "highz0"

data Strength1
  = Supply1 | Strong1 | Pull1 | Weak1 | Highz1
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show Strength1 where
  show Supply1 = "supply1"
  show Strong1 = "strong1"
  show Pull1   = "pull1"
  show Weak1   = "weak1"
  show Highz1  = "highz1"

data NetType
  = Net_wire | Net_tri | Net_tri1 | Net_supply0
  | Net_wand | Net_triand | Net_tri0 | Net_supply1 | Net_wor
  | Net_trior | Net_triref
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show NetType where
  show Net_wire    = "wire"
  show Net_tri     = "tri"
  show Net_tri1    = "tri1"
  show Net_supply0 = "supply0"
  show Net_wand    = "wand"
  show Net_triand  = "triand"
  show Net_tri0    = "tri0"
  show Net_supply1 = "supply1"
  show Net_wor     = "wor"
  show Net_trior   = "trior"
  show Net_triref  = "triref"

data RegType
  = Reg_reg      -- ^ unsigned variable of any size = "
  | Reg_integer  -- ^ signed 32-bit variable
  | Reg_time     -- ^ unsigned 64-bit variable
  | Reg_real     -- ^ double-precision floating point variable
  | Reg_realtime -- ^ (same as above)
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show RegType where
  show Reg_reg      = "reg"
  show Reg_integer  = "integer"
  show Reg_time     = "time"
  show Reg_real     = "real"
  show Reg_realtime = "real"

-- -----------------------------------------------------------------------------
-- GENERATED START
-- GENERATED STOP
