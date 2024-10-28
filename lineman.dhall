let Condition : Type =
  { hasFiles : List Text
  , hasDirectories : List Text
  , hasExtensions : List Text
  , command : Text
  , args : List Text
  }

let condition1 : Condition = 
  { hasFiles = ["lineman.cabal"] : List Text 
  -- ^ Target directory has files
  , hasDirectories = [] : List Text
  -- ^ Target directory has directories
  , hasExtensions = [] : List Text
  -- ^ Target directory has extensions.
  -- It consume exts with and without '.'
  , command = "mkdir"
  -- ^ Command to run in searched directories
  , args = ["test_dir"]
  -- ^ Command's arguments
  }

let Verbosity : Type = < V0 | V1 | V2 | V3 >
-- ^ levels of verbosity
let Severity : Type =
      < DebugS | InfoS | NoticeS | WarningS | ErrorS | CriticalS | AlertS | EmergencyS >

in { cTarget = "/home/metaxis/source/haskell/tools/lineman/"
   -- ^ target where you plan that the lineman recursively starts from.
   -- target consume 'rel', 'abs' and '~'' paths
   , cConditions = [ condition1 ] : List Condition
   -- ^ within the target one can run several commands with its own conditions 
   , cAsync = False
   -- ^ make lineman to work concurrently
   , cSeverity = Severity.InfoS
   , cVerbosity = Verbosity.V0
   }