let RawCondition : Type =
  { rcIndex : Natural
  , rcTarget : Text
  , rcHasFiles : List Text
  , rcHasDirectories : List Text
  , rcHasExtensions : List Text
  , rcCommand : Text
  , rcArgs : List Text
  , rcActConcurrent : Bool
  , rcWithBreak : Natural
  }

let condition_1 : RawCondition = 
  { rcIndex = 1
  -- Arbitrary index
  , rcTarget = "~/source/test/"  
   -- ^ target where you plan that the lineman recursively starts from.
   -- target consume 'rel', 'abs' and '~'' paths
  , rcHasFiles = ["log"] : List Text
  -- ^ Target directory has files
  , rcHasDirectories = [] : List Text
  -- ^ Target directory has directories
  , rcHasExtensions = [] : List Text
  -- ^ Target directory has extensions.
  -- It consume exts with and without '.'
  , rcCommand = "kate"
  -- ^ Command to run in searched directories
  , rcArgs = ["log"] : List Text
  -- ^ Command's arguments
  , rcActConcurrent = True
  -- ^ run actions concurrently within the particular condition
  , rcWithBreak = 0 
  -- ^ Interval between actions in microseconds
  }

let Verbosity : Type = < V0 | V1 | V2 | V3 >
-- ^ levels of verbosity
let Severity : Type =
      < DebugS | InfoS | NoticeS | WarningS | ErrorS | CriticalS | AlertS | EmergencyS >

in { confRawConditions = [ condition_1 ] : List RawCondition
   -- ^ within the target it is possible to run several commands with own conditions 
   , confSeverity = Severity.DebugS
   , confVerbosity = Verbosity.V0
   , confSwarmConcurrent = False
  -- ^ run the swarm of actions concurrently
   }