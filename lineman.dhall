let RawCondition : Type =
  { rcIndex : Natural
  -- Arbitrary index
  , rcEntryPoint : Text
   -- ^ first target where lineman starts recursively from.
   -- target consume 'rel', 'abs' and '~'' paths
  , rcHasFiles : List Text
  -- ^ Target directory has files that have to be relative to target path.
  , rcHasDirectories : List Text
  -- ^ Target directory has directories that have to be relative to target path
  , rcHasExtensions : List Text
  -- ^ Target directory has files with these extensions.
  -- It consume exts with and without '.'
  , rcCommand : Text
  -- ^ Command to run in searched directories
  , rcArgs : List Text
  -- ^ Command's arguments
  , rcConcurrentAgents : Bool
  -- ^ run actions concurrently within the particular condition
  , rcBreakBetweenAgents : Double
  -- ^ Interval between actions in seconds
  }

let condition_1 : RawCondition = 
  { rcIndex = 1
  , rcEntryPoint = "~/source/test/"  
  , rcHasFiles = ["a/log"] : List Text
  , rcHasDirectories = [] : List Text
  , rcHasExtensions = [] : List Text
  , rcCommand = "touch"
  , rcArgs = ["readme.txt"] : List Text
  , rcConcurrentAgents = False
  , rcBreakBetweenAgents = 1.0
  }

let condition_2 : RawCondition = 
  { rcIndex = 2
  , rcEntryPoint = "~/source/test/"  
  , rcHasFiles = ["a/log"] : List Text
  , rcHasDirectories = [] : List Text
  , rcHasExtensions = [] : List Text
  , rcCommand = "rm"
  , rcArgs = ["readme.txt"] : List Text
  , rcConcurrentAgents = False
  , rcBreakBetweenAgents = 1.0
  }

let Verbosity : Type = < V0 | V1 | V2 | V3 >
let Severity : Type =
      < DebugS | InfoS | NoticeS | WarningS | ErrorS | CriticalS | AlertS | EmergencyS >

let Config : Type = 
  { confRawConditions : List RawCondition
   -- ^ within the target it is possible to run several commands with own conditions 
   , confSeverity : Severity
   , confVerbosity : Verbosity
-- ^ level of verbosity
   , confConcurrentSwarms : Bool
  -- ^ run the swarm of actions concurrently
   , confBreakBetweenSwarms : Double
  -- ^ add delay of running next batch of actions (in seconds)
   }

let config : Config = 
  { confRawConditions = [ condition_1, condition_2 ] : List RawCondition
   , confSeverity = Severity.DebugS
   , confVerbosity = Verbosity.V0
   , confConcurrentSwarms = False
   , confBreakBetweenSwarms = 5.0
   }

in config