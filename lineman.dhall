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

let Severity : Type = < Debug | Info | Warning | Error >

in { cdTarget = "/home/metaxis/source/haskell/tools/lineman/"
   -- ^ targetDirectory where you plan lineman to run.
   -- targetDirectory consume 'rel', 'abs' and '~'' paths
   , cdConditions = [ condition1 ] : List Condition
   , cdAsync = False
   -- ^ make lineman to work concurrently
   , cdSeverity = Severity.Error
   , cdRichLog = False
   -- ^ Enchance log output
   }