-- let Protocol : Type = < HTTP | HTTPS >
-- let NetworkId : Type = < Mainnet | Testnet : { unNetworkMagic : Natural } >
-- let Verbosity : Type = < V0 | V1 | V2 | V3 >
-- let Severity : Type =
--       < DebugS | InfoS | NoticeS | WarningS | ErrorS | CriticalS | AlertS | EmergencyS >

-- in { icHyperTextProtocol = Protocol.HTTP,
--   icHost = "localhost",
--   icPort = +7000,
--   icNetworkId = NetworkId.Testnet { unNetworkMagic = 1 },
--   icMaestroTokenFilePath = "maestro.token",
--   icCurrencySymbol = {
--     unCurrencySymbol = 0x"fa765a4f65920d1aaa4a072457d27a00d81374245afbe33d94fc1671"
--   },
--   icEnvironment = { getEnvironment = "preprod" },
--   icVerbosity = Verbosity.V0,
--   icSeverity = Severity.DebugS,
--   icFormatMessage = True,
--   icDiscardPeriod = 12*60*60,
--   icCleanDelay = 7*24*60*60
-- }

-- targetDirectory where you plan lineman to run.
-- targetDirectory consume 'rel', 'abs' and '~'' paths
let targetDirectory : Text = "/home/metaxis/source/haskell/tools/lineman/"

let Condition =
  { hasFiles : List Text
  , hasDirectories : List Text
  , hasExtensions : List Text
  , command : Text
  , args : List Text
  }

let condition1 : Condition = 
  { hasFiles = [] : List Text 
  -- ^ Target directory has files
  , hasDirectories = [] : List Text
  -- ^ Target directory has directories
  , hasExtensions = [] : List Text
  -- ^ Target directory has extensions.
  -- It consume exts with and without '.'
  , command = "rm"
  -- ^ Command to run in searched directories
  , args = ["-rf","test_dir"]
  -- ^ Command's arguments
  }

let conditions 
    : List Condition
    = [ condition1 ]

in { targetDirectory = targetDirectory
   , conditions = conditions
   }




-- ./company.dhall

-- let Prelude =
--       https://prelude.dhall-lang.org/v19.0.0/package.dhall sha256:eb693342eb769f782174157eba9b5924cf8ac6793897fc36a31ccbd6f56dafe2

-- let companyName = "Example Dot Com"

-- let User = { name : Text, account : Text, age : Natural }

-- let users
--     : List User
--     = [ { name = "John Doe", account = "john", age = 23 }
--       , { name = "Jane Smith", account = "jane", age = 29 }
--       , { name = "William Allen", account = "bill", age = 41 }
--       ]

-- let toEmail = \(user : User) -> "${user.account}@example.com"

-- let Bio = { name : Text, age : Natural }

-- let toBio = \(user : User) -> user.(Bio)

-- let companySize = Prelude.List.length User users

-- let greetingPage =
--       ''
--       <html>
--       <title>Welcome to ${companyName}!</title>
--       <body>
--       <p>Welcome to our humble company of ${Natural/show companySize} people!</p>
--       </body>
--       </html>
--         ''

-- in  { emails = Prelude.List.map User Text toEmail users
--     , bios = Prelude.List.map User Bio toBio users
--     , greetingPage = greetingPage
--     }