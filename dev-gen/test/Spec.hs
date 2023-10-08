import Data.Vector (fromList)
import Dev.Gen (PNPMWorkspace (PNPMWorkspace, packages), gen)
import Dev.Gen.ExecSpec qualified as ExecSpec
import Dev.Gen.FileFormat qualified as FileFormat
import Dev.Gen.InterpretSpec (interpret)
import Relude.Applicative (pass)
import Relude.Function (($))
import Relude.Lifted (print)
import Relude.Monad (MonadIO)

main :: (MonadIO m) => m ()
main = do
  print $
    interpret
      [ ExecSpec.readFile "../pnpm-workspace.yaml" FileFormat.YAML (PNPMWorkspace {packages = fromList ["a"]})
      ]
      gen
  pass
