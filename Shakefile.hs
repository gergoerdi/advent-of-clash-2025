{-# LANGUAGE RecordWildCards, BlockArguments #-}
import Prelude

import Clash.Shake
import Clash.Shake.Xilinx as Xilinx
import qualified Clash.Shake.F4PGA as F4PGA
import Clash.Shake.ECP5 as ECP5

import Development.Shake
import Development.Shake.FilePath
import Data.Traversable (for)
import Data.Foldable (for_)

outDir :: FilePath
outDir = "_build"

boards :: [(String, FilePath, SynthRules, [FilePath])]
boards =
    [ ("nexys-a7-50t", "nexys-a7-50t", Xilinx.vivado nexysA750T, [])
    , ("nexys-a7-50t.f4pga", "nexys-a7-50t", F4PGA.xilinx7 nexysA750T, [])
    , ("ulx3s-45f", "ulx3s", ECP5.ecp5 "45k", ["clock.v"])
    , ("ulx3s-85f", "ulx3s", ECP5.ecp5 "85k", ["clock.v"])
    ]

problems :: [String]
problems = ["P03"]

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } do
    useConfig "build.mk"

    phony "clean" do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    outDir </> "target/ulx3s/clock.v" %> \out -> do
        let yosys :: String -> [String] -> Action ()
            yosys tool args = cmd_ (EchoStdout False) =<< toolchain "YOSYS" tool args

        alwaysRerun
        yosys "ecppll"
          [ "--module", "pll"
          , "--clkin", "25"
          , "--clkin_name", "clkin_25mhz"
          , "--clkout0", "100"
          , "--clkout0_name", "clkout_100mhz"
          , "--highres"
          , "-f", out
          ]

    for_ problems \problem -> do
        let mod = "AoC2025." <> problem <> ".TopEntity"
        (clash, kit) <- clashRules (outDir </> problem </> "clash") Verilog
            [ "src" ]
            mod
            [ "-Wno-partial-type-signatures"
            , "-fplugin=Protocols.Plugin"
            ] $
          return ()

        phony (problem <> ":clashi") $
          clash ["--interactive", "src/AoC2025" </> problem </> "TopEntity" <.> "hs"]

        for_ boards \(name, targetName, synth, extras) -> do
            let targetDir = "target" </> targetName

            SynthKit{..} <- synth kit (outDir </> problem </> name </> "synth") "Top" do
                statics <- staticFiles targetDir
                let others = [outDir </> "target" </> targetName </> fileName | fileName <- extras]
                need others
                pure (statics <> others)

            mapM_ (uncurry $ nestedPhony (problem <> ":" <> name)) $
              ("bitfile", need [bitfile]):phonies
