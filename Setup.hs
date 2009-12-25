module Main where

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Text
import System.FilePath
import System.Directory

main = defaultMainWithHooks simpleUserHooks
	{ hookedPrograms = [isccProgram]
	, postBuild = myPostBuild
	}

isccProgram = simpleProgram "ISCC"

myPostBuild _ flags pd lbi = do
	case lookupProgram isccProgram (withPrograms lbi) of
	  Nothing -> warn verb $ "The INNO Setup compile ISCC was not found, skipping the " ++
	                         "creation of the windows setup executable."
	  Just configuredProg -> do
	  	writeFile includeFilename $ "AppVerName=" ++ display (package pd) ++ "\n"
	  	rawSystemProgram verb configuredProg
		             	["/Odist","/F"++setupFilename,"setup.iss"]
		removeFile includeFilename
  where verb = fromFlag (buildVerbosity flags)
	setupFilename = display (pkgName (package pd)) ++
	                "-setup-" ++
			display (pkgVersion (package pd))
	includeFilename = "dist" </> "setup-app-ver-name.iss"
