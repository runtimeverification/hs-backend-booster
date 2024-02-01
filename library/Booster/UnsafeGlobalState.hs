module Booster.UnsafeGlobalState (
    unsafeReadGlobalMaxIterations,
    unsafeReadGlobalMaxRecursion,
    writeGlobalMaxIterations,
    writeGlobalMaxRecursion,
) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Booster.Util (Bound (..))

{-# NOINLINE globalMaxIterations #-}
{-# DEPRECATED globalMaxIterations "DO NOT MERGE TO MAIN" #-}
globalMaxIterations :: IORef (Bound "Iterations")
globalMaxIterations = unsafePerformIO (newIORef 100)

{-# NOINLINE unsafeReadGlobalMaxIterations #-}
{-# DEPRECATED unsafeReadGlobalMaxIterations "DO NOT MERGE TO MAIN" #-}
unsafeReadGlobalMaxIterations :: Bound "Iterations"
unsafeReadGlobalMaxIterations = unsafePerformIO $ readIORef globalMaxIterations

{-# DEPRECATED writeGlobalMaxIterations "DO NOT MERGE TO MAIN" #-}
writeGlobalMaxIterations :: Bound "Iterations" -> IO ()
writeGlobalMaxIterations = atomicWriteIORef globalMaxIterations

{-# NOINLINE globalMaxRecursion #-}
{-# DEPRECATED globalMaxRecursion "DO NOT MERGE TO MAIN" #-}
globalMaxRecursion :: IORef (Bound "Recursion")
globalMaxRecursion = unsafePerformIO (newIORef 5)

{-# NOINLINE unsafeReadGlobalMaxRecursion #-}
{-# DEPRECATED unsafeReadGlobalMaxRecursion "DO NOT MERGE TO MAIN" #-}
unsafeReadGlobalMaxRecursion :: Bound "Recursion"
unsafeReadGlobalMaxRecursion = unsafePerformIO $ readIORef globalMaxRecursion

{-# DEPRECATED writeGlobalMaxRecursion "DO NOT MERGE TO MAIN" #-}
writeGlobalMaxRecursion :: Bound "Recursion" -> IO ()
writeGlobalMaxRecursion = atomicWriteIORef globalMaxRecursion
