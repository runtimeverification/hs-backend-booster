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
globalMaxIterations :: IORef (Bound "Iterations")
globalMaxIterations = unsafePerformIO (newIORef 100)

{-# NOINLINE unsafeReadGlobalMaxIterations #-}
unsafeReadGlobalMaxIterations :: Bound "Iterations"
unsafeReadGlobalMaxIterations = unsafePerformIO $ readIORef globalMaxIterations

writeGlobalMaxIterations :: Bound "Iterations" -> IO ()
writeGlobalMaxIterations = atomicWriteIORef globalMaxIterations

{-# NOINLINE globalMaxRecursion #-}
globalMaxRecursion :: IORef (Bound "Recursion")
globalMaxRecursion = unsafePerformIO (newIORef 5)

{-# NOINLINE unsafeReadGlobalMaxRecursion #-}
unsafeReadGlobalMaxRecursion :: Bound "Recursion"
unsafeReadGlobalMaxRecursion = unsafePerformIO $ readIORef globalMaxRecursion

writeGlobalMaxRecursion :: Bound "Recursion" -> IO ()
writeGlobalMaxRecursion = atomicWriteIORef globalMaxRecursion
