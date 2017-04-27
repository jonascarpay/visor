{-# LANGUAGE ViewPatterns #-}
module Screenshot where

import Types
import Conduit
import Static.Image
import Data.Array.Repa.IO.BMP
import System.Directory
import Control.Monad

import Graphics.Win32.Window
import Graphics.Win32.GDI.Bitmap
import Graphics.Win32.GDI.HDC
import Graphics.Win32.GDI.Graphics2D

screenShotSource :: Int -> Int -> Int -> Int -> RTSource (Screenshot a)
screenShotSource x y w h = forever$ liftIO takeshot >>= yield
 where
   takeshot = do _ <- capture x y w h
                 Right img <- readImageFromBMP "out.bmp"
                 removeFile "out.bmp"
                 return (Screenshot img)

capture (fromIntegral -> x) (fromIntegral -> y) (fromIntegral -> w) (fromIntegral -> h) =
  -- Mac
  -- let cmd = "screencapture -xm -R" ++ show x ++ ',':show y ++ ',':show w ++ ',':show h ++ " -t bmp out.bmp"
  -- in system cmd
  -- Win
  do desktop       <- getDesktopWindow -- Grab the Hwnd of the desktop, GetDC 0, GetDC NULL etc all work too
     hdc           <- getWindowDC (Just desktop) -- Get the dc handle of the desktop
     (dx,dy,dr,db) <- getWindowRect desktop -- Find the size of the desktop so we can know which size the destination bitmap should be
     newDC     <- createCompatibleDC (Just hdc) -- Create a new DC to hold the copied image. It should be compatible with the source DC
     newBmp    <- createCompatibleBitmap hdc w h -- Create a new Bitmap which is compatible with the newly created DC
     selBmp    <- selectBitmap newDC newBmp -- Select the Bitmap into the DC, drawing on the DC now draws on the bitmap as well

     let r = r + x
     let b = b + y
                                        -- (left, top, right, bottom)
     bitBlt newDC 0 0 w h hdc x y sRCCOPY -- use SRCCOPY to copy the desktop DC into the newDC
     createBMPFile "out.bmp" newBmp newDC  -- Write out the new Bitmap file to Foo.bmp
     -- putStrLn "Bitmap image copied" -- Some debug message
     deleteBitmap selBmp -- Cleanup the selected bitmap
     deleteBitmap newBmp -- Cleanup the new bitmap
     deleteDC newDC      -- Cleanup the DC we created.
