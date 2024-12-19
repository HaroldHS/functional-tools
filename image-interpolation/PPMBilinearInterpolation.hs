import Data.Bool
import Data.List
import GHC.Num
import Text.Printf

-- splitByN (list)
splitByN :: Int -> [a] -> [[a]]
splitByN n [] = []
splitByN n l = [take n l] ++ splitByN n (drop n l)

-- convertEachPixel (list of lines from file content)
convertEachPixel :: [String] -> [[Integer]]
convertEachPixel content = do
  let splitContent = map (words) content
  let pixelElement = concatMap (\x -> splitByN 3 x) splitContent
  -- Don't use 'return' to prevent the result being wrapped with monad
  map (\eachPixel -> map (\eachChannelInPixel -> integerFromInt (read (eachChannelInPixel) :: Int)) eachPixel) pixelElement

-- takeChannel (channel code) (result of convertEachPixel)
takeChannel :: Char -> [[Integer]] -> [Integer]
takeChannel _ []     = []
takeChannel c (x:xs) | c == 'r' = [x!!0] ++ takeChannel c xs
                     | c == 'g' = [x!!1] ++ takeChannel c xs
                     | c == 'b' = [x!!2] ++ takeChannel c xs

-- groupChannel (upscaled red channel) (upscaled green channel) (upscaled blue channel)
groupChannel :: [[Integer]] -> [[Integer]] -> [[Integer]] -> [[Integer]]
groupChannel rc gc bc = zipWith3 (\r g b -> [r, g, b]) (concat rc) (concat gc) (concat bc)

-- bilinearInterpolate (num of interpolation) (p1) (p2)
bilinearInterpolate :: Num b => Int -> Integer -> Integer -> [b]
bilinearInterpolate n p1 p2 = map (\q -> fromIntegral $ round $ (fromInteger p1)*(1-q) + (fromInteger p2)*q) (
                                take n $ tail [i | i <- [0.0, 1.0/(fromIntegral (n+1)) .. 1.0]]
                              )

-- bilinearInterpolateHorizontal (num of interpolation) (list of values / horizontal array)
bilinearInterpolateHorizontal :: Int -> [Integer] -> [Integer]
bilinearInterpolateHorizontal n l = if (length l) == 1 then
                                      l
                                    else
                                      ((\p -> [head p] ++ bilinearInterpolate n (head p) (head $ tail p)) $ take 2 l)
                                      ++ bilinearInterpolateHorizontal n (drop 1 l)

-- iterateAndInterpolateHorizontal (num of interpolation) (result of convertEachPixel)
iterateAndInterpolateHorizontal :: Int -> [[Integer]] -> [[Integer]]
iterateAndInterpolateHorizontal n l = concatMap (\x -> [bilinearInterpolateHorizontal n x]) l

-- iterateAndInterpolateVertical (num of interpolation) (result of iterateAndInterpolateHorizontal)
iterateAndInterpolateVertical :: Int -> [[Integer]] -> [[Integer]]
iterateAndInterpolateVertical n l = transpose $ iterateAndInterpolateHorizontal n $ transpose l


--runImageInterpolation :: Int -> [String]
runImageInterpolation n imageWidth colors = do
  let upscaleRedChannel   = iterateAndInterpolateVertical n $ iterateAndInterpolateHorizontal n $ splitByN imageWidth $ takeChannel 'r' colors
  let upscaleGreenChannel = iterateAndInterpolateVertical n $ iterateAndInterpolateHorizontal n $ splitByN imageWidth $ takeChannel 'g' colors
  let upscaleBlueChannel  = iterateAndInterpolateVertical n $ iterateAndInterpolateHorizontal n $ splitByN imageWidth $ takeChannel 'b' colors
  -- Don't use 'return' to prevent the result being wrapped with monad which causes an error for bilinearInterpolateNtimes
  groupChannel upscaleRedChannel upscaleGreenChannel upscaleBlueChannel

getSize :: Int -> Int -> Int
getSize times size = ((size-1)*times)+size

-- entrypoint of the program
main :: IO ()
main = do
  putStr "\n[i] Input file name         : "
  inputFileName  <- getLine
  putStr "[i] Output file name        : "
  outputFileName <- getLine
  putStr "[i] Number of interpolation : "
  scaleSize      <- getLine
  let n              = read (scaleSize) :: Int

  fileContent       <- readFile inputFileName
  let content        = lines fileContent
  let ppmHeader      = words $ head content
  let imageWidth     = read (ppmHeader!!1) :: Int
  let imageHeight    = read (ppmHeader!!2) :: Int
  let ppmColour      = tail content
  let pixels         = convertEachPixel ppmColour

  printf "\n[*] Interpolating image into %dx%d image....\n" (getSize n imageWidth) (getSize n imageHeight)

  let upscaleRGB     = runImageInterpolation n imageWidth pixels
  let result         = unlines (map (\rgb -> unwords $ map (show) rgb) upscaleRGB)

  writeFile outputFileName ("P3 " ++ show (getSize n imageWidth) ++ " " ++ show (getSize n imageHeight)  ++ " 255\n" ++ result)
  putStrLn "[+] Interpolating image has done"
  return ()

