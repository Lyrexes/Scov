module Draw (drawChess) where
import qualified SDL
import qualified SDL.Font as SDLF
import Linear (V2 (V2))
import Foreign.C.Types ( CInt )
import Data.Bits
import Data.Word (Word64)
import Data.Foldable (Foldable(toList))
import Data.Vector.Storable (fromList)
import Data.Word (Word8)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (pack)
import SDL.Font (PointSize)
import Types (PieceType (..), Position (..), positionToPoint,
                cIntToInt8, int8ToCInt, GameState (..), Color (..),
                pieceToUnicode, isPieceWhite, getBitboard)

type RGB = (Word8, Word8, Word8)

tileColorWhite :: RGB
tileColorWhite = (238-60, 197-60, 145-60)

tileColorBlack :: RGB
tileColorBlack = (139-20, 90-20, 43-20)

pieceColorWhite :: RGB
pieceColorWhite = (255, 255, 255)

pieceColorBlack :: RGB
pieceColorBlack = (0, 0, 0)

possibleMovesColor :: RGB
possibleMovesColor = (0,197,0)

fontPath :: String
fontPath = "rsc/NotoSansSymbols2-Regular.ttf"

drawChess :: SDL.Window -> SDL.Renderer -> CInt -> CInt -> GameState -> IO()
drawChess win render tAmount bSize (GameState pieces pMoves _)= do
    let tileSize =  div bSize tAmount
    drawTiles render tAmount bSize
    drawAllPieces render tileSize pieces
    --drawPossibleMoves render tileSize pMoves TODO: fix drawPossibleMove


drawTiles :: SDL.Renderer -> CInt -> CInt -> IO()
drawTiles renderer n size = do
    let
        tileSize = div size n
        tiles = getTiles n tileSize
        changeColor = colorChanger renderer tileColorWhite tileColorBlack
        in
            mapM_ (\(rect,(row,col)) -> do
                    changeColor (even (row+col))
                    drawTile renderer rect
                ) tiles

drawAllPieces :: SDL.Renderer -> CInt -> [Word64] -> IO()
drawAllPieces ren size bbs = mapM_ (drawPieces ren size bbs)
                                   [minBound .. maxBound]

drawPieces :: SDL.Renderer -> CInt -> [Word64] -> PieceType -> IO()
drawPieces ren size bbs pType = do
    let drawP = drawPiece ren size
        getBb = getBitboard bbs
        in
            mapM_ (drawP pType) (bitboardToPositions (getBb pType))

changeDrawColor :: SDL.Renderer -> RGB -> IO()
changeDrawColor ren (r,g,b) = SDL.rendererDrawColor ren SDL.$= SDL.V4 r g b maxBound

getTiles :: CInt -> CInt -> [(SDL.Rectangle CInt,(CInt,CInt))]
getTiles n size = concat [getRowTiles size cur n | cur <- [0..n-1]]

colorChanger :: SDL.Renderer -> RGB -> RGB -> (Bool -> IO ())
colorChanger r c1 c2 = changer
    where
        changer :: Bool -> IO ()
        changer True =  changeDrawColor r c1
        changer False = changeDrawColor r c2

drawTile :: SDL.Renderer -> SDL.Rectangle CInt -> IO()
drawTile r rect = SDL.fillRect r (Just rect)

getRowTiles :: CInt -> CInt -> CInt-> [(SDL.Rectangle CInt,(CInt,CInt))]
getRowTiles tileSize row amount =
    let
        s = tileSize
        tSize = SDL.V2 s s
        in
        [(SDL.Rectangle (SDL.P(SDL.V2 (s*col) (s*row))) tSize ,(row,col)) |
        col <- [0..amount-1]]

nth :: Int -> SDL.V2 CInt -> CInt
nth i v = toList v !! i

drawPiece :: SDL.Renderer -> CInt -> PieceType -> (CInt,CInt)-> IO ()
drawPiece ren size pType (row,col) = do
    let textureStr = pieceToUnicode pType
    let colB = rgbToSDLColor pieceColorBlack
        colW = rgbToSDLColor pieceColorWhite

    font <- SDLF.load fontPath (fromEnum size)
    surface <- if isPieceWhite pType
            then SDLF.blended font colW (pack textureStr)
            else SDLF.blended font colB (pack textureStr)

    texture <- SDL.createTextureFromSurface ren surface

    Just(_,_,_,yMax,_) <- SDLF.glyphMetrics font (head textureStr)
    textDim <- SDL.surfaceDimensions surface

    let yOff = div (toEnum (abs yMax)) 4
        yPixel = row * size
        xPixel = col * size
        pixelPos = SDL.P(SDL.V2 xPixel yPixel)
        position = pixelPos - SDL.P(SDL.V2 0 yOff)
        rect = SDL.Rectangle position textDim
            in
                SDL.copy ren texture Nothing (Just rect)

    SDL.freeSurface surface
    SDL.destroyTexture texture
    SDLF.free font

renderSurfaceToWindow :: (MonadIO m) =>
    SDL.Window -> SDL.Surface -> SDL.Surface -> SDL.Point SDL.V2 CInt -> m ()
renderSurfaceToWindow w s i startPoint = do
  SDL.surfaceBlit i Nothing s (Just startPoint)
  return ()

rgbToSDLColor :: RGB -> SDLF.Color
rgbToSDLColor (r,g,b) = SDL.V4 r g b maxBound

drawPossibleMoves :: SDL.Renderer -> CInt -> [Position] -> IO()
drawPossibleMoves render size ps = do
    changeDrawColor render possibleMovesColor
    mapM_ (drawPossibleMove render size) ps

bitboardToPositions :: Word64 -> [(CInt,CInt)]
bitboardToPositions bb = [(toEnum row,toEnum col) | row <-[0..7], col<-[0..7],
                          testBit bb (row*8+col)]



drawPossibleMove :: SDL.Renderer -> CInt -> Position -> IO()
drawPossibleMove render size (Position x y) = do
    drawTile render (SDL.Rectangle (SDL.P(SDL.V2 xPix yPix)) (SDL.V2 size size))
        where
            xPix = int8ToCInt x*size
            yPix = int8ToCInt y*size
