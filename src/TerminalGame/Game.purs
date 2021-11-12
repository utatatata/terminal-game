module TerminalGame.Game where

import Prelude
import Ansi.Codes (Color(..))
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.RWS.Trans (RWST, RWSResult(..), runRWST)
import Control.Monad.State (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List(..), uncons)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (IntervalId, clearInterval, setInterval)
import TerminalGame.Terminal (initTerminal, resetTerminal, safeWrite)
import TerminalGame.Types (Canvas, FPS, KeyState, Vector3, Window, vec3)


{---------- Definitions ----------}

newtype Game a
  = Game (Free Command a)

data Command a
  = Fg Color a
  | Bg Color a
  | Dot Char a
  | Clear a
  | Flush a
  | Move Vector3 a
  | MoveTo Vector3 a
  | Local (Game a)
  | PopPressedKey (Maybe KeyState -> a)
  | PopPressedKeys (List KeyState -> a)
  | Quit (Effect a)
  | LiftEffect (Effect a)

derive instance newtypeGame :: Newtype (Game a) _

derive newtype instance functorGame :: Functor Game

derive newtype instance applyGame :: Apply Game

derive newtype instance applicativeGame :: Applicative Game

derive newtype instance bindGame :: Bind Game

derive newtype instance monadGame :: Monad Game

derive newtype instance monadRecGame :: MonadRec Game

instance monadEffectGame :: MonadEffect Game where
  liftEffect m = wrap $ liftF $ LiftEffect m


{---------- Commands ----------}

setFg :: Color -> Game Unit
setFg c = wrap $ liftF $ Fg c unit

setBg :: Color -> Game Unit
setBg c = wrap $ liftF $ Bg c unit

dot :: Char -> Game Unit
dot c = wrap $ liftF $ Dot c unit

line :: String -> Game Unit
line s = local $ forWithIndex_ (SCU.toCharArray s) \x c -> do
  moveTo x 0 0
  dot c

clear :: Game Unit
clear = wrap $ liftF $ Clear unit

flush :: Game Unit
flush = wrap $ liftF $ Flush unit

moveV :: Vector3 -> Game Unit
moveV v = wrap $ liftF $ Move v unit

move :: Int -> Int -> Int -> Game Unit
move x y z = moveV $ vec3 x y z

moveToV :: Vector3 -> Game Unit
moveToV v = wrap $ liftF $ MoveTo v unit

moveTo :: Int -> Int -> Int -> Game Unit
moveTo x y z = moveToV $ vec3 x y z

local :: Game Unit -> Game Unit
local m = wrap $ liftF $ Local m

quit :: Effect Unit -> Game Unit
quit cb = wrap $ liftF $ Quit cb

popPressedKey :: Game (Maybe KeyState)
popPressedKey = wrap $ liftF $ PopPressedKey identity

popPressedKeys :: Game (List KeyState)
popPressedKeys = wrap $ liftF $ PopPressedKeys identity


{---------- Runners ----------}

runGame :: { window :: Window, fps :: FPS } -> Game Unit -> Effect Unit
runGame { window, fps } m = do
  canvas <- Ref.new M.empty
  keys <- Ref.new Nil
  id <- Ref.new Nothing
  initTerminal keys
  id' <- setInterval (1000 / fps) $ void $ runGameOnce (initParams canvas keys id) m
  Ref.modify_ (const $ Just id') id
  pure unit
  where
  initParams :: Ref Canvas -> Ref (List KeyState) -> Ref (Maybe IntervalId) -> GameParams
  initParams canvas pressedKeys intervalId =
    { window
    , globalPos : vec3 0 0 0
    , fg: White
    , bg: Black
    , canvas
    , pressedKeys
    , intervalId
    }


{---------- Internal implementation ----------}

type GameParams =
  { window :: Window
  , globalPos :: Vector3
  , fg :: Color
  , bg :: Color
  , canvas :: Ref Canvas
  , pressedKeys :: Ref (List KeyState)
  , intervalId :: Ref (Maybe IntervalId)
  }

runGameOnce :: GameParams -> Game ~> Effect
runGameOnce { window, globalPos, fg, bg, canvas, pressedKeys, intervalId } m = do
  RWSResult _ a _ <- runRWST (foldFree runCommand (unwrap m)) initR initS
  pure a
  where
  initR = GameReader
    { window
    , globalPos
    , canvas
    , pressedKeys
    , intervalId
    }
  initS = GameState
    { pos: { x: 0, y: 0, z: 0 }
    , fg
    , bg
    }

newtype GameReader
  = GameReader
    { window :: Window
    , globalPos :: Vector3
    , canvas :: Ref Canvas
    , pressedKeys :: Ref (List KeyState)
    , intervalId :: Ref (Maybe IntervalId)
    }

derive instance newtypeGameReader :: Newtype GameReader _

newtype GameState
  = GameState
    { fg :: Color
    , bg :: Color
    , pos :: Vector3
    }

derive instance newtypeGameState :: Newtype GameState _

runCommand :: Command ~> RWST GameReader Unit GameState Effect
runCommand (Fg c a) = do
  modify_ $ over GameState \s -> s { fg = c }
  pure a
runCommand (Bg c a) = do
  modify_ $ over GameState \s -> s { bg = c }
  pure a
runCommand (Dot char a) = do
  GameState { pos, fg, bg } <- get
  GameReader { globalPos, canvas } <- ask
  lift $ Ref.modify_ (M.insert (pos + globalPos) { fg, bg, char }) canvas
  pure a
runCommand (Clear a) = do
  GameReader { canvas } <- ask
  lift $ Ref.modify_ (const M.empty)  canvas
  pure a
runCommand (Flush a) = do
  GameReader { window, canvas } <- ask
  lift do
    safeWrite window =<< Ref.read canvas
    Ref.modify_ (const M.empty) canvas
  pure a
runCommand (Move v a) = do
  GameState { pos } <- get
  modify_ $ over GameState \s -> s { pos = pos + v }
  pure a
runCommand (MoveTo v a) = do
  modify_ $ over GameState \s -> s { pos = v }
  pure a
runCommand (Local m) = do
  GameReader { window, canvas, pressedKeys, intervalId } <- ask
  GameState { pos, fg, bg } <- get
  lift $ runGameOnce { window, globalPos: pos, fg, bg, canvas, pressedKeys, intervalId } m
runCommand (PopPressedKey a) = do
  GameReader { pressedKeys } <- ask
  key <- lift $ Ref.read pressedKeys >>= uncons >>> traverse (\{ head, tail } -> Ref.modify_ (const tail) pressedKeys $> head)
  a <$> pure key
runCommand (PopPressedKeys a) = do
  GameReader { pressedKeys } <- ask
  a <$> lift (Ref.modify' (\keys -> { state: Nil, value: keys }) pressedKeys)
runCommand (Quit cb) = do
  GameReader { intervalId } <- ask
  lift $ Ref.read intervalId >>= maybe (pure unit) clearInterval
  a <- lift cb
  lift resetTerminal
  pure a
runCommand (LiftEffect a) = lift a
