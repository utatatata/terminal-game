module TerminalGame.Game where

import Prelude
import Ansi.Codes (Color(..), RenderingMode)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.RWS.Trans (RWST, RWSResult(..), runRWST)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.State (get, modify_)
import Control.Monad.Trans.Class (lift)
import Data.FoldableWithIndex (forWithIndex_)
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.String.CodeUnits as SCU
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (IntervalId, clearInterval, setInterval)
import TerminalGame.Terminal (initTerminal, resetTerminal, safeWrite)
import TerminalGame.Types (Canvas, FPS, KeyState, Vector3, Window, vec3)
import TerminalGame.Queue (Queue, dequeue, toList)


{---------- Classes ----------}

class (MonadState s m, MonadEffect m) <= MonadGame s m | m -> s where
  setFg :: Color -> m Unit
  setBg :: Color -> m Unit
  setMode :: RenderingMode -> m Unit
  dot :: Char -> m Unit
  clear :: m Unit
  flush :: m Unit
  moveV :: Vector3 -> m Unit
  moveToV :: Vector3 -> m Unit
  local :: m Unit -> m Unit
  quit :: Effect Unit -> m Unit
  popPressedKey :: m (Maybe KeyState)
  popPressedKeys :: m (List KeyState)

hline :: forall s m. MonadGame s m => String -> m Unit
hline s = local $ forWithIndex_ (SCU.toCharArray s) \x c -> do
  moveTo x 0 0
  dot c

vline :: forall s m. MonadGame s m => String -> m Unit
vline s = forWithIndex_ (SCU.toCharArray s) \y c -> local do
  moveTo 0 y 0
  dot c

move :: forall s m. MonadGame s m => Int -> Int -> Int -> m Unit
move x y z = moveV $ vec3 x y z

moveTo :: forall s m. MonadGame s m => Int -> Int -> Int -> m Unit
moveTo x y z = moveToV $ vec3 x y z


{---------- Definitions ----------}

newtype Game s a
  = Game (Free (Command s) a)

data Command s a
  = Fg Color a
  | Bg Color a
  | Mode RenderingMode a
  | Dot Char a
  | Clear a
  | Flush a
  | Move Vector3 a
  | MoveTo Vector3 a
  | Local (Game s a)
  | PopPressedKey (Maybe KeyState -> a)
  | PopPressedKeys (List KeyState -> a)
  | Quit (Effect a)
  | LiftEffect (Effect a)
  | State (s -> (Tuple a s))

derive instance newtypeGame :: Newtype (Game s a) _

derive newtype instance functorGame :: Functor (Game s)

derive newtype instance applyGame :: Apply (Game s)

derive newtype instance applicativeGame :: Applicative (Game s)

derive newtype instance bindGame :: Bind (Game s)

derive newtype instance monadGame :: Monad (Game s)

derive newtype instance monadRecGame :: MonadRec (Game s)

instance monadEffectGame :: MonadEffect (Game s) where
  liftEffect m = wrap $ liftF $ LiftEffect m

instance monadStateGame :: MonadState s (Game s) where
  state m = wrap $ liftF $ State m

instance monadGameGame :: MonadGame s (Game s) where
  setFg c = wrap $ liftF $ Fg c unit
  setBg c = wrap $ liftF $ Bg c unit
  setMode mode = wrap $ liftF $ Mode mode unit
  dot c = wrap $ liftF $ Dot c unit
  clear = wrap $ liftF $ Clear unit
  flush = wrap $ liftF $ Flush unit
  moveV v = wrap $ liftF $ Move v unit
  moveToV v = wrap $ liftF $ MoveTo v unit
  local m = wrap $ liftF $ Local m
  quit cb = wrap $ liftF $ Quit cb
  popPressedKey = wrap $ liftF $ PopPressedKey identity
  popPressedKeys = wrap $ liftF $ PopPressedKeys identity


{---------- Runners ----------}

runGame :: forall s. { window :: Window, fps :: FPS } -> s -> Game s Unit -> Effect Unit
runGame { window, fps } s m = do
  canvas <- Ref.new M.empty
  keys <- Ref.new mempty
  id <- Ref.new Nothing
  userState <- Ref.new s
  initTerminal keys
  id' <- setInterval (1000 / fps) $ void $ runGameOnce (initParams canvas keys id userState) m
  Ref.modify_ (const $ Just id') id
  pure unit
  where
  initParams :: Ref Canvas -> Ref (Queue KeyState) -> Ref (Maybe IntervalId) -> Ref s -> GameParams s
  initParams canvas pressedKeys intervalId userState =
    { window
    , globalPos : vec3 0 0 0
    , fg: White
    , bg: Black
    , mode: Nothing
    , canvas
    , pressedKeys
    , intervalId
    , userState
    }


{---------- Internal implementation ----------}

type GameParams s =
  { window :: Window
  , globalPos :: Vector3
  , fg :: Color
  , bg :: Color
  , mode :: Maybe RenderingMode
  , canvas :: Ref Canvas
  , pressedKeys :: Ref (Queue KeyState)
  , intervalId :: Ref (Maybe IntervalId)
  , userState :: Ref s
  }

runGameOnce :: forall s. GameParams s -> Game s ~> Effect
runGameOnce { window, globalPos, fg, bg, mode, canvas, pressedKeys, intervalId, userState } m = do
  RWSResult _ a _ <- runRWST (foldFree runCommand (unwrap m)) initR initS
  pure a
  where
  initR = GameReader
    { window
    , globalPos
    , canvas
    , pressedKeys
    , intervalId
    , userState
    }
  initS = GameState
    { pos: { x: 0, y: 0, z: 0 }
    , fg
    , bg
    , mode
    }

newtype GameReader s
  = GameReader
    { window :: Window
    , globalPos :: Vector3
    , canvas :: Ref Canvas
    , pressedKeys :: Ref (Queue KeyState)
    , intervalId :: Ref (Maybe IntervalId)
    , userState :: Ref s
    }

derive instance newtypeGameReader :: Newtype (GameReader s) _

newtype GameState
  = GameState
    { pos :: Vector3
    , fg :: Color
    , bg :: Color
    , mode :: Maybe RenderingMode
    }

derive instance newtypeGameState :: Newtype GameState _

runCommand :: forall s. Command s ~> RWST (GameReader s) Unit GameState Effect
runCommand (Fg color a) = do
  modify_ $ over GameState \s -> s { fg = color }
  pure a
runCommand (Bg color a) = do
  modify_ $ over GameState \s -> s { bg = color }
  pure a
runCommand (Mode mode a) = do
  modify_ $ over GameState \s -> s { mode = Just mode }
  pure a
runCommand (Dot char a) = do
  GameState { pos, fg, bg, mode } <- get
  GameReader { globalPos, canvas } <- ask
  lift $ Ref.modify_ (M.insert (pos + globalPos) { fg, bg, mode, char }) canvas
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
  GameReader { window, globalPos, canvas, pressedKeys, intervalId, userState } <- ask
  GameState { pos, fg, bg, mode } <- get
  lift $ runGameOnce { window, globalPos: pos + globalPos, fg, bg, mode, canvas, pressedKeys, intervalId, userState } m
runCommand (PopPressedKey a) = do
  GameReader { pressedKeys } <- ask
  key <- lift $ Ref.read pressedKeys >>= dequeue >>> traverse \{ last, rest } -> Ref.modify_ (const rest) pressedKeys $> last
  a <$> pure key
runCommand (PopPressedKeys a) = do
  GameReader { pressedKeys } <- ask
  -- a <$> lift (Ref.modify' (\keys -> { state: Nil, value: keys }) pressedKeys)
  a <$> lift (Ref.modify' (\keys -> { state: mempty, value: toList keys }) pressedKeys)
runCommand (Quit cb) = do
  GameReader { intervalId } <- ask
  lift $ Ref.read intervalId >>= maybe (pure unit) clearInterval
  a <- lift cb
  lift resetTerminal
  pure a
runCommand (LiftEffect m) = lift m
runCommand (State f) = do
  GameReader { userState } <- ask
  Tuple a s <-  f <$> lift (Ref.read userState)
  lift $ Ref.modify_ (const s) userState
  pure a
