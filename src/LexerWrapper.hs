module LexerWrapper where

import Control.Arrow (second)
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Function ((&))
import Data.Maybe (fromJust)
import Data.Span
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TLB
import Data.Word (Word8)
import GHC.Records
import Syntax.Token (Token (..))
import Syntax.Token.Kind (TokenKind)
import qualified Syntax.Token.Kind as TK
import UTF8 (utf8Encode')

data AlexInput = AlexInput
  { pos :: !SourcePos,
    prev :: !Char,
    bytes :: [Word8],
    text :: T.Text
  }
  deriving (Show, Eq)

alexStartPos :: SourcePos
alexStartPos = SourcePos {line = 1, col = 1}

alexMove :: SourcePos -> Char -> SourcePos
alexMove SourcePos {line} '\n' = SourcePos {line = line + 1, col = 1}
alexMove SourcePos {line, col} _ = SourcePos {line, col = col + 1}

data ErrorKind
  = UnknownToken Text
  | InvalidEscape
  deriving (Show, Eq)

-- data Error
--   = Error ErrorKind SourcePos SourcePos
data LexError
  = LexError ErrorKind Span
  deriving (Show, Eq)

instance Spanned LexError where
  spanOf (LexError _ s) = s

data AlexState = AlexState
  { pos :: !SourcePos,
    text :: Text,
    chr :: !Char,
    bytes :: [Word8],
    scd :: !Int, -- the current startcode
    errors :: DList LexError,
    stringBuf :: TLB.Builder,
    stringFailed :: Bool
  }

newtype Alex a = Alex {unAlex :: (StateT AlexState Identity) a}
  deriving (Functor, Applicative, Monad, MonadState AlexState)

instance MonadWriter (DList LexError) Alex where
  tell w =
    modify (\st -> st {errors = errors st `DL.append` w})

data AlexEnv = AlexEnv
  { tokText :: Text,
    span :: !Span
  }

type AlexAction a = ReaderT AlexEnv Alex a

runAlex :: MonadError [LexError] m => Text -> Alex a -> m a
runAlex text alex = do
  let (tokens, lexErrors) =
        alex
          & unAlex
          & (`runStateT` defaultAlexState text)
          & runIdentity
          & second (DL.toList . errors)
  case lexErrors of
    [] -> return tokens
    _ -> throwError lexErrors

defaultAlexState :: Text -> AlexState
defaultAlexState text =
  AlexState
    { bytes = [],
      pos = alexStartPos,
      text,
      chr = '\n',
      scd = 0,
      errors = DL.empty,
      stringBuf = TLB.fromText "",
      stringFailed = False
    }

-- These two functions are needed for alex to work
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {pos, prev, bytes = b : bs, text} =
  Just (b, AlexInput {pos, prev, bytes = bs, text})
alexGetByte AlexInput {bytes = [], text} | T.null text = Nothing
alexGetByte AlexInput {pos, bytes = [], text} =
  Just (b, AlexInput {pos = pos', prev = c, bytes = bs, text = text'})
  where
    !pos' = alexMove pos c
    (b, bs) = utf8Encode' c
    (c, text') = fromJust $ T.uncons text

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar AlexInput {prev} = prev

alexGetStartCode :: Alex Int
alexGetStartCode = gets scd

alexSetStartCode :: Int -> Alex ()
alexSetStartCode scd = modify $ \st -> st {scd}

alexGetInput :: Alex AlexInput
alexGetInput = do
  AlexState {pos, text, chr, bytes} <- get
  return $ AlexInput {pos, text, prev = chr, bytes}

alexSetInput :: AlexInput -> Alex ()
alexSetInput AlexInput {pos, text, prev, bytes} =
  modify $ \st -> st {pos, text, chr = prev, bytes}

eofSpan :: Span
eofSpan = Span {col1 = -1, line1 = -1, col2 = -1, line2 = -1}

alexEOF :: Alex Token
alexEOF = return $ Token TK.EOF eofSpan

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes inp = inp {bytes = []}

-- some combinators
tk :: TokenKind -> AlexAction Token
tk kind = asks (Token kind . (getField @"span"))

string :: (Text -> TokenKind) -> AlexAction Token
string f = do
  sp <- asks $ getField @"span"
  kind <- asks $ f . tokText
  return $ Token kind sp