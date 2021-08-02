module UTF8 where

import qualified Data.Bits
import qualified Data.Char as Char
import Data.Word (Word8)

utf8Encode' :: Char -> (Word8, [Word8])
utf8Encode' c = case go (Char.ord c) of
  (x, xs) -> (fromIntegral x, map fromIntegral xs)
  where
    go oc
      | oc <= 0x7f =
        ( oc,
          []
        )
      | oc <= 0x7ff =
        ( 0xc0 + (oc `Data.Bits.shiftR` 6),
          [ 0x80 + oc Data.Bits..&. 0x3f
          ]
        )
      | oc <= 0xffff =
        ( 0xe0 + (oc `Data.Bits.shiftR` 12),
          [ 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f),
            0x80 + oc Data.Bits..&. 0x3f
          ]
        )
      | otherwise =
        ( 0xf0 + (oc `Data.Bits.shiftR` 18),
          [ 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f),
            0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f),
            0x80 + oc Data.Bits..&. 0x3f
          ]
        )