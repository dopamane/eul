{-# LANGUAGE LambdaCase #-}
import Control.Monad      ( forM_ )
import Control.Concurrent ( threadDelay )
import Data.Word          ( Word8, Word16 )
import Data.Bits          ( Bits, shiftL, shiftR, (.|.), (.&.) )
import Narco              ( writeBulkByte, readBulkByte )

type Addr = Int
type Imm = Word16

data Instr
  = Add Addr Addr Addr
  | Sub Addr Addr Addr
  | Mul Addr Addr Addr
  | PutH Addr Imm
  | PutL Addr Imm
  | Get Addr

prog :: [Instr]
prog
  =  [ PutL 0 5
     , PutL 1 5
     , Mul  0 1 2
     , Get 2
     ]

main :: IO ()
main = forM_ prog $ \instr -> do
  writeBulkByte (parseInstr instr)
  threadDelay 100
  case instr of
    Get _ -> readBulkByte 4
    _     -> return ()
  threadDelay 100

packBits:: (Num b, Foldable t, Bits b) => t b -> b
packBits = foldl go 0
  where go acc i = (acc `shiftL` 1) .|. i

parseInstr :: Instr -> [Word8]
parseInstr = \case
  Add a b c -> encodeWord16 (packBits ([0,0,0] ++ parseAddr a ++ parseAddr b ++ parseAddr c ++ [0,0,0,0])) ++ [0x00, 0x00]
  Sub a b c -> encodeWord16 (packBits ([0,0,1] ++ parseAddr a ++ parseAddr b ++ parseAddr c ++ [0,0,0,0])) ++ [0x00, 0x00]
  Mul a b c -> encodeWord16 (packBits ([0,1,0] ++ parseAddr a ++ parseAddr b ++ parseAddr c ++ [0,0,0,0])) ++ [0x00, 0x00]
  PutH a i  -> encodeWord16 (packBits ([0,1,1] ++ parseAddr a ++ replicate 10 0)) ++ parseImm i
  PutL a i  -> encodeWord16 (packBits ([1,0,0] ++ parseAddr a ++ replicate 10 0)) ++ parseImm i
  Get a     -> encodeWord16 (packBits ([1,0,1] ++ parseAddr a ++ replicate 10 0)) ++ [0x00, 0x00]

parseAddr :: Num b => Bits b => Int -> [b]
parseAddr = \case
  0 -> [0,0,0]
  1 -> [0,0,1]
  2 -> [0,1,0]
  3 -> [0,1,1]
  4 -> [1,0,0]
  5 -> [1,0,1]
  6 -> [1,1,0]
  7 -> [1,1,1]
  _ -> [0,0,0]

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [(x .&. 0xFF00) `shiftR` 8, x .&. 0xFF]

parseImm :: Word16 -> [Word8]
parseImm = encodeWord16
