{-# LANGUAGE LambdaCase #-}
import Control.Monad      ( forM_ )
import Control.Concurrent ( threadDelay )
import Data.Word          ( Word8, Word16 )
import Data.Bits          ( Bits, shiftL, shiftR, (.|.), (.&.) )
import Narco              ( writeBulkByte, readBulkByte )

type Addr = Int
type Imm = Word16

data Instr
  = Add   Addr Addr Addr
  | Sub   Addr Addr Addr
  | Mul   Addr Addr Addr
  | ImmH  Addr Imm
  | ImmL  Addr Imm
  | Bne   Addr Addr Addr
  | Mov   Addr Addr
  | Get   Addr
  | Put   Addr
  | Load  Addr Addr -- r2 = mem addr
  | Store Addr Addr -- r1 = value, r2 = mem addr
  | Nop

progMult :: [Instr]
progMult
  =  [ ImmL 0 5
     , ImmL 1 5
     , Mul  0 1 2
     , Get 2
     ]

progAdd :: [Instr]
progAdd
  = [ ImmL 0 4
    , ImmL 1 4
    , Add 0 1 2
    , Get 2
    ]

main :: IO ()
main = do
  writeBulkByte ([0x00, 0x00] ++ encodeWord16 (fromIntegral $ length progMult)) -- write prog size
  threadDelay 1000
  forM_ progMult $ \instr -> do -- write prog instructions
    writeBulkByte (encodeInstr instr)
    threadDelay 1000
  threadDelay 1000
  readBulkByte 4 -- read prog result

packBits:: (Num b, Foldable t, Bits b) => t b -> b
packBits = foldl go 0
  where go acc i = (acc `shiftL` 1) .|. i

{- OPCODE :: BitVector 4
Add   => 0
Sub   => 1
Mul   => 2
ImmH  => 3
ImmL  => 4
Bne   => 5
Mov   => 6
Get   => 7
Put   => 8
Load  => 9
Store => 10
Nop   => 11-15
-}

--   31    27    23    19    15               0
-- 0b[****][****][****][****][****************]
--  opcode|addr1|addr2|addr3|immediate

encodeInstr :: Instr -> [Word8]
encodeInstr = \case
  Add a b c -> encodeWord16 (packBits ([0,0,0,0] ++ parseAddr a ++ parseAddr b ++ parseAddr c)) ++ [0x00, 0x00]
  Sub a b c -> encodeWord16 (packBits ([0,0,0,1] ++ parseAddr a ++ parseAddr b ++ parseAddr c)) ++ [0x00, 0x00]
  Mul a b c -> encodeWord16 (packBits ([0,0,1,0] ++ parseAddr a ++ parseAddr b ++ parseAddr c)) ++ [0x00, 0x00]
  ImmH a i  -> encodeWord16 (packBits ([0,0,1,1] ++ parseAddr a ++ replicate 8 0)) ++ parseImm i
  ImmL a i  -> encodeWord16 (packBits ([0,1,0,0] ++ parseAddr a ++ replicate 8 0)) ++ parseImm i
  Bne a b c -> encodeWord16 (packBits ([0,1,0,1] ++ parseAddr a ++ parseAddr b ++ parseAddr c)) ++ [0x00, 0x00]
  Mov a b   -> encodeWord16 (packBits ([0,1,1,0] ++ parseAddr a ++ parseAddr b ++ replicate 4 0)) ++ [0x00, 0x00]
  Get a     -> encodeWord16 (packBits ([0,1,1,1] ++ parseAddr a ++ replicate 8 0)) ++ [0x00, 0x00]
  Put a     -> encodeWord16 (packBits ([1,0,0,0] ++ parseAddr a ++ replicate 8 0)) ++ [0x00, 0x00]
  Load a b  -> encodeWord16 (packBits ([1,0,0,1] ++ parseAddr a ++ parseAddr b ++ replicate 4 0)) ++ [0x00, 0x00]
  Store a b -> encodeWord16 (packBits ([1,0,1,0] ++ parseAddr a ++ parseAddr b ++ replicate 4 0)) ++ [0x00, 0x00]
  Nop       -> encodeWord16 (packBits ([1,1,1,1] ++ replicate 12 0)) ++ [0x00, 0x00]

parseAddr :: Num b => Bits b => Int -> [b]
parseAddr = \case
  0  -> [0,0,0,0]
  1  -> [0,0,0,1]
  2  -> [0,0,1,0]
  3  -> [0,0,1,1]
  4  -> [0,1,0,0]
  5  -> [0,1,0,1]
  6  -> [0,1,1,0]
  7  -> [0,1,1,1]
  8  -> [1,0,0,0]
  9  -> [1,0,0,1]
  10 -> [1,0,1,0]
  11 -> [1,0,1,1]
  12 -> [1,1,0,0]
  13 -> [1,1,0,1]
  14 -> [1,1,1,0]
  _  -> [1,1,1,1]

encodeWord16 :: Word16 -> [Word8]
encodeWord16 x = map fromIntegral [(x .&. 0xFF00) `shiftR` 8, x .&. 0xFF]

parseImm :: Word16 -> [Word8]
parseImm = encodeWord16
