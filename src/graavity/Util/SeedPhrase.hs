module graavity.Util.SeedPhrase where

import           Control.Monad (replicateM)
import           Crypto.Error (CryptoFailable(..))
import qualified Crypto.Hash as Crypto
import           Crypto.PubKey.Ed25519 (SecretKey, PublicKey, Signature, secretKey, toPublic, sign, verify)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.ByteString.Base64 ()
import qualified Data.ByteString.Base16 as B16
import           Data.ByteArray (convert)
import           Data.Bits (shiftR)
import           Data.Char (digitToInt, intToDigit)
import           Data.List (foldl', unfoldr)
import           System.Random (randomRIO)
import           Numeric (showIntAtBase)
import           Crypto.KDF.PBKDF2 (Parameters(..), generate, prfHMAC)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8,decodeUtf8)

type SeedPhrase = [String]

-- Load the BIP-39 wordlist from a file
loadWordlist :: FilePath -> IO [String]
loadWordlist path = do
  content <- readFile path
  return $ lines content

-- Generate a random seed phrase of 12 words
generateSeedPhrase :: IO SeedPhrase
generateSeedPhrase = do
  wordlist <- loadWordlist "conf/bip39.txt"
  -- Generate 128 bits of entropy (16 bytes)
  entropy <- replicateM 16 (randomRIO (0, 255) :: IO Int)
  let entropyBytes = BS.pack $ map (toEnum . fromEnum) entropy

  -- Hash the entropy with SHA-256 to get the checksum
  let hash = Crypto.hashWith Crypto.SHA256 entropyBytes
      hashBytes = convert hash  -- Convert digest to ByteString
      checksumBits = take 1 (BS.unpack hashBytes)  -- 4 bits for 128 bits of entropy
      checksum = fromEnum (head checksumBits) `shiftR` 4  -- Extract the first 4 bits

  -- Convert entropy + checksum to a binary string
  let binaryString = concatMap (padLeft 8 '0' . flip showBinary 2) entropy
      binaryWithChecksum = binaryString ++ padLeft 4 '0' (showBinary checksum 2)

  -- Split the binary string into 11-bit chunks and map to words
  let wordIndices = unfoldr (\b -> if null b then Nothing else Just $ splitAt 11 b) binaryWithChecksum
      wordIndicesInt = map binaryToInt wordIndices
  return $ map (wordlist !!) wordIndicesInt

-- Derive a private key from a seed phrase using PBKDF2
derivePrivateKey :: SeedPhrase -> IO SecretKey
derivePrivateKey seedPhrase = do
  let seedText = T.intercalate (T.pack " ") (map T.pack seedPhrase)
      seedBytes = encodeUtf8 seedText
      salt = "mnemonic"  -- Standard salt for BIP-39
      params = Parameters { iterCounts = 2048, outputLength = 32 }
      derivedKey = generate (prfHMAC Crypto.SHA256) params seedBytes (encodeUtf8 $ T.pack salt)
  case secretKey (derivedKey :: ByteString) of
    CryptoPassed sk -> return sk
    CryptoFailed _ -> error "Failed to derive private key"

-- Helper functions
padLeft :: Int -> a -> [a] -> [a]
padLeft n c xs = replicate (n - length xs) c ++ xs

showBinary :: Int -> Int -> String
showBinary x n = padLeft n '0' $ showIntAtBase 2 intToDigit x ""

binaryToInt :: String -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

generateKeys :: IO (SeedPhrase, SecretKey, PublicKey)
generateKeys = do
  sp <- generateSeedPhrase
  privKey <- derivePrivateKey sp
  return (sp, privKey, toPublic privKey)

generateKeys' :: IO (SeedPhrase, T.Text, T.Text)
generateKeys' = do
  sp <- generateSeedPhrase
  privKey <- derivePrivateKey sp
  let pubKey = toPublicKey privKey
      pub = decodeUtf8 $ B16.encode $ convert pubKey
      pri = decodeUtf8 $ B16.encode $ convert privKey
  return (sp, pri, pub)

hashing :: String -> ByteString
hashing msg = (convert . Crypto.hashWith Crypto.SHA3_256) (BS.pack msg)

signing :: SecretKey -> PublicKey -> ByteString -> Signature
signing = sign

toPublicKey :: SecretKey -> PublicKey
toPublicKey = toPublic

verifySignature :: PublicKey -> ByteString -> Signature -> Bool
verifySignature = verify