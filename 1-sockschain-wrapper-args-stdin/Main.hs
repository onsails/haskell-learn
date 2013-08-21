import System.Cmd
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as Char8
import System.IO
import System.Exit
import System.Environment
import Data.ByteString.UTF8
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString          as S (length, take, drop)
import Data.List

data SocksLine = SocksLine {
  getHost :: S.ByteString,
  getPort :: S.ByteString
} deriving (Ord, Show, Eq)

colon :: Parser Char
colon = satisfy (== ':')

plainValue :: Parser S.ByteString
plainValue = takeTill (== ':')

socksLine = do
  host <- plainValue
  colon
  port <- plainValue
  return $ SocksLine host port

showSocksLine (SocksLine host port) =
  (toString host) ++ " " ++ (toString port)

main = do
  putStr "[#] Socks: "
  hFlush stdout
  
  socks <- getLine
  
  case parseOnly socksLine (Char8.pack socks) of
    Left err -> do
      hPutStrLn stderr "[!] Format error, example: 127.0.0.1:5000"
      exitFailure
    Right result -> do
      
      
      let config = "strict_chain\n\
      \proxy_dns \n\
      \n\
      \tcp_read_time_out 15000\n\
      \tcp_connect_time_out 8000\n\
      \[ProxyList]\n\
      \socks5   " ++ (showSocksLine result) ++ "\n"
      
      writeFile "/etc/proxychains.conf" config
      
      args <- getArgs
      
      system ("proxychains " ++ (intercalate " " args))
      