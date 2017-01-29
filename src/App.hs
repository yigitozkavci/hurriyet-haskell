import Hurriyet
import Hurriyet.Services
import Hurriyet.Services.Article
import Hurriyet.Services.File

client :: Client
client = getClient "9a9dcb9808624ac69e6c557a192afb33"

main = do
  response <- getArticles client
  case response of
    Left err -> print err
    Right (article:_) -> print . metadata . head $ files article
