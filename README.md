# hurriyet-haskell
Haskell bindings for Hurriyet API - http://developers.hurriyet.com.tr/

# Getting Started
```
import Hurriyet
import Hurriyet.Services
import Hurriyet.Services.Article
import Hurriyet.Services.File

client :: Client
client = getClient "<API KEY>"

main = do
  response <- getArticles client
  case response of
    Left err -> print err
    Right (article:_) -> print . metadata . head $ files article
```
