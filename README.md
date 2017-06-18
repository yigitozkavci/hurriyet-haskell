# hurriyet-haskell
Haskell bindings for Hurriyet API - http://developers.hurriyet.com.tr/

# Getting Started
This section provides just enough material to get you started. For a more sophisticated
illustration of the library, visit https://hackage.haskell.org/package/hurriyet for documentation.

To get started, do the following steps:
- Install the package `$ cabal install hurriyet`
- Go and get your api key from http://developers.hurriyet.com.tr/ while the package is being installed
- And start using it! See below for examples

# Examples
## Get the list of articles
```haskell
import Hurriyet
import Hurriyet.Services

client :: Client
client = getClient "<API KEY>"

main = do
  response <- withClient client getArticles
  case response of
    Left  err      -> print err
    Right articles -> print articles
```

## Get the metadata of the first file of the first article
```haskell
import Hurriyet
import Hurriyet.Services
import Hurriyet.Services.Article
import Hurriyet.Services.File

client :: Client
client = getClient "<API KEY>"

main = do
  response <- withClient client getArticles
  case response of
    Left  err         -> print err
    Right (article:_) -> print . metadata . head $ files article
```
Note that in order to use any accessor method for any of the services, you should import the relative service. See [here](https://hackage.haskell.org/package/hurriyet) for more detailed information regarding services.


# Contributing
This library is currently maintained by me, and appreciate any contributions even if they just update the docs.
There are beginner-friendly issues in [here](https://github.com/yigitozkavci/hurriyet-haskell/issues), and I encourage you to see if you can contribute in any way.

# LICENSE
[MIT License](LICENSE)
