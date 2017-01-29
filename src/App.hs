import Hurriyet
import Hurriyet.Services
import Hurriyet.Services.Article

main = do
  response <- getArticle "40349649"
  case response of
    Left err -> print err
    Right article -> print $ description article
