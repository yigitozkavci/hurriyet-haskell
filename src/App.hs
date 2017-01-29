import Hurriyet

main = do
  response <- getArticle "40349649"
  case response of
    Left err -> print err
    Right article -> print article
