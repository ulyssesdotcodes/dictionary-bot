import Network.Wreq

main = do
    r <- get ("http://httpbin.com/get")
    putstrln r ^. responseBody
