module ID3.V1
    ( parseTag
    ) where

import qualified Data.ByteString.Lazy as L
import ParseBS

parseTag :: Parse String
parseTag = do
    fileString <- look
    let fileSize = L.length fileString
    puts $ L.drop (fileSize - 128) fileString
    string "TAG"
    show <$> look
