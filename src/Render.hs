module Render 
    ( toJs
    , toHtmlAndJs
    )
    where 


import qualified Runner as R


toJs :: R.Layout -> Either String String
toJs _ = 
    return "const layout = () => { \
\                   \
\   }"


toHtmlAndJs :: R.Layout -> Either String String
toHtmlAndJs _ = 
    return ""
