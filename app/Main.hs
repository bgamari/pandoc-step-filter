module Main where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Set as S
import Text.Pandoc.JSON
import Text.Pandoc.Walk

main :: IO ()
main = toJSONFilter stepFilter

stepFilter :: Pandoc -> Pandoc
stepFilter (Pandoc meta blks) =
    Pandoc meta
    $ concatSlides
    $ filterSlides
    $ collectSlides slideLvl blks
  where
    slideLvl =
        case lookupMeta (T.pack "slide-level") meta of
          Just (MetaString s) -> read $ T.unpack s
          Just (MetaInlines [Str s]) -> read $ T.unpack s
          Just x -> error $ "Invalid slide-level: " ++ show x
          Nothing -> error "No slide-level defined"

data Slide
    = Slide Block [Block]
    | Other Block

collectSlides :: Int -> [Block] -> [Slide]
collectSlides slideLvl = go
  where
    is_slide_hdr (Header lvl _ _) = lvl <= slideLvl
    is_slide_hdr _ = False

    go [] = []
    go (blk@(Header lvl _ _) : rest)
      | lvl < slideLvl = Other blk : go rest
    go (blk : rest)
      | is_slide_hdr blk =
          let (contents, rest') = break is_slide_hdr rest
          in Slide blk contents : go rest'
      | otherwise = Other blk : go rest

concatSlides :: [Slide] -> [Block]
concatSlides (Other blk : rest) =
    blk : concatSlides rest
concatSlides (Slide hdr contents : rest) =
    hdr : (contents ++ concatSlides rest)
concatSlides [] = []

filterSlides :: [Slide] -> [Slide]
filterSlides = foldMap filterSlide

filterSlide :: Slide -> [Slide]
filterSlide (Slide hdr contents)
  | steps <- slideSteps contents
  , not $ null steps
  = [ Slide hdr (stepContents step contents)
    | step <- S.toList steps
    ]
filterSlide slide = [slide]

stepContents :: Step -> [Block] -> [Block]
stepContents step = walk f
  where
    f blk
      | Just steps <- isStepDiv blk
      , not $ step `S.member` steps
      = Null
    f blk = blk

slideSteps :: [Block] -> S.Set Step
slideSteps = query $ maybe mempty id . isStepDiv

isStepDiv :: Block -> Maybe (S.Set Step)
isStepDiv (Div (_id, classes, _kvs) _contents) =
    case mapMaybe isStepClass classes of
      steps
        | null steps -> Nothing
        | otherwise -> Just (S.fromList steps)
isStepDiv _ = Nothing

newtype Step = Step Integer
    deriving (Eq, Ord, Show)

isStepClass :: T.Text -> Maybe Step
isStepClass s
  | Just s' <- T.pack "step-" `T.stripPrefix` s
  , (n, ""):_ <- reads $ T.unpack s'
  = Just (Step n)
  | otherwise
  = Nothing

