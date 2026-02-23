module Tui.Markdown
  ( renderMarkdown
  ) where

import Brick
import Data.Text (Text)
import qualified Data.Text as T

import Tui.Types (Name)

-- | Render text as markdown with basic formatting
renderMarkdown :: Text -> Widget Name
renderMarkdown content = vBox $ map renderLine $ T.lines $ unescapeNewlines content

-- | Convert literal \n sequences to actual newlines
unescapeNewlines :: Text -> Text
unescapeNewlines = T.replace "\\n" "\n"

renderLine :: Text -> Widget Name
renderLine line
  -- Headers
  | "### " `T.isPrefixOf` line =
      withAttr (attrName "mdHeader") $ txt $ T.drop 4 line
  | "## " `T.isPrefixOf` line =
      withAttr (attrName "mdHeader") $ txt $ T.drop 3 line
  | "# " `T.isPrefixOf` line =
      withAttr (attrName "mdHeader") $ txt $ T.drop 2 line
  -- Bullet points
  | "- " `T.isPrefixOf` line =
      hBox [ withAttr (attrName "mdBullet") $ txt "* "
           , renderInline $ T.drop 2 line
           ]
  | "* " `T.isPrefixOf` line =
      hBox [ withAttr (attrName "mdBullet") $ txt "* "
           , renderInline $ T.drop 2 line
           ]
  -- Normal lines
  | otherwise = renderInline line

-- | Render inline formatting (bold, code)
renderInline :: Text -> Widget Name
renderInline txt'
  | T.null txt' = emptyWidget
  | otherwise = hBox $ parseInline txt'

parseInline :: Text -> [Widget Name]
parseInline t
  | T.null t = []
  | Just rest <- T.stripPrefix "**" t =
      case T.breakOn "**" rest of
        (bold, after) | not (T.null after) ->
          withAttr (attrName "mdBold") (txt bold) : parseInline (T.drop 2 after)
        _ -> txt "**" : parseInline rest
  | Just rest <- T.stripPrefix "`" t =
      case T.breakOn "`" rest of
        (code, after) | not (T.null after) ->
          withAttr (attrName "mdCode") (txt code) : parseInline (T.drop 1 after)
        _ -> txt "`" : parseInline rest
  | otherwise =
      let (plain, rest) = T.break (\c -> c == '*' || c == '`') t
      in if T.null plain
         then txt (T.take 1 t) : parseInline (T.drop 1 t)
         else txtWrap plain : parseInline rest
