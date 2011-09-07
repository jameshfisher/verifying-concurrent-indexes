import Lex
import LineTypes
import Snip
import Paragraphs
import GenerateHTML

main = interact $ html . paragraphs . snip . typedLines . tokens
