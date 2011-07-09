import Lex
import LineTypes
import Paragraphs
import GenerateHTML

main = interact $ html . paragraphs . typedLines . tokens
