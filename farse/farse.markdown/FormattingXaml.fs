module Farse.Markdown.Formatting.Xaml

open System.IO
open System.Collections.Generic

open Farse.Markdown
open Farse.Patterns
open Farse.Collections

// --------------------------------------------------------------------------------------
// Formats Markdown documents as an HTML file
// --------------------------------------------------------------------------------------

/// Basic escaping as done by Markdown
let htmlEncode (code:string) = 
  code.Replace("&", "&amp;").Replace("<", "&lt;").Replace(">", "&gt;")

/// Basic escaping as done by Markdown including quotes
let htmlEncodeQuotes (code:string) = 
  (htmlEncode code).Replace("\"", "&quot;")

/// Lookup a specified key in a dictionary, possibly
/// ignoring newlines or spaces in the key.
let (|LookupKey|_|) (dict:IDictionary<_, _>) (key:string) = 
  [ key; key.Replace("\r\n", ""); key.Replace("\r\n", " "); 
    key.Replace("\n", ""); key.Replace("\n", " ") ]
  |> Seq.tryPick (fun key ->
    match dict.TryGetValue(key) with
    | true, v -> Some v 
    | _ -> None)

/// Context passed around while formatting the HTML
type FormattingContext =
  { LineBreak : unit -> unit
    Newline : string
    Writer : TextWriter
    Links : IDictionary<string, string * option<string>>
    ParagraphIndent : unit -> unit
    WithinList : bool }
    member this.Indent (text:string) = 
        this.ParagraphIndent()
        this.Writer.Write text

let bigBreak (ctx:FormattingContext) () =
  ctx.Writer.Write(ctx.Newline + ctx.Newline)
let smallBreak (ctx:FormattingContext) () =
  ctx.Writer.Write(ctx.Newline)
let noBreak (ctx:FormattingContext) () = ()

let indentLevel (level:int) (ctx:FormattingContext) =
    ctx.ParagraphIndent();
    for i in [1..level] do ctx.Writer.Write "  "

let indent (ctx:FormattingContext) () =
    indentLevel 1 ctx
let noIndent (ctx:FormattingContext) () = ()

/// Write MarkdownSpan value to a TextWriter
let rec formatSpan (ctx:FormattingContext) = function 
  | Literal(str) -> 
      ctx.Writer.Write str
  | HardLineBreak -> 
      ctx.Writer.Write("<LineBreak />")
  | IndirectLink(body, _, LookupKey ctx.Links (link, title)) 
  | DirectLink(body, (link, title)) -> 
      ctx.Writer.Write("<!-- Hyperlink -->")
      ctx.Writer.Write("<Hyperlink NavigateUri=\"" + htmlEncode(link) + "\">")
      formatSpans {ctx with ParagraphIndent = noIndent ctx } body
      ctx.Writer.Write "</Hyperlink>"
  | IndirectLink(body, original, _) ->
      ctx.Writer.Write("[")
      formatSpans {ctx with ParagraphIndent = noIndent ctx } body
      ctx.Writer.Write("]")
      ctx.Writer.Write(original)
  | IndirectImage(body, _, LookupKey ctx.Links (link, title)) 
  | DirectImage(body, (link, title)) -> 
      ctx.Indent <| "<InlineUIContainer>" + ctx.Newline
      ctx.Indent <| "  <Image>" + ctx.Newline
      ctx.Indent <| "    <Image.Source>" + ctx.Newline
      ctx.Indent <| "      <BitmapImage UriSource=\"" + htmlEncodeQuotes(link) + "\" />" + ctx.Newline
      ctx.Indent <| "    </Image.Source>" + ctx.Newline
      ctx.Indent <| "  </Image>" + ctx.Newline
      ctx.Indent <| "</InlineUIContainer>"
  | IndirectImage(body, original, _) ->
      ctx.Writer.Write("[")
      ctx.Writer.Write(body)
      ctx.Writer.Write("]")
      ctx.Writer.Write(original)
  | Strong(body) -> 
      ctx.Writer.Write("<Bold>")
      formatSpans ctx body
      ctx.Writer.Write("</Bold>")
  | InlineCode(body) -> 
      ctx.Writer.Write "<InlineUIContainer Style=\"{StaticResource InlineCodeStyle}\">"
      ctx.Writer.Write (htmlEncode body)
      ctx.Writer.Write "</InlineUIContainer>"
  | Emphasis(body) -> 
      ctx.Writer.Write("<Italic>")
      formatSpans ctx body
      ctx.Writer.Write("</Italic>")

/// Write list of MarkdownSpan values to a TextWriter
and formatSpans ctx = List.iter (formatSpan ctx)

/// Write a MarkdownParagraph value to a TextWriter
let rec formatParagraph (ctx:FormattingContext) paragraph =
  match paragraph with
  | Heading(n, spans) -> 
      ctx.Indent <| "<!-- H" + string n + " -->" + ctx.Newline
      ctx.Writer.Write("<Paragraph Style=\"{StaticResource Heading" + string n + "}\">")
      formatSpans ctx spans
      ctx.Writer.Write("</Paragraph>")
  | Paragraph(spans) ->
      ctx.Indent <| "<!-- Paragraph -->" + ctx.Newline
      ctx.Indent <| "<Paragraph>" + ctx.Newline
      formatSpans { ctx with ParagraphIndent = indent ctx } spans
      ctx.Writer.Write ctx.Newline
      ctx.Indent <| "</Paragraph>"
  | HorizontalRule ->
      ctx.Indent <| "<!-- HR -->" + ctx.Newline
      ctx.Indent <| "<BlockUIContainer>"
      ctx.Indent <| "  <Line X2=\"1\" StrokeThickness=\"1\" />"
      ctx.Indent <| "</BlockUIContainer>"
  | CodeBlock(code) ->
      ctx.Indent <| "<!-- CodeBlock -->" + ctx.Newline
      ctx.Indent <| "<Paragraph>"
      ctx.Writer.Write(htmlEncode code)
      ctx.Indent <| "</Paragraph>"
  | TableBlock(headers, alignments, rows) -> // rec
      ctx.Indent <| "<!-- Table -->" + ctx.Newline
      ctx.Indent <| "<Paragraph>" + ctx.Newline
      for id, row in rows |> List.mapi(fun i row -> (i + 1, row)) do
        for cell in row do
            for paragraph in cell do
                formatParagraph { ctx with LineBreak = noBreak ctx } paragraph
        ctx.Writer.Write(ctx.Newline)
      ctx.Indent <| "</Paragraph>"
  | ListBlock(kind, items) -> // rec
      ctx.Indent <| "<!-- List (" + string kind + ") -->" + ctx.Newline
      ctx.Indent <| "<Paragraph>" + ctx.Newline
      ctx.Indent <| "  <InlineUIContainer>" + ctx.Newline
      ctx.Indent <| "    <StackPanel>" + ctx.Newline
      let indention = indentLevel 3
      for body in items do
        formatListItem { ctx with LineBreak = noBreak ctx; ParagraphIndent = fun i -> indention ctx } body
      ctx.Indent <| "    </StackPanel>" + ctx.Newline
      ctx.Indent <| "  </InlineUIContainer>" + ctx.Newline
      ctx.Indent <| "</Paragraph>"
  | QuotedBlock(body) ->
      ctx.Indent <| "<!-- Quoted Block -->" + ctx.Newline
      ctx.Indent <| "<Paragraph Style=\"{StaticResource BlockQuote}\">" + ctx.Newline
      formatParagraphs { ctx with ParagraphIndent = indent ctx } body
      ctx.Indent <| "</Paragraph>"
  | Span spans -> 
      formatSpans ctx spans
  | HtmlBlock(code) ->
      ctx.Indent <| "<!-- HTML Block -->" + ctx.Newline
      ctx.Indent <| code
  ctx.LineBreak()

and formatListItem (ctx:FormattingContext) body =
  ctx.Indent <| "<Grid><Grid.ColumnDefinitions><ColumnDefinition Width=\"15\" /><ColumnDefinition /></Grid.ColumnDefinitions>" + ctx.Newline
  ctx.Indent <| "<TextBlock Grid.Column=\"0\">&#x2022;</TextBlock>"
  body |> List.iteri(fun index para ->
    formatListParagraph { ctx with ParagraphIndent = indent ctx } para)
  ctx.Indent <| "</Grid>" + ctx.Newline

and formatListParagraph (ctx:FormattingContext) = function
  | Paragraph(spans) -> 
      ctx.Indent <| "<!-- List Paragraph -->" + ctx.Newline
  | CodeBlock(code) -> 
      ctx.Indent <| "<!-- List Code Block -->" + ctx.Newline
  | QuotedBlock(body) -> 
      ctx.Indent <| "<!-- List Quoted Block -->" + ctx.Newline
  | ListBlock(kind, items) -> // recursive
      ctx.Indent <| "<!-- Nested List -->" + ctx.Newline
      for body in items do
        formatListItem { ctx with ParagraphIndent = indent ctx } body
  | Span spans -> 
      ctx.Writer.Write ("<!-- Spans -->")
      formatSpans ctx spans
      ctx.Writer.Write ctx.Newline
  | _ -> ()

/// Write a list of MarkdownParagraph values to a TextWriter
and formatParagraphs ctx paragraphs = 
  let length = List.length paragraphs
  let smallCtx = { ctx with LineBreak = smallBreak ctx }
  let bigCtx = { ctx with LineBreak = bigBreak ctx }
  for last, paragraph in paragraphs |> Seq.mapi (fun i v -> (i = length - 1), v) do
    formatParagraph (if last then smallCtx else bigCtx) paragraph

/// Format Markdown document and write the result to 
/// a specified TextWriter. Parameters specify newline character
/// and a dictionary with link keys defined in the document.
let formatMarkdown writer newline links = 
  formatParagraphs 
    { Writer = writer
      Links = links
      Newline = newline
      LineBreak = ignore
      ParagraphIndent = ignore
      WithinList = false }
