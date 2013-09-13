namespace Farse.Markdown

open System
open System.IO
open System.Collections.Generic

open Farse.Markdown
open Farse.Patterns
open Farse.Markdown.Parser
open Farse.Markdown.Formatting.Xaml

type Xaml =

  /// Transform Markdown document into HTML format. The result
  /// will be written to the provided TextWriter.
  static member Transform(text, writer:TextWriter, newline) = 
    let doc = Markdown.Parse(text, newline)
    formatMarkdown writer newline doc.DefinedLinks doc.Paragraphs

  /// Transform Markdown document into HTML format. The result
  /// will be written to the provided TextWriter.
  static member Transform(text, writer:TextWriter) = 
    Xaml.Transform(text, writer, Environment.NewLine)

  /// Transform Markdown document into HTML format. 
  /// The result will be returned as a string.
  static member Transform(text, newline) =
    let sb = new System.Text.StringBuilder()
    use wr = new StringWriter(sb)
    Xaml.Transform(text, wr, newline)
    sb.ToString()

  /// Transform Markdown document into HTML format. 
  /// The result will be returned as a string.
  static member Transform(text) =
    Xaml.Transform(text, Environment.NewLine)
  
  /// Transform the provided MakrdownDocument into HTML
  /// format and write the result to a given writer.
  static member Write(doc:MarkdownDocument, writer, newline) = 
    formatMarkdown writer newline doc.DefinedLinks doc.Paragraphs

  /// Transform the provided MakrdownDocument into HTML
  /// format and return the result as a string.
  static member Write(doc:MarkdownDocument, newline) = 
    let sb = new System.Text.StringBuilder()
    use wr = new StringWriter(sb)
    Xaml.Write(doc, wr, newline)
    sb.ToString()

  /// Transform the provided MakrdownDocument into HTML
  /// format and return the result as a string.
  static member Write(doc:MarkdownDocument) = 
    Xaml.Write(doc, Environment.NewLine)

  /// Transform the provided MakrdownDocument into HTML
  /// format and write the result to a given writer.
  static member Write(doc:MarkdownDocument, writer) = 
    Xaml.Write(doc, writer, Environment.NewLine)