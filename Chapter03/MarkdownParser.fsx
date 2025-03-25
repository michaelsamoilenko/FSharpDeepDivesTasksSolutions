// ======================================================================================
// This is not fully completed and not super well designed completion of tasks for Chapter 3 of "F# Deep Dives" book
// ======================================================================================
open System
open System.IO
open System.Text.RegularExpressions

type MarkdownDocument = list<MarkdownBlock>

and MarkdownBlock =
    | Heading of int * MarkdownSpans
    | Paragraph of MarkdownSpans
    | CodeBlock of list<string>
    | BlockQuote of MarkdownBlocks

and MarkdownSpans = list<MarkdownSpan>
and MarkdownBlocks = list<MarkdownBlock>

and MarkdownSpan =
    | Literal of string
    | InlineCode of string
    | Strong of MarkdownSpans
    | Emphasis of MarkdownSpans
    | HyperLink of MarkdownSpans * string
    | HardLineBreak

let rec parseInlineBody acc =
    function
    | '`' :: rest -> Some(List.rev acc, rest)
    | c :: chars -> parseInlineBody (c :: acc) chars
    | [] -> None

let parseInline =
    function
    | '`' :: chars -> parseInlineBody [] chars
    | _ -> None

let toString chars =
    String(chars |> Array.ofList)

let (|StartsWith|_|) prefix input =
    let rec loop = function
        | p::prefix, r::rest when p = r ->
            loop (prefix, rest)
        | [], rest ->
            Some(rest)
        | _ -> None
    loop (prefix, input)

let rec parseBracketedBody closing acc = function
    | StartsWith closing rest ->
        Some(List.rev acc, rest)
    | c::chars ->
        parseBracketedBody closing (c::acc) chars
    | _ -> None

let parseBracketed opening closing = function
    | StartsWith opening chars ->
        parseBracketedBody closing [] chars
    | _ -> None

let (|Delimited|_|) delim = parseBracketed delim delim

let (|Bracketed|_|) openingDelim closingDelim = parseBracketed openingDelim closingDelim

let rec parseSpans acc chars = seq {
    let emitLiteral() = seq {
        if acc <> [] then
            yield acc |> List.rev |> toString |> Literal }
    match chars with
    | StartsWith [' '; ' '; '\n'; '\r'] chars
    | StartsWith [' '; ' '; '\r'] chars
    | StartsWith [' '; ' '; '\n';] chars ->
        yield! emitLiteral()
        yield HardLineBreak
        yield! parseSpans [] chars
    | Delimited ['`'] (body, chars) ->
        yield! emitLiteral()
        yield InlineCode(toString body)
        yield! parseSpans [] chars
    | Delimited ['*'; '*' ] (body, chars)
    | Delimited ['_'; '_' ] (body, chars) ->
        yield! emitLiteral()
        yield Strong(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | Delimited ['*' ] (body, chars)
    | Delimited ['_' ] (body, chars) ->
        yield! emitLiteral()
        yield Emphasis(parseSpans [] body |> List.ofSeq)
        yield! parseSpans [] chars
    | Bracketed ['['] [']'] (body, Bracketed ['('] [')'] (url, chars)) ->
        yield! emitLiteral()
        yield HyperLink(parseSpans [] body |> List.ofSeq, toString url)
        yield! parseSpans [] chars
    | c::chars ->
        yield! parseSpans (c::acc) chars
    | [] ->
        yield! emitLiteral() }

// Example: parsing string to markdown spans
"hello  \n\rworld  \r!!!" |> List.ofSeq |> parseSpans [] |> List.ofSeq
"**important `code`** and _emphasized_" |> List.ofSeq |> parseSpans [] |> List.ofSeq
"**important `code`** and _emphasized_ [**F# home page**](http://fsharp.net)" |> List.ofSeq |> parseSpans [] |> List.ofSeq

module List =
    let partitionWhile f =
        let rec loop acc = function
        | x::xs when f x -> loop (x::acc) xs
        | xs -> List.rev acc, xs
        loop []

let (|PrefixedLines|) (prefix:string) (lines:list<string>) =
    let prefixed, other =
        lines |> List.partitionWhile _.StartsWith(prefix)
    [ for line in prefixed -> line.Substring(prefix.Length) ], other

let (|SeparatedByLineBreak|) lines =
    let isWhite = System.String.IsNullOrWhiteSpace
    match List.partitionWhile (isWhite >> not) lines with
    | par, _ :: rest
    | par, ([] as rest) -> par, rest

let (|AsCharList|) (str:string) =
    List.ofSeq str

let (|AsHeading|_|) = function
    | AsCharList(possibleHeading) :: lines ->
        let phoneKeys, other = possibleHeading |> List.partitionWhile (fun char -> char = '#')
        match phoneKeys, other with
        | phoneKeys, space :: rest when phoneKeys <> [] && Char.IsWhiteSpace space ->
            Some(phoneKeys |> Seq.length, rest, lines)
        | _ -> None
    | _ -> None

let rec parseBlocks lines = seq {
    match lines with
    | AsHeading(size, heading, lines) ->
        yield Heading(size, parseSpans [] heading |> List.ofSeq)
        yield! parseBlocks lines
    | PrefixedLines "    " (body, lines) when body <> [] ->
        yield CodeBlock(body)
        yield! parseBlocks lines
    | SeparatedByLineBreak (beforeLineBreak, afterLineBreak) when beforeLineBreak <> [] ->
        match beforeLineBreak with
        | quotedLine :: _ when quotedLine.StartsWith("> ") ->
           let beforeLineBreakWithoutQuote = beforeLineBreak |> List.map (fun line -> if line.StartsWith("> ") then line.Substring(2) elif line = ">" then String.Empty else line)
           let markdownBlocks = parseBlocks beforeLineBreakWithoutQuote |> Seq.toList
           yield BlockQuote(markdownBlocks)
           yield! parseBlocks afterLineBreak
        | _ ->
            let body = String.concat " " beforeLineBreak |> List.ofSeq
            yield Paragraph(parseSpans [] body |> List.ofSeq)
            yield! parseBlocks afterLineBreak
    | line::lines when String.IsNullOrWhiteSpace(line) ->
        yield! parseBlocks lines
    | _ -> ()
}

// Example: parsing string to markdown blocks
let sample = """
  # Visual F#

  F# is a **programming language** that supports _functional_, as
  well as _object-oriented_ and _imperative_ programming styles.
  Hello world can be written as follows:

      printfn "Hello world!"

  For more information, see the [F# home page] (http://fsharp.net) or
  read [Real-World Func tional Programming](http://manning.com/petricek)
  published by [Manning](http://manning.com).

"""

let sampleDoc = sample.Split([| "\r\n"; "\r"; "\n";|], StringSplitOptions.None) |> List.ofSeq |> parseBlocks |> List.ofSeq

let outputElement (output:TextWriter) tag attributes body =
    let attrString =
        [ for k, v in attributes -> k + "=\"" + v + "\"" ]
        |> String.concat " "
    output.Write("<" + tag + attrString + ">")
    body ()
    output.Write("</" + tag + ">")

let rec formatSpan (output:TextWriter) = function
    | Literal(str) ->
        output.Write(str)
    | Strong(spans) ->
        outputElement output "strong" [] (fun () -> spans |> List.iter (formatSpan output))
    | Emphasis(spans) ->
        outputElement output "em" [] (fun () -> spans |> List.iter (formatSpan output))
    | HyperLink(spans, url) ->
        outputElement output "a" ["href", url] (fun () -> spans |> List.iter (formatSpan output))
    | InlineCode(code) ->
        output.Write("<code>" + code + "</code>")

let rec formatBlock (output:TextWriter) = function
    | Heading(size, spans) ->
        outputElement output ("h" + size.ToString()) [] (fun () ->
        spans |> List.iter (formatSpan output))
    | Paragraph(spans) ->
        outputElement output "p" [] (fun () ->
        spans |> List.iter (formatSpan output))
    | CodeBlock(lines) ->
        outputElement output "pre" [] (fun () ->
        lines |> List.iter output.WriteLine )

// Example: parsing markdown blocks to html
let sb = System.Text.StringBuilder()
let output = new StringWriter(sb)
sampleDoc |> List.iter (formatBlock output)
sb.ToString()

module Matching =
    let (|SpanNode|_|) span =
        match span with
        | Strong spans | Emphasis spans | HyperLink(spans, _) ->
            Some(box span, spans)
        | _ -> None

    let SpanNode (span:obj, children) =
        match unbox span with
        | Strong _ -> Strong children
        | Emphasis _ -> Emphasis children
        | HyperLink(_, url) -> HyperLink(children, url)
        | _ -> invalidArg "" "Incorrect MarkdownSpan"

    let (|BlockNode|_|) block =
        match block with
        | Heading(_, spans)
        | Paragraph(spans) -> Some(box block, spans)
        | _ -> None

    let BlockNode (block:obj, spans) =
        match unbox block with
        | Heading(a, _) -> Heading(a, spans)
        | Paragraph(_) -> Paragraph(spans)
        | _ -> invalidArg "" "Incorrect MarkdownBlock."

let rec generateSpanReferences (refs:ResizeArray<_>) = function
    | HyperLink(body, url) ->
      let id = sprintf "[%d]" (refs.Count + 1)
      refs.Add(id, url)
      [HyperLink(body, url); Literal(id)]
    | Matching.SpanNode(shape, spans) ->
        let spans = spans |> List.collect (generateSpanReferences refs)
        [Matching.SpanNode(shape, spans)]
    | span -> [span]

let generateBlockReferences refs = function
    | Matching.BlockNode(shape, spans) ->
        let spans = spans |> List.collect (generateSpanReferences refs)
        Matching.BlockNode(shape, spans)
    | block -> block

// Example: Generating references from a sample document
let ndoc = parseBlocks [ """For more information, see the
      [F# home page](http://fsharp.net) or read [Real-World Functional
      Programming](http://manning.com/petricek) published by
      [Manning](http://manning.com).""" ] |> List.ofSeq
let refs = ResizeArray<_>()
let docRef = ndoc |> List.map (generateBlockReferences refs)

let countNumberOfWordsInBlock markdownBlock =
    let rec countNumberOfWordsInSpan = function
        | Literal body ->
            let matches = Regex.Matches(body, @"\b[\w']*\b")
            let numberOfWords = matches |> Seq.cast<Match> |> Seq.map _.Value |> Seq.filter (String.IsNullOrEmpty >> not) |> Seq.length
            numberOfWords
        | Matching.SpanNode(shape, spans) ->
            let numberOfWords = spans |> List.map countNumberOfWordsInSpan |> List.sum
            numberOfWords
        | _ -> 0

    match markdownBlock with
    | Matching.BlockNode(_, spans) ->
        let numberOfWords = spans |> List.map countNumberOfWordsInSpan |> List.sum
        numberOfWords
    | _ -> 0

// Example: count words in a string
let countWordsSample = """
# F# Programming language
F# is a **programming language** that supports _functional_, as
well as _object-oriented_ and _imperative_ programming styles.
Hello world can be written as follows:

    printfn "Hello world!"

For more information, see the [F# home page] (http://fsharp.net) or
read [Real-World Functional Programming](http://manning.com/petricek)
published by [Manning](http://manning.com).
"""
let numberOfWords = countWordsSample.Split([| "\r\n"; "\r"; "\n";|], StringSplitOptions.None) |> List.ofSeq |> parseBlocks |> Seq.map countNumberOfWordsInBlock |> Seq.sum

let rec generateHeadingNumbering (headersTracker: Map<_,_>) = function
    | Heading(size, span) ->
        let number = headersTracker |> Map.tryFind size |> Option.map (fun number -> number + 1) |> Option.defaultValue 1
        let acc = headersTracker |> Map.filter (fun key _ -> key > size |> not) |> Map.add size number
        let formatedNumber =
            acc
            |> Seq.sortBy (fun (KeyValue(heading, _)) -> heading)
            |> Seq.map (fun (KeyValue(_, currentNumber)) -> currentNumber.ToString())
            |> String.concat "."
        Heading(size, Literal(formatedNumber) :: span), acc
    | markdownBlock ->
        markdownBlock, headersTracker

// Example: enrich headers with numbers. So in my case each line will be prepended with [1; 1.1; 1.2; 2]
let headingsSample = """
# Paradigms
## Object-oriented
## Functional
# Conclusions
"""

let parsedHeadings =
    countWordsSample.Split([| "\r\n"; "\r"; "\n";|], StringSplitOptions.None)
    |> List.ofSeq
    |> parseBlocks

let result =
    parsedHeadings
    |> Seq.mapFold generateHeadingNumbering Map.empty
    |> fst
    |> Seq.toList
