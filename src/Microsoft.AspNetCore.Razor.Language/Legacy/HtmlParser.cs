// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Razor.Language.Syntax.InternalSyntax;

namespace Microsoft.AspNetCore.Razor.Language.Legacy
{
    internal partial class HtmlMarkupParser
    {
        public HtmlDocumentSyntax ParseDocument()
        {
            if (Context == null)
            {
                throw new InvalidOperationException(Resources.Parser_Context_Not_Set);
            }

            var builder = Pool.Allocate<RazorSyntaxNode>();
            using (PushSpanContextConfig(DefaultMarkupSpanContext))
            {
                NextToken();
                while (!EndOfFile)
                {
                    SkipToAndParseCode(builder, SyntaxKind.OpenAngle);
                    ParseTagInDocumentContext(builder);
                }
                AcceptMarkerTokenIfNecessary();
                builder.Add(OutputTokensAsHtmlLiteral());
            }

            var markup = SyntaxFactory.HtmlMarkupBlock(builder.ToList());
            Pool.Free(builder);

            return SyntaxFactory.HtmlDocument(markup);
        }

        private void SkipToAndParseCode(SyntaxListBuilder<RazorSyntaxNode> builder, SyntaxKind type)
        {
            SkipToAndParseCode(builder, token => token.Kind == type);
        }

        private void SkipToAndParseCode(SyntaxListBuilder<RazorSyntaxNode> builder, Func<SyntaxToken, bool> condition)
        {
            SyntaxToken last = null;
            var startOfLine = false;
            while (!EndOfFile && !condition(CurrentToken))
            {
                if (Context.NullGenerateWhitespaceAndNewLine)
                {
                    Context.NullGenerateWhitespaceAndNewLine = false;
                    SpanContext.ChunkGenerator = SpanChunkGenerator.Null;
                    AcceptTokenWhile(token => token.Kind == SyntaxKind.Whitespace);
                    if (At(SyntaxKind.NewLine))
                    {
                        AcceptTokenAndMoveNext();
                    }

                    builder.Add(OutputTokensAsHtmlLiteral());
                }
                else if (At(SyntaxKind.NewLine))
                {
                    if (last != null)
                    {
                        AcceptToken(last);
                    }

                    // Mark the start of a new line
                    startOfLine = true;
                    last = null;
                    AcceptTokenAndMoveNext();
                }
                else if (At(SyntaxKind.Transition))
                {
                    var transition = CurrentToken;
                    NextToken();
                    if (At(SyntaxKind.Transition))
                    {
                        if (last != null)
                        {
                            AcceptToken(last);
                            last = null;
                        }
                        builder.Add(OutputTokensAsHtmlLiteral());
                        AcceptToken(transition);
                        SpanContext.ChunkGenerator = SpanChunkGenerator.Null;
                        builder.Add(OutputTokensAsHtmlLiteral());
                        AcceptTokenAndMoveNext();
                        continue; // while
                    }
                    else
                    {
                        if (!EndOfFile)
                        {
                            PutCurrentBack();
                        }
                        PutBack(transition);
                    }

                    // Handle whitespace rewriting
                    if (last != null)
                    {
                        if (!Context.DesignTimeMode && last.Kind == SyntaxKind.Whitespace && startOfLine)
                        {
                            // Put the whitespace back too
                            startOfLine = false;
                            PutBack(last);
                            last = null;
                        }
                        else
                        {
                            // Accept last
                            AcceptToken(last);
                            last = null;
                        }
                    }

                    OtherParserBlock(builder);
                }
                else if (At(SyntaxKind.RazorCommentTransition))
                {
                    if (last != null)
                    {
                        // Don't render the whitespace between the start of the line and the razor comment.
                        if (startOfLine && last.Kind == SyntaxKind.Whitespace)
                        {
                            AcceptMarkerTokenIfNecessary();
                            // Output the tokens that may have been accepted prior to the whitespace.
                            builder.Add(OutputTokensAsHtmlLiteral());

                            SpanContext.ChunkGenerator = SpanChunkGenerator.Null;
                        }

                        AcceptToken(last);
                        last = null;
                    }

                    AcceptMarkerTokenIfNecessary();
                    builder.Add(OutputTokensAsHtmlLiteral());

                    var comment = ParseRazorComment();
                    builder.Add(comment);

                    // Handle the whitespace and newline at the end of a razor comment.
                    if (startOfLine &&
                        (At(SyntaxKind.NewLine) ||
                        (At(SyntaxKind.Whitespace) && NextIs(SyntaxKind.NewLine))))
                    {
                        AcceptTokenWhile(IsSpacingToken(includeNewLines: false));
                        AcceptTokenAndMoveNext();
                        SpanContext.ChunkGenerator = SpanChunkGenerator.Null;
                        builder.Add(OutputTokensAsHtmlLiteral());
                    }
                }
                else
                {
                    // As long as we see whitespace, we're still at the "start" of the line
                    startOfLine &= At(SyntaxKind.Whitespace);

                    // If there's a last token, accept it
                    if (last != null)
                    {
                        AcceptToken(last);
                        last = null;
                    }

                    // Advance
                    last = CurrentToken;
                    NextToken();
                }
            }

            if (last != null)
            {
                AcceptToken(last);
            }
        }

        /// <summary>
        /// Reads the content of a tag (if present) in the MarkupDocument (or MarkupSection) context,
        /// where we don't care about maintaining a stack of tags.
        /// </summary>
        private void ScanTagInDocumentContext(SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            if (At(SyntaxKind.OpenAngle))
            {
                if (NextIs(SyntaxKind.Bang))
                {
                    // Checking to see if we meet the conditions of a special '!' tag: <!DOCTYPE, <![CDATA[, <!--.
                    if (!IsBangEscape(lookahead: 1))
                    {
                        if (Lookahead(2)?.Kind == SyntaxKind.DoubleHyphen)
                        {
                            builder.Add(OutputTokensAsHtmlLiteral());
                        }

                        AcceptTokenAndMoveNext(); // Accept '<'
                        ParseBangTag(builder);

                        return;
                    }

                    // We should behave like a normal tag that has a parser escape, fall through to the normal
                    // tag logic.
                }
                else if (NextIs(SyntaxKind.QuestionMark))
                {
                    AcceptTokenAndMoveNext(); // Accept '<'
                    ParseXmlPI(builder);
                    return;
                }

                builder.Add(OutputTokensAsHtmlLiteral());

                // Start tag block
                var tagBuilder = Pool.Allocate<RazorSyntaxNode>();

                AcceptTokenAndMoveNext(); // Accept '<'

                if (!At(SyntaxKind.ForwardSlash))
                {
                    ParseOptionalBangEscape(tagBuilder);

                    // Parsing a start tag
                    var scriptTag = At(SyntaxKind.Text) &&
                                    string.Equals(CurrentToken.Content, "script", StringComparison.OrdinalIgnoreCase);
                    OptionalToken(SyntaxKind.Text);
                    ParseTagContent(tagBuilder); // Parse the tag, don't care about the content
                    OptionalToken(SyntaxKind.ForwardSlash);
                    OptionalToken(SyntaxKind.CloseAngle);

                    // If the script tag expects javascript content then we should do minimal parsing until we reach
                    // the end script tag. Don't want to incorrectly parse a "var tag = '<input />';" as an HTML tag.
                    if (scriptTag && !CurrentScriptTagExpectsHtml(builder))
                    {
                        tagBuilder.Add(OutputTokensAsHtmlLiteral());
                        var block = SyntaxFactory.HtmlTagBlock(tagBuilder.ToList());
                        builder.Add(block);
                        Pool.Free(tagBuilder);

                        SkipToEndScriptAndParseCode(tagBuilder);
                        return;
                    }
                }
                else
                {
                    // Parsing an end tag
                    // This section can accept things like: '</p  >' or '</p>' etc.
                    ParserState = ParserState.EndTag;
                    OptionalToken(SyntaxKind.ForwardSlash);

                    // Whitespace here is invalid (according to the spec)
                    ParseOptionalBangEscape(tagBuilder);
                    OptionalToken(SyntaxKind.Text);
                    OptionalToken(SyntaxKind.Whitespace);
                    OptionalToken(SyntaxKind.CloseAngle);
                    ParserState = ParserState.Content;
                }

                tagBuilder.Add(OutputTokensAsHtmlLiteral());

                // End tag block
                var tagBlock = SyntaxFactory.HtmlTagBlock(tagBuilder.ToList());
                builder.Add(tagBlock);
                Pool.Free(tagBuilder);
            }
        }

        public HtmlMarkupBlockSyntax ParseBlock()
        {
            var builder = Pool.Allocate<RazorSyntaxNode>();
            try
            {
                // TODO
                var markupBlock = builder.ToList();

                return SyntaxFactory.HtmlMarkupBlock(markupBlock);
            }
            finally
            {
                Pool.Free(builder);
            }
        }

        private void DefaultMarkupSpanContext(SpanContextBuilder spanContext)
        {
            spanContext.ChunkGenerator = new MarkupChunkGenerator();
            spanContext.EditHandler = new SpanEditHandler(Language.TokenizeString, AcceptedCharactersInternal.Any);
        }

        private void OtherParserBlock(SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            AcceptMarkerTokenIfNecessary();
            builder.Add(OutputTokensAsHtmlLiteral());

            RazorSyntaxNode codeBlock;
            using (PushSpanContextConfig())
            {
                codeBlock = CodeParser.ParseBlock();
            }

            builder.Add(codeBlock);
            InitializeContext(SpanContext);
            NextToken();
        }
    }
}
