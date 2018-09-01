// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
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

            using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
            using (PushSpanContextConfig(DefaultMarkupSpanContext))
            {
                var builder = pooledResult.Builder;
                NextToken();
                while (!EndOfFile)
                {
                    SkipToAndParseCode(builder, SyntaxKind.OpenAngle);
                    ParseTagInDocumentContext(builder);
                }
                AcceptMarkerTokenIfNecessary();
                builder.Add(OutputTokensAsHtmlLiteral());

                var markup = SyntaxFactory.HtmlMarkupBlock(builder.ToList());

                return SyntaxFactory.HtmlDocument(markup);
            }
        }

        private void SkipToAndParseCode(in SyntaxListBuilder<RazorSyntaxNode> builder, SyntaxKind type)
        {
            SkipToAndParseCode(builder, token => token.Kind == type);
        }

        private void SkipToAndParseCode(in SyntaxListBuilder<RazorSyntaxNode> builder, Func<SyntaxToken, bool> condition)
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
        private void ParseTagInDocumentContext(in SyntaxListBuilder<RazorSyntaxNode> builder)
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
                    TryParseXmlPI();
                    return;
                }

                builder.Add(OutputTokensAsHtmlLiteral());

                // Start tag block
                using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
                {
                    var tagBuilder = pooledResult.Builder;
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
                }
            }
        }

        private void ParseTagContent(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            if (!At(SyntaxKind.Whitespace) && !At(SyntaxKind.NewLine))
            {
                // We should be right after the tag name, so if there's no whitespace or new line, something is wrong
                RecoverToEndOfTag(builder);
            }
            else
            {
                // We are here ($): <tag$ foo="bar" biz="~/Baz" />
                while (!EndOfFile && !IsEndOfTag())
                {
                    BeforeAttribute(builder);
                }
            }
        }

        private bool IsEndOfTag()
        {
            if (At(SyntaxKind.ForwardSlash))
            {
                if (NextIs(SyntaxKind.CloseAngle))
                {
                    return true;
                }
                else
                {
                    AcceptTokenAndMoveNext();
                }
            }
            return At(SyntaxKind.CloseAngle) || At(SyntaxKind.OpenAngle);
        }

        private void BeforeAttribute(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            // http://dev.w3.org/html5/spec/tokenization.html#before-attribute-name-state
            // Capture whitespace
            var whitespace = ReadWhile(token => token.Kind == SyntaxKind.Whitespace || token.Kind == SyntaxKind.NewLine);

            if (At(SyntaxKind.Transition) || At(SyntaxKind.RazorCommentTransition))
            {
                // Transition outside of attribute value => Switch to recovery mode
                AcceptToken(whitespace);
                RecoverToEndOfTag(builder);
                return;
            }

            // http://dev.w3.org/html5/spec/tokenization.html#attribute-name-state
            // Read the 'name' (i.e. read until the '=' or whitespace/newline)
            var name = Enumerable.Empty<SyntaxToken>();
            var whitespaceAfterAttributeName = Enumerable.Empty<SyntaxToken>();
            if (IsValidAttributeNameToken(CurrentToken))
            {
                name = ReadWhile(token =>
                                 token.Kind != SyntaxKind.Whitespace &&
                                 token.Kind != SyntaxKind.NewLine &&
                                 token.Kind != SyntaxKind.Equals &&
                                 token.Kind != SyntaxKind.CloseAngle &&
                                 token.Kind != SyntaxKind.OpenAngle &&
                                 (token.Kind != SyntaxKind.ForwardSlash || !NextIs(SyntaxKind.CloseAngle)));

                // capture whitespace after attribute name (if any)
                whitespaceAfterAttributeName = ReadWhile(
                    token => token.Kind == SyntaxKind.Whitespace || token.Kind == SyntaxKind.NewLine);
            }
            else
            {
                // Unexpected character in tag, enter recovery
                AcceptToken(whitespace);
                RecoverToEndOfTag(builder);
                return;
            }

            if (!At(SyntaxKind.Equals))
            {
                // Minimized attribute

                // We are at the prefix of the next attribute or the end of tag. Put it back so it is parsed later.
                PutCurrentBack();
                PutBack(whitespaceAfterAttributeName);

                // Output anything prior to the attribute, in most cases this will be the tag name:
                // |<input| checked />. If in-between other attributes this will noop or output malformed attribute
                // content (if the previous attribute was malformed).
                builder.Add(OutputTokensAsHtmlLiteral());

                using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
                {
                    var attributeBuilder = pooledResult.Builder;

                    AcceptToken(whitespace);
                    AcceptToken(name);
                    attributeBuilder.Add(OutputTokensAsHtmlLiteral());

                    var attributeBlock = SyntaxFactory.HtmlAttributeBlock(attributeBuilder.ToList());
                    builder.Add(attributeBlock);
                }

                return;
            }

            // Not a minimized attribute, parse as if it were well-formed (if attribute turns out to be malformed we
            // will go into recovery).
            builder.Add(OutputTokensAsHtmlLiteral());

            // Start a new markup block for the attribute
            using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
            {
                var attributeBuilder = pooledResult.Builder;

                ParseAttributePrefix(attributeBuilder, whitespace, name, whitespaceAfterAttributeName);

                var attributeBlock = SyntaxFactory.HtmlAttributeBlock(attributeBuilder.ToList());
                builder.Add(attributeBlock);
            }
        }

        private void ParseAttributePrefix(
            in SyntaxListBuilder<RazorSyntaxNode> builder,
            IEnumerable<SyntaxToken> whitespace,
            IEnumerable<SyntaxToken> nameTokens,
            IEnumerable<SyntaxToken> whitespaceAfterAttributeName)
        {
            // First, determine if this is a 'data-' attribute (since those can't use conditional attributes)
            var name = string.Concat(nameTokens.Select(s => s.Content));
            var attributeCanBeConditional =
                Context.FeatureFlags.EXPERIMENTAL_AllowConditionalDataDashAttributes ||
                !name.StartsWith("data-", StringComparison.OrdinalIgnoreCase);

            // Accept the whitespace and name
            AcceptToken(whitespace);
            AcceptToken(nameTokens);

            // Since this is not a minimized attribute, the whitespace after attribute name belongs to this attribute.
            AcceptToken(whitespaceAfterAttributeName);
            Assert(SyntaxKind.Equals); // We should be at "="
            AcceptTokenAndMoveNext();

            var whitespaceAfterEquals = ReadWhile(token => token.Kind == SyntaxKind.Whitespace || token.Kind == SyntaxKind.NewLine);
            var quote = SyntaxKind.Unknown;
            if (At(SyntaxKind.SingleQuote) || At(SyntaxKind.DoubleQuote))
            {
                // Found a quote, the whitespace belongs to this attribute.
                AcceptToken(whitespaceAfterEquals);
                quote = CurrentToken.Kind;
                AcceptTokenAndMoveNext();
            }
            else if (whitespaceAfterEquals.Any())
            {
                // No quotes found after the whitespace. Put it back so that it can be parsed later.
                PutCurrentBack();
                PutBack(whitespaceAfterEquals);
            }

            // We now have the prefix: (i.e. '      foo="')
            var prefix = new LocationTagged<string>(string.Concat(TokenBuilder.ToList().Nodes.Select(s => s.Content)), Span.Start);

            if (attributeCanBeConditional)
            {
                Span.ChunkGenerator = SpanChunkGenerator.Null; // The block chunk generator will render the prefix
                builder.Add(OutputTokensAsHtmlLiteral());

                // Read the attribute value only if the value is quoted
                // or if there is no whitespace between '=' and the unquoted value.
                if (quote != SyntaxKind.Unknown || !whitespaceAfterEquals.Any())
                {
                    // Read the attribute value.
                    while (!EndOfFile && !IsEndOfAttributeValue(quote, CurrentToken))
                    {
                        ParseAttributeValue(builder, quote);
                    }
                }

                // Capture the suffix
                var suffix = new LocationTagged<string>(string.Empty, CurrentStart);
                if (quote != SyntaxKind.Unknown && At(quote))
                {
                    suffix = new LocationTagged<string>(CurrentToken.Content, CurrentStart);
                    AcceptTokenAndMoveNext();
                }

                if (TokenBuilder.Count > 0)
                {
                    // Again, block chunk generator will render the suffix
                    SpanContext.ChunkGenerator = SpanChunkGenerator.Null;
                    builder.Add(OutputTokensAsHtmlLiteral());
                }

                // Create the block chunk generator
                Contet.Builder.CurrentBlock.ChunkGenerator = new AttributeBlockChunkGenerator(
                    name, prefix, suffix);
            }
            else
            {
                // Output the attribute name, the equals and optional quote. Ex: foo="
                builder.Add(OutputTokensAsHtmlLiteral());

                if (quote == SyntaxKind.Unknown && whitespaceAfterEquals.Any())
                {
                    return;
                }

                // Not a "conditional" attribute, so just read the value
                SkipToAndParseCode(builder, token => IsEndOfAttributeValue(quote, token));

                // Output the attribute value (will include everything in-between the attribute's quotes).
                builder.Add(OutputTokensAsHtmlLiteral());

                if (quote != SyntaxKind.Unknown)
                {
                    OptionalToken(quote);
                }
                builder.Add(OutputTokensAsHtmlLiteral());
            }
        }

        private void RecoverToEndOfTag(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            // Accept until ">", "/" or "<", but parse code
            while (!EndOfFile)
            {
                SkipToAndParseCode(builder, IsTagRecoveryStopPoint);
                if (!EndOfFile)
                {
                    EnsureCurrent();
                    switch (CurrentToken.Kind)
                    {
                        case SyntaxKind.SingleQuote:
                        case SyntaxKind.DoubleQuote:
                            ParseQuoted(builder);
                            break;
                        case SyntaxKind.OpenAngle:
                        // Another "<" means this tag is invalid.
                        case SyntaxKind.ForwardSlash:
                        // Empty tag
                        case SyntaxKind.CloseAngle:
                            // End of tag
                            return;
                        default:
                            AcceptTokenAndMoveNext();
                            break;
                    }
                }
            }
        }

        private void ParseQuoted(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            var type = CurrentToken.Kind;
            AcceptTokenAndMoveNext();
            ParseQuoted(builder, type);
        }

        private void ParseQuoted(in SyntaxListBuilder<RazorSyntaxNode> builder, SyntaxKind type)
        {
            SkipToAndParseCode(builder, type);
            if (!EndOfFile)
            {
                Assert(type);
                AcceptTokenAndMoveNext();
            }
        }

        private bool ParseBangTag(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            // Accept "!"
            Assert(SyntaxKind.Bang);

            if (AcceptTokenAndMoveNext())
            {
                if (IsHtmlCommentAhead())
                {
                    using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
                    {
                        var htmlCommentBuilder = pooledResult.Builder;

                        // Accept the double-hyphen token at the beginning of the comment block.
                        AcceptTokenAndMoveNext();
                        SpanContext.EditHandler.AcceptedCharacters = AcceptedCharactersInternal.None;
                        htmlCommentBuilder.Add(OutputTokensAsHtmlLiteral());

                        SpanContext.EditHandler.AcceptedCharacters = AcceptedCharactersInternal.Whitespace;
                        while (!EndOfFile)
                        {
                            SkipToAndParseCode(htmlCommentBuilder, SyntaxKind.DoubleHyphen);
                            var lastDoubleHyphen = AcceptAllButLastDoubleHyphens1();

                            if (At(SyntaxKind.CloseAngle))
                            {
                                // Output the content in the comment block as a separate markup
                                SpanContext.EditHandler.AcceptedCharacters = AcceptedCharactersInternal.Whitespace;
                                htmlCommentBuilder.Add(OutputTokensAsHtmlLiteral());

                                // This is the end of a comment block
                                AcceptToken(lastDoubleHyphen);
                                AcceptTokenAndMoveNext();
                                SpanContext.EditHandler.AcceptedCharacters = AcceptedCharactersInternal.None;
                                htmlCommentBuilder.Add(OutputTokensAsHtmlLiteral());
                                return true;
                            }
                            else if (lastDoubleHyphen != null)
                            {
                                AcceptToken(lastDoubleHyphen);
                            }
                        }

                        var commentBlock = SyntaxFactory.HtmlCommentBlock(htmlCommentBuilder.ToList());
                        builder.Add(commentBlock);
                    }
                }
                else if (CurrentToken.Kind == SyntaxKind.LeftBracket)
                {
                    if (AcceptTokenAndMoveNext())
                    {
                        return TryParseCData();
                    }
                }
                else
                {
                    AcceptTokenAndMoveNext();
                    return AcceptTokenUntilAll(SyntaxKind.CloseAngle);
                }
            }

            return false;
        }

        private bool TryParseCData()
        {
            if (CurrentToken.Kind == SyntaxKind.Text && string.Equals(CurrentToken.Content, "cdata", StringComparison.OrdinalIgnoreCase))
            {
                if (AcceptTokenAndMoveNext())
                {
                    if (CurrentToken.Kind == SyntaxKind.LeftBracket)
                    {
                        return AcceptTokenUntilAll(SyntaxKind.RightBracket, SyntaxKind.RightBracket, SyntaxKind.CloseAngle);
                    }
                }
            }

            return false;
        }

        private bool TryParseXmlPI()
        {
            // Accept "?"
            Assert(SyntaxKind.QuestionMark);
            AcceptTokenAndMoveNext();
            return AcceptTokenUntilAll(SyntaxKind.QuestionMark, SyntaxKind.CloseAngle);
        }

        private void ParseOptionalBangEscape(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            if (IsBangEscape(lookahead: 0))
            {
                builder.Add(OutputTokensAsHtmlLiteral());

                // Accept the parser escape character '!'.
                Assert(SyntaxKind.Bang);
                AcceptTokenAndMoveNext();

                // Setup the metacode span that we will be outputing.
                SpanContext.ChunkGenerator = SpanChunkGenerator.Null;
                builder.Add(OutputAsMetaCode(OutputTokens()));
            }
        }

        private void SkipToEndScriptAndParseCode(in SyntaxListBuilder<RazorSyntaxNode> builder, AcceptedCharactersInternal endTagAcceptedCharacters = AcceptedCharactersInternal.Any)
        {
            // Special case for <script>: Skip to end of script tag and parse code
            var seenEndScript = false;

            while (!seenEndScript && !EndOfFile)
            {
                SkipToAndParseCode(builder, SyntaxKind.OpenAngle);
                var tagStart = CurrentStart;

                if (NextIs(SyntaxKind.ForwardSlash))
                {
                    var openAngle = CurrentToken;
                    NextToken(); // Skip over '<', current is '/'
                    var solidus = CurrentToken;
                    NextToken(); // Skip over '/', current should be text

                    if (At(SyntaxKind.Text) &&
                        string.Equals(CurrentToken.Content, ScriptTagName, StringComparison.OrdinalIgnoreCase))
                    {
                        seenEndScript = true;
                    }

                    // We put everything back because we just wanted to look ahead to see if the current end tag that we're parsing is
                    // the script tag.  If so we'll generate correct code to encompass it.
                    PutCurrentBack(); // Put back whatever was after the solidus
                    PutBack(solidus); // Put back '/'
                    PutBack(openAngle); // Put back '<'

                    // We just looked ahead, this NextToken will set CurrentToken to an open angle bracket.
                    NextToken();
                }

                if (seenEndScript)
                {
                    builder.Add(OutputTokensAsHtmlLiteral());

                    using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
                    {
                        var tagBuilder = pooledResult.Builder;
                        SpanContext.EditHandler.AcceptedCharacters = endTagAcceptedCharacters;

                        AcceptTokenAndMoveNext(); // '<'
                        AcceptTokenAndMoveNext(); // '/'
                        SkipToAndParseCode(tagBuilder, SyntaxKind.CloseAngle);
                        if (!OptionalToken(SyntaxKind.CloseAngle))
                        {
                            Context.ErrorSink.OnError(
                                RazorDiagnosticFactory.CreateParsing_UnfinishedTag(
                                    new SourceSpan(SourceLocationTracker.Advance(tagStart, "</"), ScriptTagName.Length),
                                    ScriptTagName));
                            var closeAngle = SyntaxFactory.MissingToken(SyntaxKind.CloseAngle);
                            AcceptToken(closeAngle);
                        }
                        tagBuilder.Add(OutputTokensAsHtmlLiteral());
                        builder.Add(SyntaxFactory.HtmlTagBlock(tagBuilder.ToList()));
                    }
                }
                else
                {
                    AcceptTokenAndMoveNext(); // Accept '<' (not the closing script tag's open angle)
                }
            }
        }

        private bool CurrentScriptTagExpectsHtml(in SyntaxListBuilder<RazorSyntaxNode> builder)
        {
            Debug.Assert(!builder.IsNull);

            HtmlAttributeBlockSyntax typeAttribute = null;
            for (var i = 0; i < builder.Count; i++)
            {
                var node = builder[i];
                if (node.IsToken || node.IsTrivia)
                {
                    continue;
                }
                
                if (node is HtmlAttributeBlockSyntax attributeBlock &&
                    attributeBlock.Children.Count >= 2 &&
                    IsTypeAttribute(attributeBlock))
                {
                    typeAttribute = attributeBlock;
                }
            }

            if (typeAttribute != null)
            {
                var contentValues = typeAttribute.Children.Nodes
                    .OfType<HtmlTextLiteralSyntax>()
                    .Select(textLiteral => textLiteral.ToFullString());

                var scriptType = string.Concat(contentValues).Trim();

                // Does not allow charset parameter (or any other parameters).
                return string.Equals(scriptType, "text/html", StringComparison.OrdinalIgnoreCase);
            }

            return false;
        }

        private static bool IsTypeAttribute(HtmlAttributeBlockSyntax attributeBlock)
        {
            if (!(attributeBlock.Children.Nodes.First() is HtmlTextLiteralSyntax attributeName))
            {
                return false;
            }

            var trimmedStartContent = attributeName.ToFullString().TrimStart();
            if (trimmedStartContent.StartsWith("type", StringComparison.OrdinalIgnoreCase) &&
                (trimmedStartContent.Length == 4 ||
                ValidAfterTypeAttributeNameCharacters.Contains(trimmedStartContent[4])))
            {
                return true;
            }

            return false;
        }

        protected SyntaxToken AcceptAllButLastDoubleHyphens()
        {
            var lastDoubleHyphen = CurrentToken;
            AcceptTokenWhile(s =>
            {
                if (NextIs(SyntaxKind.DoubleHyphen))
                {
                    lastDoubleHyphen = s;
                    return true;
                }

                return false;
            });

            NextToken();

            if (At(SyntaxKind.Text) && IsHyphen(CurrentToken))
            {
                // Doing this here to maintain the order of tokens
                if (!NextIs(SyntaxKind.CloseAngle))
                {
                    AcceptToken(lastDoubleHyphen);
                    lastDoubleHyphen = null;
                }

                AcceptTokenAndMoveNext();
            }

            return lastDoubleHyphen;
        }

        private bool AcceptTokenUntilAll(params SyntaxKind[] endSequence)
        {
            while (!EndOfFile)
            {
                SkipToAndParseCode(endSequence[0]);
                if (AcceptAllToken(endSequence))
                {
                    return true;
                }
            }
            Debug.Assert(EndOfFile);
            SpanContext.EditHandler.AcceptedCharacters = AcceptedCharactersInternal.Any;
            return false;
        }

        public HtmlMarkupBlockSyntax ParseBlock()
        {
            using (var pooledResult = Pool.Allocate<RazorSyntaxNode>())
            {
                // TODO
                var markupBlock = pooledResult.Builder.ToList();

                return SyntaxFactory.HtmlMarkupBlock(markupBlock);
            }
        }

        private void DefaultMarkupSpanContext(SpanContextBuilder spanContext)
        {
            spanContext.ChunkGenerator = new MarkupChunkGenerator();
            spanContext.EditHandler = new SpanEditHandler(Language.TokenizeString, AcceptedCharactersInternal.Any);
        }

        private void OtherParserBlock(in SyntaxListBuilder<RazorSyntaxNode> builder)
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
