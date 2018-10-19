// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using Microsoft.AspNetCore.Razor.Language.Extensions;
using Microsoft.AspNetCore.Razor.Language.Intermediate;
using Microsoft.AspNetCore.Razor.Language.Legacy;
using Microsoft.AspNetCore.Razor.Language.Syntax;

namespace Microsoft.AspNetCore.Razor.Language
{
#pragma warning disable CS0618 // Type or member is obsolete
    internal class DefaultRazorIntermediateNodeLoweringPhase : RazorEnginePhaseBase, IRazorIntermediateNodeLoweringPhase
    {
        private IRazorCodeGenerationOptionsFeature _optionsFeature;

        protected override void OnIntialized()
        {
            _optionsFeature = GetRequiredFeature<IRazorCodeGenerationOptionsFeature>();
        }

        protected override void ExecuteCore(RazorCodeDocument codeDocument)
        {
            var syntaxTree = codeDocument.GetSyntaxTree();
            ThrowForMissingDocumentDependency(syntaxTree);

            if (syntaxTree is LegacyRazorSyntaxTree)
            {
                // Can't handle legacy tree.
                return;
            }

            // This might not have been set if there are no tag helpers.
            var tagHelperContext = codeDocument.GetTagHelperContext();

            var document = new DocumentIntermediateNode();
            var builder = IntermediateNodeBuilder.Create(document);

            document.Options = codeDocument.GetCodeGenerationOptions() ?? _optionsFeature.GetOptions();

            IReadOnlyList<UsingReference> importedUsings = Array.Empty<UsingReference>();

            // The import documents should be inserted logically before the main document.
            var imports = codeDocument.GetImportSyntaxTrees();
            if (imports != null)
            {
                var importsVisitor = new ImportsVisitor(document, builder, syntaxTree.Options.FeatureFlags);

                for (var j = 0; j < imports.Count; j++)
                {
                    var import = imports[j];

                    importsVisitor.SourceDocument = import.Source;
                    importsVisitor.Visit(import.Root);
                }

                importedUsings = importsVisitor.Usings;
            }

            var tagHelperPrefix = tagHelperContext?.Prefix;
            var visitor = new MainSourceVisitor(document, builder, tagHelperPrefix, syntaxTree.Options.FeatureFlags)
            {
                SourceDocument = syntaxTree.Source,
            };

            visitor.Visit(syntaxTree.Root);

            // 1. Prioritize non-imported usings over imported ones.
            // 2. Don't import usings that already exist in primary document.
            // 3. Allow duplicate usings in primary document (C# warning).
            var usingReferences = new List<UsingReference>(visitor.Usings);
            for (var j = importedUsings.Count - 1; j >= 0; j--)
            {
                if (!usingReferences.Contains(importedUsings[j]))
                {
                    usingReferences.Insert(0, importedUsings[j]);
                }
            }

            // In each lowering piece above, namespaces were tracked. We render them here to ensure every
            // lowering action has a chance to add a source location to a namespace. Ultimately, closest wins.

            var i = 0;
            foreach (var reference in usingReferences)
            {
                var @using = new UsingDirectiveIntermediateNode()
                {
                    Content = reference.Namespace,
                    Source = reference.Source,
                };

                builder.Insert(i++, @using);
            }

            ImportDirectives(document);

            // The document should contain all errors that currently exist in the system. This involves
            // adding the errors from the primary and imported syntax trees.
            for (i = 0; i < syntaxTree.Diagnostics.Count; i++)
            {
                document.Diagnostics.Add(syntaxTree.Diagnostics[i]);
            }

            if (imports != null)
            {
                for (i = 0; i < imports.Count; i++)
                {
                    var import = imports[i];
                    for (var j = 0; j < import.Diagnostics.Count; j++)
                    {
                        document.Diagnostics.Add(import.Diagnostics[j]);
                    }
                }
            }

            codeDocument.SetDocumentIntermediateNode(document);
        }

        private void ImportDirectives(DocumentIntermediateNode document)
        {
            var visitor = new DirectiveVisitor();
            visitor.VisitDocument(document);

            var seenDirectives = new HashSet<DirectiveDescriptor>();
            for (var i = visitor.Directives.Count - 1; i >= 0; i--)
            {
                var reference = visitor.Directives[i];
                var directive = (DirectiveIntermediateNode)reference.Node;
                var descriptor = directive.Directive;
                var seenDirective = !seenDirectives.Add(descriptor);

                if (!directive.IsImported())
                {
                    continue;
                }

                switch (descriptor.Kind)
                {
                    case DirectiveKind.SingleLine:
                        if (seenDirective && descriptor.Usage == DirectiveUsage.FileScopedSinglyOccurring)
                        {
                            // This directive has been overridden, it should be removed from the document.

                            break;
                        }

                        continue;
                    case DirectiveKind.RazorBlock:
                    case DirectiveKind.CodeBlock:
                        if (descriptor.Usage == DirectiveUsage.FileScopedSinglyOccurring)
                        {
                            // A block directive cannot be imported.

                            document.Diagnostics.Add(
                                RazorDiagnosticFactory.CreateDirective_BlockDirectiveCannotBeImported(descriptor.Directive));
                        }
                        break;
                    default:
                        throw new InvalidOperationException(Resources.FormatUnexpectedDirectiveKind(typeof(DirectiveKind).FullName));
                }

                // Overridden and invalid imported directives make it to here. They should be removed from the document.

                reference.Remove();
            }
        }

        private struct UsingReference : IEquatable<UsingReference>
        {
            public UsingReference(string @namespace, SourceSpan? source)
            {
                Namespace = @namespace;
                Source = source;
            }
            public string Namespace { get; }

            public SourceSpan? Source { get; }

            public override bool Equals(object other)
            {
                if (other is UsingReference reference)
                {
                    return Equals(reference);
                }

                return false;
            }
            public bool Equals(UsingReference other)
            {
                return string.Equals(Namespace, other.Namespace, StringComparison.Ordinal);
            }

            public override int GetHashCode() => Namespace.GetHashCode();
        }

        private class LoweringVisitor : SyntaxRewriter
        {
            protected readonly IntermediateNodeBuilder _builder;
            protected readonly DocumentIntermediateNode _document;
            protected readonly List<UsingReference> _usings;
            protected readonly RazorParserFeatureFlags _featureFlags;

            public LoweringVisitor(DocumentIntermediateNode document, IntermediateNodeBuilder builder, RazorParserFeatureFlags featureFlags)
            {
                _document = document;
                _builder = builder;
                _usings = new List<UsingReference>();
                _featureFlags = featureFlags;
            }

            public IReadOnlyList<UsingReference> Usings => _usings;

            public RazorSourceDocument SourceDocument { get; set; }

            public override SyntaxNode VisitRazorDirective(RazorDirectiveSyntax node)
            {
                IntermediateNode directiveNode;
                var diagnostics = node.GetDiagnostics();
                var descriptor = node.DirectiveDescriptor;

                if (descriptor != null)
                {
                    // This is an extensible directive.
                    if (IsMalformed(diagnostics))
                    {
                        directiveNode = new MalformedDirectiveIntermediateNode()
                        {
                            DirectiveName = descriptor.Directive,
                            Directive = descriptor,
                            Source = BuildSourceSpanFromNode(node),
                        };
                    }
                    else
                    {
                        directiveNode = new DirectiveIntermediateNode()
                        {
                            DirectiveName = descriptor.Directive,
                            Directive = descriptor,
                            Source = BuildSourceSpanFromNode(node),
                        };
                    }

                    for (var i = 0; i < diagnostics.Length; i++)
                    {
                        directiveNode.Diagnostics.Add(diagnostics[i]);
                    }

                    _builder.Push(directiveNode);
                }

                var descendantNodes = node.DescendantNodes();
                foreach (var child in descendantNodes)
                {
                    if (child is CSharpStatementLiteralSyntax statementLiteral)
                    {
                        var context = statementLiteral.GetSpanContext();
                        if (context == null)
                        {
                            // We can't find a chunk generator.
                            continue;
                        }
                        else if (context.ChunkGenerator is DirectiveTokenChunkGenerator tokenChunkGenerator)
                        {
                            _builder.Add(new DirectiveTokenIntermediateNode()
                            {
                                Content = child.GetContent(),
                                DirectiveToken = tokenChunkGenerator.Descriptor,
                                Source = BuildSourceSpanFromNode(child),
                            });
                        }
                        else if (context.ChunkGenerator is AddImportChunkGenerator importChunkGenerator)
                        {
                            var namespaceImport = importChunkGenerator.Namespace.Trim();
                            var namespaceSpan = BuildSourceSpanFromNode(child);
                            _usings.Add(new UsingReference(namespaceImport, namespaceSpan));
                        }
                        else if (context.ChunkGenerator is AddTagHelperChunkGenerator addTagHelperChunkGenerator)
                        {
                            if (IsMalformed(addTagHelperChunkGenerator.Diagnostics))
                            {
                                directiveNode = new MalformedDirectiveIntermediateNode()
                                {
                                    DirectiveName = CSharpCodeParser.AddTagHelperDirectiveDescriptor.Directive,
                                    Directive = CSharpCodeParser.AddTagHelperDirectiveDescriptor,
                                    Source = BuildSourceSpanFromNode(child),
                                };
                            }
                            else
                            {
                                directiveNode = new DirectiveIntermediateNode()
                                {
                                    DirectiveName = CSharpCodeParser.AddTagHelperDirectiveDescriptor.Directive,
                                    Directive = CSharpCodeParser.AddTagHelperDirectiveDescriptor,
                                    Source = BuildSourceSpanFromNode(child),
                                };
                            }

                            for (var i = 0; i < addTagHelperChunkGenerator.Diagnostics.Count; i++)
                            {
                                directiveNode.Diagnostics.Add(addTagHelperChunkGenerator.Diagnostics[i]);
                            }

                            _builder.Push(directiveNode);

                            _builder.Add(new DirectiveTokenIntermediateNode()
                            {
                                Content = addTagHelperChunkGenerator.LookupText,
                                DirectiveToken = CSharpCodeParser.AddTagHelperDirectiveDescriptor.Tokens.First(),
                                Source = BuildSourceSpanFromNode(child),
                            });

                            _builder.Pop();
                        }
                        else if (context.ChunkGenerator is RemoveTagHelperChunkGenerator removeTagHelperChunkGenerator)
                        {
                            if (IsMalformed(removeTagHelperChunkGenerator.Diagnostics))
                            {
                                directiveNode = new MalformedDirectiveIntermediateNode()
                                {
                                    DirectiveName = CSharpCodeParser.RemoveTagHelperDirectiveDescriptor.Directive,
                                    Directive = CSharpCodeParser.RemoveTagHelperDirectiveDescriptor,
                                    Source = BuildSourceSpanFromNode(child),
                                };
                            }
                            else
                            {
                                directiveNode = new DirectiveIntermediateNode()
                                {
                                    DirectiveName = CSharpCodeParser.RemoveTagHelperDirectiveDescriptor.Directive,
                                    Directive = CSharpCodeParser.RemoveTagHelperDirectiveDescriptor,
                                    Source = BuildSourceSpanFromNode(child),
                                };
                            }

                            for (var i = 0; i < removeTagHelperChunkGenerator.Diagnostics.Count; i++)
                            {
                                directiveNode.Diagnostics.Add(removeTagHelperChunkGenerator.Diagnostics[i]);
                            }

                            _builder.Push(directiveNode);

                            _builder.Add(new DirectiveTokenIntermediateNode()
                            {
                                Content = removeTagHelperChunkGenerator.LookupText,
                                DirectiveToken = CSharpCodeParser.RemoveTagHelperDirectiveDescriptor.Tokens.First(),
                                Source = BuildSourceSpanFromNode(child),
                            });

                            _builder.Pop();
                        }
                        else if (context.ChunkGenerator is TagHelperPrefixDirectiveChunkGenerator tagHelperPrefixChunkGenerator)
                        {
                            if (IsMalformed(tagHelperPrefixChunkGenerator.Diagnostics))
                            {
                                directiveNode = new MalformedDirectiveIntermediateNode()
                                {
                                    DirectiveName = CSharpCodeParser.TagHelperPrefixDirectiveDescriptor.Directive,
                                    Directive = CSharpCodeParser.TagHelperPrefixDirectiveDescriptor,
                                    Source = BuildSourceSpanFromNode(child),
                                };
                            }
                            else
                            {
                                directiveNode = new DirectiveIntermediateNode()
                                {
                                    DirectiveName = CSharpCodeParser.TagHelperPrefixDirectiveDescriptor.Directive,
                                    Directive = CSharpCodeParser.TagHelperPrefixDirectiveDescriptor,
                                    Source = BuildSourceSpanFromNode(child),
                                };
                            }

                            for (var i = 0; i < tagHelperPrefixChunkGenerator.Diagnostics.Count; i++)
                            {
                                directiveNode.Diagnostics.Add(tagHelperPrefixChunkGenerator.Diagnostics[i]);
                            }

                            _builder.Push(directiveNode);

                            _builder.Add(new DirectiveTokenIntermediateNode()
                            {
                                Content = tagHelperPrefixChunkGenerator.Prefix,
                                DirectiveToken = CSharpCodeParser.TagHelperPrefixDirectiveDescriptor.Tokens.First(),
                                Source = BuildSourceSpanFromNode(child),
                            });

                            _builder.Pop();
                        }
                        else
                        {
                            Visit(statementLiteral);
                        }
                    }
                    else if (child is MarkupTextLiteralSyntax textLiteral)
                    {
                        Visit(textLiteral);
                    }
                }

                if (descriptor != null)
                {
                    _builder.Pop();
                }

                return node;
            }

            protected SourceSpan? BuildSourceSpanFromNode(SyntaxNode node)
            {
                if (node == null)
                {
                    return null;
                }

                if (node.Position == SourceDocument.Length)
                {
                    // This can happen when we have a marker symbol at the end of the syntax tree.
                    var location = SourceDocument.Lines.GetLocation(node.Position - 1);
                    return new SourceSpan(
                        SourceDocument.FilePath,
                        location.AbsoluteIndex + 1,
                        location.LineIndex,
                        location.CharacterIndex + 1,
                        length: 0);
                }

                return node.GetSourceSpan(SourceDocument);
            }
        }

        private class MainSourceVisitor : LoweringVisitor
        {
            private readonly string _tagHelperPrefix;

            public MainSourceVisitor(DocumentIntermediateNode document, IntermediateNodeBuilder builder, string tagHelperPrefix, RazorParserFeatureFlags featureFlags)
                : base(document, builder, featureFlags)
            {
                _tagHelperPrefix = tagHelperPrefix;
            }

            // Example
            // <input` checked="hello-world @false"`/>
            //  Name=checked
            //  Prefix= checked="
            //  Suffix="
            public override SyntaxNode VisitMarkupAttributeBlock(MarkupAttributeBlockSyntax node)
            {
                var prefixTokens = MergeLiterals(
                    node.NamePrefix?.LiteralTokens,
                    node.Name?.LiteralTokens,
                    node.NameSuffix?.LiteralTokens,
                    node.EqualsToken == null ? new SyntaxList<SyntaxToken>() : new SyntaxList<SyntaxToken>(node.EqualsToken),
                    node.ValuePrefix?.LiteralTokens);
                var prefix = SyntaxFactory.MarkupTextLiteral(prefixTokens);

                _builder.Push(new HtmlAttributeIntermediateNode()
                {
                    AttributeName = node.Name.GetContent(),
                    Prefix = prefix.GetContent(),
                    Suffix = node.ValueSuffix?.GetContent(),
                    Source = BuildSourceSpanFromNode(node),
                });

                Visit(node.Value);

                _builder.Pop();

                return node;
            }

            public override SyntaxNode VisitMarkupMinimizedAttributeBlock(MarkupMinimizedAttributeBlockSyntax node)
            {
                _builder.Push(new HtmlAttributeIntermediateNode()
                {
                    AttributeName = node.Name.GetContent(),
                    Prefix = null,
                    Suffix = null,
                    Source = BuildSourceSpanFromNode(node),
                });

                var result = base.VisitMarkupMinimizedAttributeBlock(node);

                _builder.Pop();

                return result;
            }

            // Example
            // <input checked="hello-world `@false`"/>
            //  Prefix= (space)
            //  Children will contain a token for @false.
            public override SyntaxNode VisitMarkupDynamicAttributeValue(MarkupDynamicAttributeValueSyntax node)
            {
                var containsExpression = false;
                var descendantNodes = node.DescendantNodes(n =>
                {
                    // Don't go into sub block. They may contain expressions but we only care about the top level.
                    return !(n.Parent is CSharpCodeBlockSyntax);
                });
                foreach (var child in descendantNodes)
                {
                    if (child is CSharpImplicitExpressionSyntax || child is CSharpExplicitExpressionSyntax)
                    {
                        containsExpression = true;
                    }
                }

                if (containsExpression)
                {
                    _builder.Push(new CSharpExpressionAttributeValueIntermediateNode()
                    {
                        Prefix = node.Prefix?.GetContent() ?? string.Empty,
                        Source = BuildSourceSpanFromNode(node),
                    });
                }
                else
                {
                    _builder.Push(new CSharpCodeAttributeValueIntermediateNode()
                    {
                        Prefix = node.Prefix?.GetContent() ?? string.Empty,
                        Source = BuildSourceSpanFromNode(node),
                    });
                }

                Visit(node.Value);

                _builder.Pop();

                return node;
            }

            public override SyntaxNode VisitMarkupLiteralAttributeValue(MarkupLiteralAttributeValueSyntax node)
            {
                _builder.Push(new HtmlAttributeValueIntermediateNode()
                {
                    Prefix = node.Prefix?.GetContent() ?? string.Empty,
                    Source = BuildSourceSpanFromNode(node),
                });

                _builder.Add(new IntermediateToken()
                {
                    Content = node.Value?.GetContent() ?? string.Empty,
                    Kind = TokenKind.Html,
                    Source = BuildSourceSpanFromNode(node.Value)
                });

                _builder.Pop();

                return node;
            }

            public override SyntaxNode VisitCSharpTemplateBlock(CSharpTemplateBlockSyntax node)
            {
                var templateNode = new TemplateIntermediateNode();
                _builder.Push(templateNode);

                var result = base.VisitCSharpTemplateBlock(node);

                _builder.Pop();

                if (templateNode.Children.Count > 0)
                {
                    var sourceRangeStart = templateNode
                        .Children
                        .FirstOrDefault(child => child.Source != null)
                        ?.Source;

                    if (sourceRangeStart != null)
                    {
                        var contentLength = templateNode.Children.Sum(child => child.Source?.Length ?? 0);

                        templateNode.Source = new SourceSpan(
                            sourceRangeStart.Value.FilePath ?? SourceDocument.FilePath,
                            sourceRangeStart.Value.AbsoluteIndex,
                            sourceRangeStart.Value.LineIndex,
                            sourceRangeStart.Value.CharacterIndex,
                            contentLength);
                    }
                }

                return result;
            }

            // CSharp expressions are broken up into blocks and spans because Razor allows Razor comments
            // inside an expression.
            // Ex:
            //      @DateTime.@*This is a comment*@Now
            //
            // We need to capture this in the IR so that we can give each piece the correct source mappings
            public override SyntaxNode VisitCSharpExplicitExpression(CSharpExplicitExpressionSyntax node)
            {
                if (_builder.Current is CSharpExpressionAttributeValueIntermediateNode)
                {
                    return base.VisitCSharpExplicitExpression(node);
                }

                var expressionNode = new CSharpExpressionIntermediateNode();

                _builder.Push(expressionNode);

                var result = base.VisitCSharpExplicitExpression(node);

                _builder.Pop();

                if (expressionNode.Children.Count > 0)
                {
                    var sourceRangeStart = expressionNode
                        .Children
                        .FirstOrDefault(child => child.Source != null)
                        ?.Source;

                    if (sourceRangeStart != null)
                    {
                        var contentLength = expressionNode.Children.Sum(child => child.Source?.Length ?? 0);

                        expressionNode.Source = new SourceSpan(
                            sourceRangeStart.Value.FilePath ?? SourceDocument.FilePath,
                            sourceRangeStart.Value.AbsoluteIndex,
                            sourceRangeStart.Value.LineIndex,
                            sourceRangeStart.Value.CharacterIndex,
                            contentLength);
                    }
                }

                return result;
            }

            public override SyntaxNode VisitCSharpImplicitExpression(CSharpImplicitExpressionSyntax node)
            {
                if (_builder.Current is CSharpExpressionAttributeValueIntermediateNode)
                {
                    return base.VisitCSharpImplicitExpression(node);
                }

                var expressionNode = new CSharpExpressionIntermediateNode();

                _builder.Push(expressionNode);

                var result = base.VisitCSharpImplicitExpression(node);

                _builder.Pop();

                if (expressionNode.Children.Count > 0)
                {
                    var sourceRangeStart = expressionNode
                        .Children
                        .FirstOrDefault(child => child.Source != null)
                        ?.Source;

                    if (sourceRangeStart != null)
                    {
                        var contentLength = expressionNode.Children.Sum(child => child.Source?.Length ?? 0);

                        expressionNode.Source = new SourceSpan(
                            sourceRangeStart.Value.FilePath ?? SourceDocument.FilePath,
                            sourceRangeStart.Value.AbsoluteIndex,
                            sourceRangeStart.Value.LineIndex,
                            sourceRangeStart.Value.CharacterIndex,
                            contentLength);
                    }
                }

                return result;
            }

            public override SyntaxNode VisitCSharpExpressionLiteral(CSharpExpressionLiteralSyntax node)
            {
                _builder.Add(new IntermediateToken()
                {
                    Content = node.GetContent(),
                    Kind = TokenKind.CSharp,
                    Source = BuildSourceSpanFromNode(node),
                });

                return base.VisitCSharpExpressionLiteral(node);
            }

            public override SyntaxNode VisitCSharpStatementLiteral(CSharpStatementLiteralSyntax node)
            {
                var isAttributeValue = _builder.Current is CSharpCodeAttributeValueIntermediateNode;

                if (!isAttributeValue)
                {
                    var statementNode = new CSharpCodeIntermediateNode()
                    {
                        Source = BuildSourceSpanFromNode(node)
                    };
                    _builder.Push(statementNode);
                }

                _builder.Add(new IntermediateToken()
                {
                    Content = node.GetContent(),
                    Kind = TokenKind.CSharp,
                    Source = BuildSourceSpanFromNode(node),
                });

                if (!isAttributeValue)
                {
                    _builder.Pop();
                }

                return base.VisitCSharpStatementLiteral(node);
            }

            public override SyntaxNode VisitMarkupTextLiteral(MarkupTextLiteralSyntax node)
            {
                var context = node.GetSpanContext();
                if (context != null && context.ChunkGenerator == SpanChunkGenerator.Null)
                {
                    return base.VisitMarkupTextLiteral(node);
                }

                if (node.LiteralTokens.Count == 1)
                {
                    var token = node.LiteralTokens[0];
                    if (token != null &&
                        token.Kind == SyntaxKind.Marker &&
                        token.Content.Length == 0)
                    {
                        // We don't want to create IR nodes for marker tokens.
                        return base.VisitMarkupTextLiteral(node);
                    }
                }

                var source = BuildSourceSpanFromNode(node);
                var currentChildren = _builder.Current.Children;
                if (currentChildren.Count > 0 && currentChildren[currentChildren.Count - 1] is HtmlContentIntermediateNode)
                {
                    var existingHtmlContent = (HtmlContentIntermediateNode)currentChildren[currentChildren.Count - 1];

                    if (existingHtmlContent.Source == null && source == null)
                    {
                        Combine(existingHtmlContent, node);
                        return base.VisitMarkupTextLiteral(node);
                    }

                    if (source != null &&
                        existingHtmlContent.Source != null &&
                        existingHtmlContent.Source.Value.FilePath == source.Value.FilePath &&
                        existingHtmlContent.Source.Value.AbsoluteIndex + existingHtmlContent.Source.Value.Length == source.Value.AbsoluteIndex)
                    {
                        Combine(existingHtmlContent, node);
                        return base.VisitMarkupTextLiteral(node);
                    }
                }

                var contentNode = new HtmlContentIntermediateNode()
                {
                    Source = source
                };
                _builder.Push(contentNode);

                _builder.Add(new IntermediateToken()
                {
                    Content = node.GetContent(),
                    Kind = TokenKind.Html,
                    Source = source,
                });

                _builder.Pop();

                return base.VisitMarkupTextLiteral(node);
            }

            public override SyntaxNode VisitMarkupTagHelperElement(MarkupTagHelperElementSyntax node)
            {
                var info = node.TagHelperInfo;
                var tagName = info.TagName;
                if (_tagHelperPrefix != null)
                {
                    tagName = tagName.Substring(_tagHelperPrefix.Length);
                }

                var tagHelperNode = new TagHelperIntermediateNode()
                {
                    TagName = tagName,
                    TagMode = info.TagMode,
                    Source = BuildSourceSpanFromNode(node)
                };

                foreach (var tagHelper in info.BindingResult.Descriptors)
                {
                    tagHelperNode.TagHelpers.Add(tagHelper);
                }

                _builder.Push(tagHelperNode);

                _builder.Push(new TagHelperBodyIntermediateNode());

                VisitList(node.Body);

                _builder.Pop(); // Pop InitializeTagHelperStructureIntermediateNode

                Visit(node.StartTag);

                _builder.Pop(); // Pop TagHelperIntermediateNode

                return node;
            }

            public override SyntaxNode VisitMarkupTagHelperAttribute(MarkupTagHelperAttributeSyntax node)
            {
                var element = node.FirstAncestorOrSelf<MarkupTagHelperElementSyntax>();
                var descriptors = element.TagHelperInfo.BindingResult.Descriptors;

                return base.VisitMarkupTagHelperAttribute(node);
            }

            private void Combine(HtmlContentIntermediateNode node, SyntaxNode item)
            {
                node.Children.Add(new IntermediateToken()
                {
                    Content = item.GetContent(),
                    Kind = TokenKind.Html,
                    Source = BuildSourceSpanFromNode(item),
                });

                if (node.Source != null)
                {
                    Debug.Assert(node.Source.Value.FilePath != null);

                    node.Source = new SourceSpan(
                        node.Source.Value.FilePath,
                        node.Source.Value.AbsoluteIndex,
                        node.Source.Value.LineIndex,
                        node.Source.Value.CharacterIndex,
                        node.Source.Value.Length + item.FullWidth);
                }
            }

            private SyntaxList<SyntaxToken> MergeLiterals(params SyntaxList<SyntaxToken>?[] literals)
            {
                var builder = SyntaxListBuilder<SyntaxToken>.Create();
                foreach (var literal in literals)
                {
                    if (!literal.HasValue)
                    {
                        continue;
                    }

                    builder.AddRange(literal.Value);
                }

                return builder.ToList();
            }
        }

        private class ImportsVisitor : LoweringVisitor
        {
            public ImportsVisitor(DocumentIntermediateNode document, IntermediateNodeBuilder builder, RazorParserFeatureFlags featureFlags)
                : base(document, new ImportBuilder(builder), featureFlags)
            {
            }

            private class ImportBuilder : IntermediateNodeBuilder
            {
                private readonly IntermediateNodeBuilder _innerBuilder;

                public ImportBuilder(IntermediateNodeBuilder innerBuilder)
                {
                    _innerBuilder = innerBuilder;
                }

                public override IntermediateNode Current => _innerBuilder.Current;

                public override void Add(IntermediateNode node)
                {
                    node.Annotations[CommonAnnotations.Imported] = CommonAnnotations.Imported;
                    _innerBuilder.Add(node);
                }

                public override IntermediateNode Build() => _innerBuilder.Build();

                public override void Insert(int index, IntermediateNode node)
                {
                    node.Annotations[CommonAnnotations.Imported] = CommonAnnotations.Imported;
                    _innerBuilder.Insert(index, node);
                }

                public override IntermediateNode Pop() => _innerBuilder.Pop();

                public override void Push(IntermediateNode node)
                {
                    node.Annotations[CommonAnnotations.Imported] = CommonAnnotations.Imported;
                    _innerBuilder.Push(node);
                }
            }
        }

        private class DirectiveVisitor : IntermediateNodeWalker
        {
            public List<IntermediateNodeReference> Directives = new List<IntermediateNodeReference>();

            public override void VisitDirective(DirectiveIntermediateNode node)
            {
                Directives.Add(new IntermediateNodeReference(Parent, node));

                base.VisitDirective(node);
            }
        }

        private static bool IsMalformed(IEnumerable<RazorDiagnostic> diagnostics)
            => diagnostics.Any(diagnostic => diagnostic.Severity == RazorDiagnosticSeverity.Error);
    }
#pragma warning restore CS0618 // Type or member is obsolete
}
