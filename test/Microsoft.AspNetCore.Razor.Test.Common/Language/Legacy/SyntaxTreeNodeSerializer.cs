// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.IO;
using Microsoft.AspNetCore.Razor.Language.Syntax;

namespace Microsoft.AspNetCore.Razor.Language.Legacy
{
    public static class SyntaxTreeNodeSerializer
    {
        internal static string Serialize(SyntaxTreeNode node)
        {
            using (var writer = new StringWriter())
            {
                var walker = new Walker(writer);
                walker.Visit(node);

                return writer.ToString();
            }
        }

        private class Walker : SyntaxTreeNodeWalker
        {
            private readonly SyntaxTreeNodeWriter _visitor;
            private readonly TextWriter _writer;

            public Walker(TextWriter writer)
            {
                _visitor = new SyntaxTreeNodeWriter(writer);
                _writer = writer;
            }

            public TextWriter Writer { get; }

            public override void Visit(SyntaxTreeNode node)
            {
                _visitor.Visit(node);
                _writer.WriteLine();

                if (node is Block block)
                {
                    _visitor.Depth++;
                    base.VisitDefault(block);
                    _visitor.Depth--;
                }
            }
        }
    }

    public static class SyntaxNodeSerializer
    {
        internal static string Serialize(SyntaxNode node)
        {
            using (var writer = new StringWriter())
            {
                var walker = new Walker(writer);
                walker.Visit(node);

                return writer.ToString();
            }
        }

        private class Walker : SyntaxNodeWalker
        {
            private readonly SyntaxNodeWriter _visitor;
            private readonly TextWriter _writer;

            public Walker(TextWriter writer)
            {
                _visitor = new SyntaxNodeWriter(writer);
                _writer = writer;
            }

            public TextWriter Writer { get; }

            public override SyntaxNode Visit(SyntaxNode node)
            {
                if (node.IsList)
                {
                    return base.DefaultVisit(node);
                }

                _visitor.Visit(node);
                _writer.WriteLine();

                if (!node.Green.IsToken && !node.Green.IsTrivia)
                {
                    _visitor.Depth++;
                    node = base.DefaultVisit(node);
                    _visitor.Depth--;
                }

                return node;
            }
        }
    }
}
