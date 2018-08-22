// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Collections.Generic;
using Microsoft.AspNetCore.Razor.Language.Syntax;

namespace Microsoft.AspNetCore.Razor.Language.Legacy
{
    internal class SyntaxTreeNodeWalker : ParserVisitor
    {
        private readonly List<SyntaxTreeNode> _ancestors = new List<SyntaxTreeNode>();

        protected IReadOnlyList<SyntaxTreeNode> Ancestors => _ancestors;

        protected SyntaxTreeNode Parent => _ancestors.Count > 0 ? _ancestors[0] : null;

        public override void VisitDefault(Block block)
        {
            var children = block.Children;
            if (block.Children.Count == 0)
            {
                return;
            }

            _ancestors.Insert(0, block);

            try
            {
                for (var i = 0; i < block.Children.Count; i++)
                {
                    var child = children[i];
                    Visit(child);
                }
            }
            finally
            {
                _ancestors.RemoveAt(0);
            }
        }
    }

    internal class SyntaxNodeWalker : SyntaxRewriter
    {
        private readonly List<SyntaxNode> _ancestors = new List<SyntaxNode>();

        protected IReadOnlyList<SyntaxNode> Ancestors => _ancestors;

        protected SyntaxNode Parent => _ancestors.Count > 0 ? _ancestors[0] : null;

        protected override SyntaxNode DefaultVisit(SyntaxNode node)
        {
            _ancestors.Insert(0, node);

            try
            {
                for (var i = 0; i < node.SlotCount; i++)
                {
                    var child = node.GetNodeSlot(i);
                    Visit(child);
                }
            }
            finally
            {
                _ancestors.RemoveAt(0);
            }

            return node;
        }
    }
}