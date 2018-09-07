// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Linq;

namespace Microsoft.AspNetCore.Razor.Language.Syntax
{
    internal static class SyntaxNodeExtensions
    {
        public static TNode WithAnnotations<TNode>(this TNode node, params SyntaxAnnotation[] annotations) where TNode : SyntaxNode
        {
            return (TNode)node.Green.SetAnnotations(annotations).CreateRed();
        }

        public static object GetAnnotationValue<TNode>(this TNode node, string key) where TNode : SyntaxNode
        {
            var annotation = node.GetAnnotations().FirstOrDefault(n => n.Kind == key);
            return annotation?.Data;
        }
    }
}
