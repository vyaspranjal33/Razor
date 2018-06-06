// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Razor.Language;
using Microsoft.CodeAnalysis.Text;

namespace Microsoft.CodeAnalysis.Razor.ProjectSystem
{
    // This will implement IDocumentServiceFactory and ISpanMapper, but those interfaces aren't
    // available right now.
    internal class GeneratedCodeContainer : SourceTextContainer
    {
        public override event EventHandler<TextChangeEventArgs> TextChanged;

        private readonly SourceText EmptySourceText = SourceText.From(string.Empty);

        public GeneratedCodeContainer()
        {
            Source = EmptySourceText;
            SourceVersion = VersionStamp.Default;
            Output = RazorCSharpDocument.Create("", RazorCodeGenerationOptions.CreateDefault(), Enumerable.Empty<RazorDiagnostic>());
            OutputSourceText = EmptySourceText;
        }

        public override SourceText CurrentText => Source;

        public SourceText Source { get; private set; }

        public VersionStamp SourceVersion { get; private set; }

        public RazorCSharpDocument Output { get; private set; }

        public SourceText OutputSourceText { get; private set; }

        public TService GetService<TService>()
        {
            if (this is TService service)
            {
                return service;
            }

            return default(TService);
        }

        public void SetOutput(SourceText source, VersionStamp sourceVersion, RazorCSharpDocument generated)
        {
            var old = OutputSourceText;

            Source = source;
            SourceVersion = sourceVersion;
            Output = generated;
            OutputSourceText = SourceText.From(Output.GeneratedCode);


            var e = new TextChangeEventArgs(old, OutputSourceText);
            TextChanged?.Invoke(this, e);
        }

        public Task<ImmutableArray<SpanMapResult>> MapSpansAsync(
                Document document,
                IEnumerable<TextSpan> spans,
                CancellationToken cancellationToken)
        {
            if (Output == null)
            {
                return Task.FromResult(ImmutableArray<SpanMapResult>.Empty);
            }

            var results = ImmutableArray.CreateBuilder<SpanMapResult>();
            foreach (var span in spans)
            {
                if (TryGetLinePositionSpan(span, out var linePositionSpan))
                {
                    results.Add(new SpanMapResult(document, linePositionSpan));
                }
            }

            return Task.FromResult(results.ToImmutable());
        }

        // This is temporary until we can reference this type from Roslyn
        public struct SpanMapResult
        {
            public SpanMapResult(Document document, LinePositionSpan linePositionSpan)
            {
                Document = document;
                LinePositionSpan = linePositionSpan;
            }

            public Document Document { get; }
            public LinePositionSpan LinePositionSpan { get; }
        }

        // Internal for testing.
        internal bool TryGetLinePositionSpan(TextSpan span, out LinePositionSpan linePositionSpan)
        {
            for (var i = 0; i < Output.SourceMappings.Count; i++)
            {
                var original = Output.SourceMappings[i].OriginalSpan.AsTextSpan();
                var generated = Output.SourceMappings[i].GeneratedSpan.AsTextSpan();

                var leftOffset = span.Start - generated.Start;
                var rightOffset = span.End - generated.End;
                if (leftOffset >= 0 && rightOffset <= 0)
                {
                    // This span mapping contains the span.
                    var adjusted = new TextSpan(original.Start + leftOffset, (original.End + rightOffset) - (original.Start + leftOffset));
                    linePositionSpan = Source.Lines.GetLinePositionSpan(adjusted);
                    return true;
                }
            }

            linePositionSpan = default;
            return false;
        }
    }
}
